package toolc
package lexer

import utils._
import scala.io.Source
import java.io.File
import java.util.NoSuchElementException
import toolc.ast.Trees.IntLit
import scala.annotation.tailrec

object Lexer extends Pipeline[File, Iterator[Token]] {
  import Tokens._

  def run(ctx: Context)(f: File): Iterator[Token] = {
    val source = Source.fromFile(f)
    import ctx.reporter._

    // Allows to readahead 1 char when needed.
    var buffered = false;
    var charBuffer = '$'

    val idsToKeywords = Map(
      "object" -> OBJECT,
      "class" -> CLASS,
      "def" -> DEF,
      "var" -> VAR,
      "Unit" -> UNIT,
      "main" -> MAIN,
      "String" -> STRING,
      "extends" -> EXTENDS,
      "Int" -> INT,
      "Bool" -> BOOLEAN,
      "while" -> WHILE,
      "if" -> IF,
      "else" -> ELSE,
      "return" -> RETURN,
      "length" -> LENGTH,
      "true" -> TRUE,
      "false" -> FALSE,
      "this" -> THIS,
      "new" -> NEW,
      "println" -> PRINTLN)

    /**
     * Consumes next char from the <code>source</code>.
     */
    def consumeNextChar(): Char = {
      if (buffered) {
        buffered = false;
        charBuffer
      }
      else { source.next }
    }

    /**
     * Get the next char from <code>source</code> without consuming it.
     */
    def peakNextChar(): Char = {
      if (buffered) {
        charBuffer
      }
      else {
        charBuffer = source.next;
        buffered = true;
        charBuffer
      }
    }

    def currentPos(): Positioned = {
      new Positioned {}.setPos(f, source.pos)
    }

    def lineComment(): Token = {
      val p = currentPos
      try {
        while (!source.next.equals('\n')) {}
      }
      catch {
        case e: NoSuchElementException => new Token(EOF).setPos(p)
      }
      readNextToken
    }

    def blockComment(): Token = {
      val p = currentPos
      try {
        if (consumeNextChar == '*' && peakNextChar == '/') {
          consumeNextChar
          readNextToken
        }
        else {
          blockComment
        }
      }
      catch {
        case e: NoSuchElementException =>
          error("Lexing error: Cannot end file with unclosed block comment.")
          new Token(BAD).setPos(p)
      }
    }

    def readID(s: String, p: Positioned): Token = {
      try {
        val c = peakNextChar
        if (!c.isLetter && !c.isDigit && c != '_') {
          idsToKeywords.get(s) match {
            case Some(keyword) => new Token(keyword).setPos(p)
            case _             => new ID(s).setPos(p)
          }
        }
        else {
          val str = s + consumeNextChar
          readID(str, p)
        }
      }
      catch {
        case e: NoSuchElementException =>
          error("Lexing error: EOF found when reading an ID.")
          new Token(BAD).setPos(p)
      }
    }

    def readIntLit(s: String, p: Positioned): Token = {
      try {
        if (peakNextChar.isDigit) {
          val str = s + consumeNextChar
          readIntLit(str, p)
        }
        else {
          return new INTLIT(s.toInt).setPos(p)
        }
      }
      catch {
        case e:
          NoSuchElementException =>
          error("Lexing error: EOF found when reading an IntLit.")
          new Token(BAD).setPos(p)
      }
    }

    def readStrLit(s: String, p: Positioned): Token = {
      try {
        val c = consumeNextChar
        if (c == '\n' || c == '\r') {
          error("Lexing error: new line found when reading a StringLit.")
          new Token(BAD).setPos(p)
        }
        else if (c == '"') {
          new STRLIT(s).setPos(p)
        }
        else {
          readStrLit(s + c, p)
        }
      }
      catch {
        case e: NoSuchElementException =>
          error("Lexing error: EOF found when reading a StringLit.")
          new Token(BAD).setPos(p)
      }
    }

    @tailrec
    def readNextToken(): Token = {
      val c = try {
        consumeNextChar
      }
      catch {
        case e: NoSuchElementException => return new Token(EOF).setPos(currentPos)
      }

      val p = currentPos

      c match {
        case ' ' | '\r' | '\n' | '\t' => readNextToken
        case '0'                      => new INTLIT(0).setPos(p) // <INTEGER_LITERAL> represents a sequence of digits, with no leading zeros
        case ':'                      => new Token(COLON).setPos(p)
        case ';'                      => new Token(SEMICOLON).setPos(p)
        case '.'                      => new Token(DOT).setPos(p)
        case ','                      => new Token(COMMA).setPos(p)
        case '=' =>
          if (peakNextChar == '=') {
            consumeNextChar
            new Token(EQUALS).setPos(p)
          }
          else {
            new Token(EQSIGN).setPos(p)
          }
        case '!' => new Token(BANG).setPos(p)
        case '(' => new Token(LPAREN).setPos(p)
        case ')' => new Token(RPAREN).setPos(p)
        case '[' => new Token(LBRACKET).setPos(p)
        case ']' => new Token(RBRACKET).setPos(p)
        case '{' => new Token(LBRACE).setPos(p)
        case '}' => new Token(RBRACE).setPos(p)
        case '&' =>
          if (peakNextChar == '&') {
            consumeNextChar
            new Token(AND).setPos(p)
          }
          else {
            error("Lexing error: Got a single '&', expected '&&'.", p)
            new Token(BAD).setPos(p)
          }
        case '|' =>
          if (peakNextChar == '|') {
            consumeNextChar
            new Token(OR).setPos(p)
          }
          else {
            error("Lexing error: Got a single '|', expected '||'.", p)
            new Token(BAD).setPos(p)
          }
        case '<' => new Token(LESSTHAN).setPos(p)
        case '+' => new Token(PLUS).setPos(p)
        case '-' => new Token(MINUS).setPos(p)
        case '*' => new Token(STAR).setPos(p)
        case '/' =>
          val next = peakNextChar
          if (next == '/') {
            consumeNextChar
            lineComment
          }
          else if (next == '*') {
            consumeNextChar
            blockComment
          }
          else {
            new Token(DIV).setPos(p)
          }

        case '"' => readStrLit("", p)

        case _ =>
          if (c.isLetter) {
            readID(c + "", p)
          }
          else if (c.isDigit) {
            readIntLit(c + "", p)
          }
          else {
            error("Lexing error: Character '" + c + "' unkown.", p)
            new Token(BAD).setPos(p)
          }
      }
    }

    new Iterator[Token] {
      var nextToken: Token = readNextToken
      var reachedEnd = false

      def hasNext = {
        nextToken.kind != EOF || !reachedEnd
      }

      def next = {
        val r = nextToken
        nextToken = readNextToken
        if (r.kind == EOF) {
          reachedEnd = true
        }
        r
      }
    }
  }
}