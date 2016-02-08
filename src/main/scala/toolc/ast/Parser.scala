package toolc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._
import scala.collection.mutable.ListBuffer

object Parser extends Pipeline[Iterator[Token], Program] {
  def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._

    // Store the current token, as read from the lexer.
    var currentToken: Token = new Token(BAD)

    def readToken: Unit = {
      if (tokens.hasNext) {
        currentToken = tokens.next

        // skips bad tokens
        while (currentToken.kind == BAD && tokens.hasNext) {
          currentToken = tokens.next
        }
      }
    }

    // ''Eats'' the expected token, or terminates with an error.
    def eat(kind: TokenKind): Unit = {
      if (currentToken.kind == kind) {
        readToken
      }
      else {
        expected(kind)
      }
    }

    // Complains that what was found was not expected.
    def expected(kind: TokenKind, more: TokenKind*): Nothing = {
      fatal("expected: " + (kind :: more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)
    }

    def getId: Identifier = currentToken match {
      case t: ID =>
        val p = currentToken
        eat(IDKIND)
        new Identifier(t.value).setPos(p)
      case _ => expected(IDKIND)
    }

    def getType: TypeTree = {
      val p = currentToken
      val res = p.kind match {
        case INT =>
          readToken
          if (currentToken.kind == LBRACKET) {
            eat(LBRACKET)
            eat(RBRACKET)
            IntArrayType().setPos(p)
          }
          else {
            IntType().setPos(p)
          }

        case STRING =>
          readToken
          if (currentToken.kind == LBRACKET) {
            eat(LBRACKET)
            eat(RBRACKET)
            StringArrayType().setPos(p)
          }
          else {
            StringType().setPos(p)
          }

        case BOOLEAN =>
          readToken
          if (currentToken.kind == LBRACKET) {
            eat(LBRACKET)
            eat(RBRACKET)
            BoolArrayType().setPos(p)
          }
          else {
            BooleanType().setPos(p)
          }
        case IDKIND =>
          val id = getId
          if (currentToken.kind == LBRACKET) {
            eat(LBRACKET)
            eat(RBRACKET)
            ObjectArrayType(id).setPos(p)
          }
          else {
            id
          }

        case _ => expected(INT, STRING, BOOLEAN, IDKIND)
      }
      res
    }

    def getArg: Formal = {
      val p = currentToken
      val id = getId
      eat(COLON)

      val tpe = getType

      currentToken.kind match {
        case STAR =>
          // VarArg
          new Formal(tpe, id, isVarArg = true).setPos(p)
        case EQSIGN =>
          // Arg with default value
          eat(EQSIGN)
          val expr = parseExpr
          new Formal(tpe, id, isVarArg = false, Some(expr)).setPos(p)
        case _ =>
          new Formal(tpe, id).setPos(p)
      }
    }

    def getVar: VarDecl = {
      val p = currentToken
      eat(VAR)

      val id = getId
      eat(COLON)

      val tpe = getType

      if(currentToken.kind == EQSIGN){
        eat(EQSIGN)
        val expr = parseExpr
        eat(SEMICOLON)
        new VarDecl(tpe, id, Some(expr)).setPos(p)
      } else {
        eat(SEMICOLON)
        new VarDecl(tpe, id).setPos(p)
      }
    }

    def parseStat: StatTree = {

      val p = currentToken

      currentToken.kind match {
        case LBRACE =>
          eat(LBRACE)
          val stmtsBuffer = ListBuffer.empty[StatTree]
          while (currentToken.kind != RBRACE) { stmtsBuffer += parseStat }
          eat(RBRACE)
          new Block(stmtsBuffer.toList).setPos(p)

        case IF =>
          eat(IF)
          eat(LPAREN)
          val expr = parseExpr
          eat(RPAREN)
          val thn = parseStat
          val els: Option[StatTree] = if (currentToken.kind == ELSE) {
            eat(ELSE)
            Some(parseStat)
          }
          else {
            None
          }
          new If(expr, thn, els).setPos(p)

        case WHILE =>
          eat(WHILE)
          eat(LPAREN)
          val expr = parseExpr
          eat(RPAREN)
          val stat = parseStat
          new While(expr, stat).setPos(p)

        case PRINTLN =>
          eat(PRINTLN)
          eat(LPAREN)
          val expr = parseExpr
          eat(RPAREN)
          eat(SEMICOLON)
          new Println(expr).setPos(p)

        case IDKIND =>
          val id = getId

          if (currentToken.kind == LBRACKET) {
            eat(LBRACKET)
            val index = parseExpr
            eat(RBRACKET)

            eat(EQSIGN)
            val expr = parseExpr
            eat(SEMICOLON)
            new ArrayAssign(id, index, expr).setPos(p)
          }
          else {
            eat(EQSIGN)
            val expr = parseExpr
            eat(SEMICOLON)
            new Assign(id, expr).setPos(p)
          }

        case _ => expected(LBRACE, IF, WHILE, PRINTLN, IDKIND)
      }
    }

    def parseExpr: ExprTree = {
      var expr = parseExpr2
      while (currentToken.kind == OR) {
        val p = currentToken
        eat(OR)
        expr = new Or(expr, parseExpr2).setPos(p)
      }
      expr
    }

    def parseExpr2: ExprTree = {
      var expr = parseExpr3
      while (currentToken.kind == AND) {
        val p = currentToken
        eat(AND)
        expr = new And(expr, parseExpr3).setPos(p)
      }
      expr
    }

    def parseExpr3: ExprTree = {
      var expr = parseExpr4
      while (currentToken.kind == LESSTHAN || currentToken.kind == EQUALS) {
        val p = currentToken
        if (currentToken.kind == LESSTHAN) {
          eat(LESSTHAN)
          expr = new LessThan(expr, parseExpr4).setPos(p)
        }
        else {
          eat(EQUALS)
          expr = new Equals(expr, parseExpr4).setPos(p)
        }
      }
      expr
    }

    def parseExpr4: ExprTree = {
      var expr = parseExpr5
      while (currentToken.kind == PLUS || currentToken.kind == MINUS) {
        val p = currentToken
        if (currentToken.kind == PLUS) {
          eat(PLUS)
          expr = new Plus(expr, parseExpr5).setPos(p)
        }
        else {
          eat(MINUS)
          expr = new Minus(expr, parseExpr5).setPos(p)
        }
      }
      expr
    }

    def parseExpr5: ExprTree = {
      var expr = parseExpr6
      while (currentToken.kind == STAR || currentToken.kind == DIV) {
        val p = currentToken
        if (currentToken.kind == STAR) {
          eat(STAR)
          expr = new Times(expr, parseExpr6).setPos(p)
        }
        else {
          eat(DIV)
          expr = new Div(expr, parseExpr6).setPos(p)
        }
      }
      expr
    }

    def parseExpr6: ExprTree = {
      if (currentToken.kind == BANG) {
        val p = currentToken
        eat(BANG)
        new Not(parseExpr6).setPos(p)
      }
      else parseExpr7
    }

    def parseExpr7: ExprTree = {
      var expr = parseExpr8
      while (currentToken.kind == LBRACKET || currentToken.kind == DOT) {
        val p = currentToken
        if (currentToken.kind == LBRACKET) {
          eat(LBRACKET)
          expr = new ArrayRead(expr, parseExpr).setPos(p)
          eat(RBRACKET)
        }
        else {
          eat(DOT)
          if (currentToken.kind == LENGTH) {
            eat(LENGTH)
            expr = new ArrayLength(expr).setPos(p)
          }
          else {
            val meth = getId
            eat(LPAREN)

            val argsBuffer = ListBuffer.empty[ExprTree]
            while (!(currentToken.kind == RPAREN)) {
              if (currentToken.kind == COMMA) {
                eat(COMMA)
              }
              argsBuffer += parseExpr
            }

            eat(RPAREN)

            expr = new MethodCall(expr, meth, argsBuffer.toList).setPos(p)
          }
        }
      }
      expr
    }

    def parseExpr8: ExprTree = {
      val p = currentToken

      currentToken.kind match {

        case INTLITKIND =>
          currentToken match {
            case int: INTLIT =>
              val p = currentToken
              eat(INTLITKIND)
              new IntLit(int.value).setPos(p)
            case _ => expected(INTLITKIND)
          }

        case STRLITKIND =>
          currentToken match {
            case str: STRLIT =>
              val p = currentToken
              eat(STRLITKIND)
              new StringLit(str.value).setPos(p)
            case _ => expected(STRLITKIND)
          }

        case TRUE =>
          eat(TRUE)
          new True().setPos(p)

        case FALSE =>
          eat(FALSE)
          new False().setPos(p)

        case IDKIND =>
          getId

        case THIS =>
          eat(THIS)
          new This().setPos(p)

        case NEW =>
          eat(NEW)

          currentToken.kind match {
            case INT =>
              eat(INT)
              eat(LBRACKET)
              val size = parseExpr
              eat(RBRACKET)
              new NewIntArray(size).setPos(p)

            case BOOLEAN =>
              eat(BOOLEAN)
              eat(LBRACKET)
              val size = parseExpr
              eat(RBRACKET)
              new NewBoolArray(size).setPos(p)

            case STRING =>
              eat(STRING)
              eat(LBRACKET)
              val size = parseExpr
              eat(RBRACKET)
              new NewStringArray(size).setPos(p)

            case identifier =>
              val id = getId
              if (currentToken.kind == LBRACKET) {
                eat(LBRACKET)
                val size = parseExpr
                eat(RBRACKET)
                new NewObjectArray(size, id).setPos(p)
              }
              else {
                eat(LPAREN)
                eat(RPAREN)
                new New(id).setPos(p)
              }
          }

        case (LPAREN) =>
          eat(LPAREN)
          val expr = parseExpr
          eat(RPAREN)
          expr.setPos(p)

        case _ => expected(INTLITKIND, STRLITKIND, TRUE, FALSE, IDKIND, THIS, NEW)

      }
    }

    def parseMethod: MethodDecl = {
      val p = currentToken
      eat(DEF)
      val id = getId
      eat(LPAREN)

      val argsBuffer = ListBuffer.empty[Formal]
      if (!(currentToken.kind == RPAREN)) {
        argsBuffer += getArg
        if (currentToken.kind == STAR) {
          eat(STAR)
        }
        while (currentToken.kind == COMMA) {
          eat(COMMA)
          argsBuffer += getArg
          if (currentToken.kind == STAR) {
            eat(STAR)
          }
        }
      }

      eat(RPAREN)
      eat(COLON)

      val retType = getType

      eat(EQSIGN)
      eat(LBRACE)

      val varsBuffer = ListBuffer.empty[VarDecl]
      while (currentToken.kind == VAR) {
        varsBuffer += getVar
      }

      val statsBuffer = ListBuffer.empty[StatTree]
      while (currentToken.kind != RETURN) {
        statsBuffer += parseStat
      }

      eat(RETURN)

      val retExpr = parseExpr

      eat(SEMICOLON)
      eat(RBRACE)

      new MethodDecl(retType, id, argsBuffer.toList, varsBuffer.toList, statsBuffer.toList, retExpr).setPos(p)
    }

    def parseClass: ClassDecl = {
      val p = currentToken
      eat(CLASS)
      val id = getId

      val extendsId: Option[Identifier] = if (currentToken.kind == EXTENDS) {
        val p = currentToken
        eat(EXTENDS)
        currentToken match {
          case t: ID =>
            eat(IDKIND)
            Some(new Identifier(t.value).setPos(p))
          case other => expected(IDKIND)
        }
      }
      else {
        None
      }

      eat(LBRACE)

      val varsBuffer = ListBuffer.empty[VarDecl]
      while (currentToken.kind == VAR) {
        varsBuffer += getVar
      }

      val methsBuffer = ListBuffer.empty[MethodDecl]
      while (currentToken.kind == DEF) {
        methsBuffer += parseMethod
      }

      eat(RBRACE)

      new ClassDecl(id, extendsId, varsBuffer.toList, methsBuffer.toList).setPos(p)
    }

    def parseMain: MainObject = {
      val p = currentToken
      eat(OBJECT)
      val id = getId

      eat(LBRACE)
      eat(DEF)
      eat(MAIN)
      eat(LPAREN)
      eat(RPAREN)
      eat(COLON)
      eat(UNIT)
      eat(EQSIGN)

      eat(LBRACE)
      val stmtsBuffer = ListBuffer.empty[StatTree]
      while (currentToken.kind != RBRACE) { stmtsBuffer += parseStat }
      eat(RBRACE)
      eat(RBRACE)

      new MainObject(id, stmtsBuffer.toList).setPos(p)
    }

    def parseGoal: Program = {
      val main = parseMain
      val classesBuffer = ListBuffer.empty[ClassDecl]
      while (currentToken.kind != EOF) {
        classesBuffer += parseClass
      }
      eat(EOF)

      new Program(main, classesBuffer.toList).setPos(main)
    }

    // Initialize the first token
    readToken

    // Parse
    parseGoal
  }
}
