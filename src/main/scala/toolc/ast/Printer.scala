package toolc
package ast

import Trees._

object Printer {
  def apply(t: Tree): String = {
    t match {
      case t: Program    => programToString(t)
      case t: MainObject => mainToString(t, false)
      case t: ClassDecl  => classToString(t)
      case t: StatTree   => statToString(t)
      case t: VarDecl    => varToString(t)
      case t: MethodDecl => methToString(t)
      case t: TypeTree   => typeToStringWithSymbol(t)
      case t: ExprTree   => exprToString(t)

      case _             => sys.error("Printer Error: operation not implemented for: " + t)
    }
  }

  var indent = 0

  /**
   * Note: We use the standard definition of tabs in sweng: 4 spaces.
   *
   * @return \n followed by as many tabs as </code>indent</code>'s value.
   */
  def crlf(): String = {
    val s = new StringBuilder()
    s.append("\n")

    var i = 0
    while (i < indent) {
      s.append("  ")
      i += 1
    }

    s.toString()
  }

  /**
   * @return The same string as <code>crlf()</code>, but increments <code>indent</code> by <code>increment</code> first.
   */
  def crlf(increment: Int): String = {
    indent += increment
    if (indent < 0) {
      sys.error("Printer Error: Tried to use negative indentation: " + indent)
    }
    crlf()
  }

  def programToString(t: Program): String = {
    val s = new StringBuilder()

    s.append(mainToString(t.main, !t.classes.isEmpty))

    val classes = t.classes map { x => classToString(x) }
    s.append(classes.mkString(crlf + crlf))

    s.toString()
  }

  /**
   * @param t: a <code>MainObject</code>
   * @param b: <code>true</code> iff there is something (classes) to print after the main object.
   *
   * @return a <code>String</code> representation of this <code>MainObject</code>.
   */
  def mainToString(t: MainObject, b: Boolean): String = {
    val s = new StringBuilder()

    s.append(s"object ${typeToStringWithSymbol(t.id)} {" + crlf(1))

    s.append(s"def main() : Unit = {" + crlf(1))

    val stats = t.stats map { x => statToString(x) }
    s.append(stats.mkString(crlf))

    s.append(crlf(-1) + "}")
    s.append(crlf(-1) + "}")

    if (b) s.append(crlf + crlf)

    s.toString()
  }

  def statToString(t: StatTree): String = {
    val s = new StringBuilder()

    t match {
      case t: Block =>
        val stats = t.stats map { x => statToString(x) }
        s.append(stats.mkString(crlf))
      case t: If =>
        s.append(s"if (${exprToString(t.expr)}) {${crlf(1)}")
        s.append(statToString(t.thn) + crlf(-1))
        s.append("}")

        t.els match {
          case Some(els) =>
            s.append(" else {" + crlf(1))
            s.append(statToString(els) + crlf(-1))
            s.append("}")
          case _ => ()
        }
      case t: While =>
        s.append(s"while (${exprToString(t.expr)}) {${crlf(1)}")
        s.append(statToString(t.stat) + crlf(-1))
        s.append("}")

      case t: Println =>
        s.append(s"println(${exprToString(t.expr)});")
      case t: Assign =>
        s.append(s"${exprToString(t.id)} = ${exprToString(t.expr)};")
      case t: ArrayAssign =>
        s.append(s"${exprToString(t.id)}[${exprToString(t.index)}] = ${exprToString(t.expr)};")
    }

    s.toString()
  }

  def classToString(t: ClassDecl): String = {
    val s = new StringBuilder()

    s.append(s"class ${typeToStringWithSymbol(t.id)} ")

    t.parent match {
      case Some(p) => s.append(s"extends ${typeToStringWithSymbol(p)} {")
      case _       => s.append("{")
    }

    s.append(crlf(1))

    s.append(varsToString(t.vars))

    val meths = t.methods map { x => methToString(x) }
    s.append(meths.mkString(crlf + crlf))

    s.append(crlf(-1) + '}')

    s.toString()
  }

  def varToString(v: VarDecl): String = {
    s"var ${typeToStringWithSymbol(v.id)}: ${typeToString(v.tpe)};"
  }

  def varsToString(l: List[VarDecl]): String = {
    val vars = l map { x => varToString(x) }
    if (l.isEmpty) {
      vars.mkString(crlf)
    }
    else {
      vars.mkString(crlf) + crlf + crlf
    }
  }

  def methToString(t: MethodDecl): String = {
    val s = new StringBuilder()

    s.append(s"def ${typeToStringWithSymbol(t.id)}(")
    val args = t.args.map { x => s"${typeToStringWithSymbol(x.id)}: ${typeToString(x.tpe)}" }
    s.append(s"${args.mkString(", ")}) : ${typeToString(t.retType)} = {${crlf(1)}")

    s.append(varsToString(t.vars))

    val stats = t.stats map { x => statToString(x) }
    s.append(stats.mkString(crlf))

    val prefix = if (stats.isEmpty) "" else crlf + crlf

    s.append(s"${prefix}return ${exprToString(t.retExpr)};${crlf(-1)}")
    s.append("}")

    s.toString()
  }

  def typeToString(t: TypeTree): String = t match {
    case IntArrayType() =>
      "Int[]"
    case IntType() =>
      "Int"
    case BooleanType() =>
      "Bool"
    case StringType() =>
      "String"
    case id: Identifier =>
      id.value
  }

  def typeToStringWithSymbol(t: TypeTree): String = t match {
    case id: Identifier =>
      val s = new StringBuilder()
      s.append(id.value)

      try {
        val ident = id.getSymbol.id
        s.append("#" + ident)
      }
      catch {
        case e: Exception => // nothing
      }

      s.toString()
    case other => typeToString(t)
  }

  def exprToString(t: ExprTree): String = {
    val s = new StringBuilder()

    t match {
      // The use of parentheses in places not needed (but allowed) helps the debugging of the parser.
      case t: And =>
        s.append(s"(${exprToString(t.lhs)} && ${exprToString(t.rhs)})")
      case t: Or =>
        s.append(s"(${exprToString(t.lhs)} || ${exprToString(t.rhs)})")
      case t: Plus =>
        s.append(s"(${exprToString(t.lhs)} + ${exprToString(t.rhs)})")
      case t: Minus =>
        s.append(s"(${exprToString(t.lhs)} - ${exprToString(t.rhs)})")
      case t: Times =>
        s.append(s"(${exprToString(t.lhs)} * ${exprToString(t.rhs)})")
      case t: Div =>
        s.append(s"(${exprToString(t.lhs)} / ${exprToString(t.rhs)})")
      case t: LessThan =>
        s.append(s"(${exprToString(t.lhs)} < ${exprToString(t.rhs)})")
      case t: Equals =>
        s.append(s"(${exprToString(t.lhs)} == ${exprToString(t.rhs)})")
      case t: ArrayRead =>
        s.append(s"${exprToString(t.arr)}[${exprToString(t.index)}]")
      case t: ArrayLength =>
        s.append(s"${exprToString(t.arr)}.length")
      case t: MethodCall =>
        s.append(s"${exprToString(t.obj)}.${typeToString(t.meth)}(")
        val args = t.args map { x => exprToString(x) }
        s.append(args.mkString(", ") + ")")
      case t: IntLit =>
        s.append(t.value)
      case t: StringLit =>
        val q = '"'
        s.append(s"$q${t.value}$q")

      case t: True =>
        s.append("true")
      case t: False =>
        s.append("false")
      case t: Identifier =>
        s.append(typeToStringWithSymbol(t))
      case t: This =>
        s.append("this")
      case t: NewIntArray =>
        s.append(s"new Int[${exprToString(t.size)}]")
      case t: New =>
        // There is only the default constructor in the TOOL language.
        s.append(s"new ${typeToStringWithSymbol(t.tpe)}()")
      case t: Not =>
        s.append(s"!(${exprToString(t.expr)})")
    }

    s.toString()
  }
}