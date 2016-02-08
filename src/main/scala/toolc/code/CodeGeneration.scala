package toolc
package code

import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{ New => _, _ }
import ByteCodes._
import scala.collection.mutable.HashMap
import utils._

object CodeGeneration extends Pipeline[Program, Unit] {

  def run(ctx: Context)(prog: Program): Unit = {
    import ctx.reporter._

    var argToIndex = new HashMap[String, Int]
    var varToIndex = new HashMap[String, Int]
    var initializedVars = Set.empty[VariableSymbol]

    def getFieldType(tpe: Type): String = {
      tpe match {
        case TInt => "I"
        case TBoolean => "Z"
        case TIntArray => "[I"
        case TBoolArray => "[Z"
        case TStringArray => "[Ljava/lang/String;"
        case TObjectArray(c) => "[L" + c.name + ";"
        case TString => "Ljava/lang/String;"
        case TObject(c) => "L" + c.name + ";"
        case _ => sys.error("Code generation error: can't handle type " + tpe)
      }
    }

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {
      initializedVars = Set.empty[VariableSymbol]
      
      val parent = ct.parent match {
        case Some(p) => Some(p.value)
        case None => None
      }

      val classFile = new ClassFile(ct.id.value, parent)

      classFile.setSourceFile(sourceName)

      classFile.addDefaultConstructor

      ct.vars.foreach { v =>
        classFile.addField(getFieldType(v.getSymbol.getType), v.id.value)
      }

      ct.methods.foreach { meth =>
        val args: String = typeListToJVM(meth.args.map { arg =>
          arg.id.getType
        })
        val mh: MethodHandler = classFile.addMethod(getFieldType(meth.retType.getType), meth.id.value, args)
        generateMethodCode(mh.codeHandler, meth, ct.id.value)
      }

      classFile.writeToFile(getClassFileAdress(dir, ct.id))

    }

    def getClassFileAdress(dir: String, id: Identifier): String = {
      val dirPath = dir match {
        case "" => "./"
        case _ => dir + "/"
      }

      dirPath + id.value + ".class"
    }

    def typeListToJVM(lst: List[Type]): String = {
      val res = new StringBuilder
      lst.foreach { tpe => res.append(getFieldType(tpe)) }
      res.toString
    }

    def generateStatTreeCode(ch: CodeHandler, st: StatTree, cname: String): Unit = st match {
      case Block(stats) => stats.foreach { stat => generateStatTreeCode(ch, stat, cname) }

      case If(expr, thn, els) =>
        val ifElse = ch.getFreshLabel("else")
        val after = ch.getFreshLabel("then")

        generateExprTreeCode(ch, expr, cname)
        ch << IfEq(ifElse)
        generateStatTreeCode(ch, thn, cname)
        ch << Goto(after)
        ch << Label(ifElse)
        els match {
          case Some(els) => generateStatTreeCode(ch, els, cname)
          case None =>
        }
        ch << Label(after)

      case While(expr, stat) =>
        val loop = ch.getFreshLabel("loop")
        val break = ch.getFreshLabel("break")
        ch << Label(loop)
        generateExprTreeCode(ch, expr, cname)
        ch << IfEq(break)
        generateStatTreeCode(ch, stat, cname)
        ch << Goto(loop)
        ch << Label(break)

      case Println(expr) =>
        ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
        generateExprTreeCode(ch, expr, cname)
        ch << InvokeVirtual("java/io/PrintStream", "println", "(" + getFieldType(expr.getType) + ")V")

      case Assign(id, expr) =>
        id.getSymbol match {
          case s: VariableSymbol => initializedVars += s
          case _ => //Should not happen
        }
        varToIndex.get(id.value) match {
          case Some(v) =>
            generateExprTreeCode(ch, expr, cname)
            id.getType match {
              case TInt | TBoolean => ch << IStore(v)
              case TString | TIntArray | TBoolArray | TStringArray => ch << AStore(v)
              case TObjectArray(cs) => ch << AStore(v)
              case TObject(o) => ch << AStore(v)
              case _ => sys.error("Should not happen")
            }
          case _ =>
            argToIndex.get(id.value) match {
              case Some(v) =>
                generateExprTreeCode(ch, expr, cname)
                id.getType match {
                  case TInt | TBoolean => ch << IStore(v)
                  case TIntArray | TBoolArray | TStringArray | TString => ch << AStore(v)
                  case TObject(o) => ch << AStore(v)
                  case _ => sys.error("Should not happen")
                }
              case _ =>
                ch << ALoad(0)
                generateExprTreeCode(ch, expr, cname)
                ch << PutField(cname, id.value, getFieldType(id.getType))
            }
        }

      case ArrayAssign(id, index, expr) =>
        generateExprTreeCode(ch, id, cname)
        generateExprTreeCode(ch, index, cname)
        generateExprTreeCode(ch, expr, cname)
        id.getType match {
          case TIntArray => ch << IASTORE
          case TBoolArray => ch << BASTORE
          case _ => ch << AASTORE
        }
    }

    /**
     * Note: we apply lazy evaluation, such that (true || (1/0 == 1)) doesn't crash.
     */
    def generateExprTreeCode(ch: CodeHandler, et: ExprTree, cname: String): Unit = et match {
      case And(lhs, rhs) =>
        // IfEq(label): Jump to label if top of stack is zero,
        // consumes the stack elem.
        val done = ch.getFreshLabel("done")
        val lhsFalse = ch.getFreshLabel("lhsFalse")

        generateExprTreeCode(ch, lhs, cname)
        ch << IfEq(lhsFalse)
        generateExprTreeCode(ch, rhs, cname)
        ch << IfEq(lhsFalse) << Ldc(1) << Goto(done)
        ch << Label(lhsFalse) << Ldc(0) << Label(done)

      case Or(lhs, rhs) =>
        val lhsFalse = ch.getFreshLabel("lhsFalse")
        val done = ch.getFreshLabel("done")

        generateExprTreeCode(ch, lhs, cname)
        ch << IfEq(lhsFalse) << Ldc(1) << Goto(done)
        ch << Label(lhsFalse)
        generateExprTreeCode(ch, rhs, cname)
        ch << Label(done)

      case Plus(lhs, rhs) =>
        et.getType match {
          case TInt =>
            generateExprTreeCode(ch, lhs, cname)
            generateExprTreeCode(ch, rhs, cname)
            ch << IADD
          case TString =>
            ch << DefaultNew("java/lang/StringBuilder")
            generateExprTreeCode(ch, lhs, cname)
            ch << InvokeVirtual("java/lang/StringBuilder", "append", "(" + getFieldType(lhs.getType) + ")Ljava/lang/StringBuilder;")
            generateExprTreeCode(ch, rhs, cname)
            ch << InvokeVirtual("java/lang/StringBuilder", "append", "(" + getFieldType(rhs.getType) + ")Ljava/lang/StringBuilder;")
            ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")

          case _ => sys.error("unexpected types for add")
        }

      case Minus(lhs, rhs) =>
        generateExprTreeCode(ch, lhs, cname)
        generateExprTreeCode(ch, rhs, cname)
        ch << ISUB

      case Times(lhs, rhs) =>
        generateExprTreeCode(ch, lhs, cname)
        generateExprTreeCode(ch, rhs, cname)
        ch << IMUL

      case Div(lhs, rhs) =>
        generateExprTreeCode(ch, lhs, cname)
        generateExprTreeCode(ch, rhs, cname)
        ch << IDIV

      case LessThan(lhs, rhs) =>
        generateExprTreeCode(ch, lhs, cname)
        generateExprTreeCode(ch, rhs, cname)
        val lessThan = ch.getFreshLabel("lessthan")
        val lessThanEnd = ch.getFreshLabel("lessthanend")
        ch << ISUB << IfLt(lessThan) << Ldc(0) << Goto(lessThanEnd) << Label(lessThan) << Ldc(1) << Label(lessThanEnd)

      case Equals(lhs, rhs) =>
        generateExprTreeCode(ch, lhs, cname)
        generateExprTreeCode(ch, rhs, cname)
        val equals = ch.getFreshLabel("equals")
        val equalsEnd = ch.getFreshLabel("equalsend")
        lhs.getType match {
          case TInt | TBoolean =>
            ch << ISUB << IfEq(equals) << Ldc(0) << Goto(equalsEnd) << Label(equals) << Ldc(1) << Label(equalsEnd)
          case TString | TIntArray | TBoolArray | TStringArray =>
            ch << If_ACmpEq(equals) << Ldc(0) << Goto(equalsEnd) << Label(equals) << Ldc(1) << Label(equalsEnd)
          case o: TObject =>
            ch << If_ACmpEq(equals) << Ldc(0) << Goto(equalsEnd) << Label(equals) << Ldc(1) << Label(equalsEnd)
          case _ => sys.error("This should not happen.")
        }

      case ArrayRead(arr, index) =>
        generateExprTreeCode(ch, arr, cname)
        generateExprTreeCode(ch, index, cname)

        arr.getType match {
          case TIntArray => ch << IALOAD
          case TBoolArray => ch << BALOAD
          case _ => ch << AALOAD // TString, other user created objects.
        }

      case ArrayLength(arr) =>
        generateExprTreeCode(ch, arr, cname)
        ch << ARRAYLENGTH

      case MethodCall(obj, meth, args) =>
        var types: String = ""
        var normalArgs = List.empty[ExprTree]
        var varArgs = List.empty[ExprTree]
        var ms: MethodSymbol = null

        val className = obj.getType match {
          case TObject(c) => c.lookupMethod(meth.value) match {
            case Some(s) =>
              ms = s
              types = s.argList.map(a => getFieldType(a.getType)).mkString("")
              if (s.hasVarArg) {
                normalArgs = args.take(s.argList.size - 1)
                varArgs = args.takeRight(args.size - normalArgs.size)
              }
              else {
                normalArgs = args
              }
              s.classSymbol.name
            case None => sys.error("Method not found")
          }
          case _ => sys.error("Method should only be called on object")
        }

        generateExprTreeCode(ch, obj, cname)
        normalArgs foreach {
          arg =>
            generateExprTreeCode(ch, arg, cname)
        }

        // Put default values on stack
        if (args.size < ms.argList.size) {
          val defaultArgs = ms.argList.takeRight(ms.argList.size - args.size)
          defaultArgs.foreach {
            arg =>
              arg.defaultValue match {
                case Some(expr) => generateExprTreeCode(ch, expr, cname)
                case _ => sys.error(s"Expected to find a default value for arg ${arg.name}.")
              }
          }
        }

        if (!varArgs.isEmpty) {
          // Create array
          args.last.getType match {
            case TInt => generateExprTreeCode(ch, NewIntArray(IntLit(varArgs.size)), cname)
            case TBoolean => generateExprTreeCode(ch, NewBoolArray(IntLit(varArgs.size)), cname)
            case TString => generateExprTreeCode(ch, NewStringArray(IntLit(varArgs.size)), cname)
            case TObject(c) =>
              val id = new Identifier(c.name).setSymbol(c)
              generateExprTreeCode(ch, NewObjectArray(IntLit(varArgs.size), id), cname)
            case _ => sys.error("Unsupported vararg type")
          }

          val arrayRef = ch.getFreshVar
          ch << AStore(arrayRef)

          var i = 0
          varArgs foreach {
            arg =>
              ch << ALoad(arrayRef) << Ldc(i)
              generateExprTreeCode(ch, arg, cname)
              arg.getType match {
                case TInt => ch << IASTORE
                case TBoolean => ch << BASTORE
                case _ => ch << AASTORE
              }
              i += 1
          }

          ch << ALoad(arrayRef)
        }

        ch << InvokeVirtual(className, meth.value, "(" + types + ")" + getFieldType(et.getType))

      case IntLit(value) =>
        ch << Ldc(value)

      case StringLit(value) =>
        ch << Ldc(value)

      case True() =>
        ch << Ldc(1)

      case False() =>
        ch << Ldc(0)

      case id:Identifier => {
        var isInitialized = id.getSymbol match {
          case s: VariableSymbol => initializedVars.contains(s)
          case _ => sys.error("Should not happen.")
        }
        if(isInitialized){
          varToIndex.get(id.value) match {
            case Some(v) => et.getType match {
              case TInt | TBoolean => ch << ILoad(v)
              case TString | TIntArray | TBoolArray | TStringArray => ch << ALoad(v)
              case TObjectArray(cs) => ch << ALoad(v)
              case TObject(o) => ch << ALoad(v)
              case _ => sys.error("Should not happen.")
            }
            case _ =>
              argToIndex.get(id.value) match {
                case Some(v) => ch << ArgLoad(v)
                case _ => ch << ALoad(0) << GetField(cname, id.value, getFieldType(et.getType))
              }
          }
        } else {
          id.getSymbol match {
            case s: VariableSymbol => s.defaultValue match {
              case Some(v) => generateExprTreeCode(ch, v, cname)
              case None => error("Uninitialized variable " + id.value, id)
            }
            case _ => sys.error("Should not happen.")
          }
        }
      }

      case This() =>
        ch << ALoad(0)

      case NewIntArray(size) =>
        // http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.9.1-120-O
        // The atype operand of each newarray instruction must take one of the values [...] T_INT (10) [...].
        generateExprTreeCode(ch, size, cname)
        ch << NewArray(10)

      case NewBoolArray(size) =>
        generateExprTreeCode(ch, size, cname)
        ch << NewArray(4)

      case NewStringArray(size) =>
        generateExprTreeCode(ch, size, cname)
        ch << NewArray("java/lang/String")

      case NewObjectArray(size, id) =>
        generateExprTreeCode(ch, size, cname)
        ch << NewArray(id.value)

      case New(tpe) =>
        ch << DefaultNew(tpe.value)

      case Not(expr) =>
        generateExprTreeCode(ch, expr, cname)
        ch << Ldc(1) << IXOR
    }

    // A mapping from variable symbols to positions in the local variables
    // of the stack frame.
    def generateMethodCode(ch: CodeHandler, mt: MethodDecl, cname: String): Unit = {
      argToIndex = new HashMap[String, Int]
      varToIndex = new HashMap[String, Int]
      initializedVars = initializedVars ++ mt.getSymbol.argList

      var argIndex = 1
      mt.args foreach {
        arg =>
          argToIndex += (arg.id.value -> argIndex)
          argIndex += 1
      }
      
      mt.vars foreach {
        v =>
          val varIndex = ch.getFreshVar
          varToIndex += (v.id.value -> varIndex)
      }

      mt.stats.foreach { stat => generateStatTreeCode(ch, stat, cname) }

      generateExprTreeCode(ch, mt.retExpr, cname)

      mt.retType.getType match {
        case TInt | TBoolean => ch << IRETURN
        case TIntArray | TBoolArray | TStringArray | TString => ch << ARETURN
        case TObject(cd) => ch << ARETURN
        case TUntyped => ch << RETURN
        case _ => sys.error("CodeGen Error: got " + mt.retType.getType)
      }

      // ch.print
      ch.freeze
    }

    def generateMainMethodCode(ch: CodeHandler, stmts: List[StatTree], cname: String): Unit = {

      /**
       * The main object is of the form
       *
       * object <ObjectName> {
       * def main() : Unit = {
       *  <statsList>
       * }
       * }
       */

      stmts.foreach { stat => generateStatTreeCode(ch, stat, cname) }

      ch << RETURN

      ch.freeze
    }

    def generateMainClassFile(ct: MainObject, dir: String): Unit = {

      val sourceName = ct.id.value
      val mainClassFile = new ClassFile(sourceName, None)

      mainClassFile.setSourceFile(sourceName)

      mainClassFile.addDefaultConstructor

      val mh: MethodHandler = mainClassFile.addMainMethod
      generateMainMethodCode(mh.codeHandler, ct.stats, sourceName)

      mainClassFile.writeToFile(getClassFileAdress(dir, ct.id))
    }

    val outDir = ctx.outDir.map(_.getPath + "/").getOrElse("./")

    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }

    val sourceName = ctx.file.getName

    // output code
    prog.classes foreach {
      ct => generateClassFile(sourceName, ct, outDir)
    }

    generateMainClassFile(prog.main, outDir)
  }

}
