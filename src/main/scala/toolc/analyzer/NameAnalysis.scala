package toolc
package analyzer

import utils._
import ast.Trees._
import Symbols._
import scala.annotation.tailrec
import toolc.analyzer.Types._

object NameAnalysis extends Pipeline[Program, Program] {

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    def nameAnalysis(p: Program): GlobalScope = {
      // All class symbols are defined in the global scope.
      val globalScope = new GlobalScope()
      var declaredVarSymbols = Set.empty[VariableSymbol]
      var usedVarSymbols = Set.empty[VariableSymbol]

      @tailrec
      def checkNoCycles(c: ClassSymbol, children: Set[Int]): Unit = {
        // The inheritance graph has a cycle (eg. “class A extends B {} class B extends A {}”).
        if (children.contains(c.id)) fatal(s"The inheritance graph contains a cycle.", c)

        c.parent match {
          case Some(p) => checkNoCycles(p, children + c.id)
          case None => ()
        }
      }

      def collectClasses(p: Program): Unit = {
        val main = new ClassSymbol(p.main.id.value)
        globalScope.mainClass = main
        p.main.setSymbol(main).setPos(p.main)
        p.main.id.setSymbol(main).setPos(p.main.id)

        p.classes.foreach { x => collectClass(x) }

        p.classes.foreach { x => collectParent(x) }

        globalScope.classes.values.foreach {
          c => checkNoCycles(c, Set.empty[Int])
        }

        def collectClass(c: ClassDecl): Unit = {

          if (globalScope.classes.contains(c.id.value)) {
            error(s"Class ${c.id.value} is defined more than once.", c)
          }

          if (c.id.value == globalScope.mainClass.name) {
            error(s"Class ${c.id.value} has the same name as the main class.", c)
          }

          val symb = new ClassSymbol(c.id.value)

          c.setSymbol(symb).setPos(c)
          c.id.setSymbol(symb).setPos(c.id)

          globalScope.classes += (c.id.value -> symb)

        }

        def collectParent(c: ClassDecl): Unit = {
          c.parent match {
            case Some(p) => {
              globalScope.lookupClass(c.id.value) match {
                case Some(symb) => symb.parent = globalScope.lookupClass(p.value)
                case None =>
              }
            }
            case None =>
          }
        }

      }

      def collectVariablesAndMethods(p: Program): Unit = {
        var declaredVariables = Set.empty[VarDecl]
        var declaredMethods = Set.empty[MethodDecl]
        var currentClass = p.main.getSymbol
        var currentMethod: Option[MethodSymbol] = None
        p.classes.foreach {
          c =>
            currentClass = c.getSymbol
            currentMethod = None
            declaredVariables = Set.empty[VarDecl]
            declaredMethods = Set.empty[MethodDecl]
            collectVariableList(c.vars)
            collectMethodList(c.methods)
            c.methods.foreach {
              m =>
                currentMethod = Some(m.getSymbol)
                declaredVariables = Set.empty[VarDecl]
                collectArgs(m.args)
                collectVariableList(m.vars)
            }
        }

        p.classes.foreach { c =>
          checkNoVarOverriden(c.parent, c.vars)
        }

        // Check that a member of a class is not overriden in some children (shadowing not allowed in TOOL).
        def checkNoVarOverriden(o: Option[Identifier], lst: List[VarDecl]): Unit = {
          o match {
            case Some(par) =>
              globalScope.lookupClass(par.value) match {
                case Some(c) =>
                  lst.foreach { v =>
                    c.lookupVar(v.id.value) match {
                      case Some(o) => fatal("member declaration overrides previous declaration", c)
                      case None => // good
                    }
                  }
                case None => sys.error(s"The parent of ${par.value} was not found where it should have been.")
              }
            case None =>
          }
        }

        def collectArgs(args: List[Formal]): Unit = {
          var declaredArgs = Set.empty[Identifier]
          args.foreach {
            a =>
              if (declaredArgs.contains(a.id)) {
                error(s"Parameter name ${a.id.value} is used twice.", a.id)
              }
              declaredArgs += a.id
              val s = new VariableSymbol(a.id.value, a.defaultValue).setPos(a)
              a.setSymbol(s)
              a.id.setSymbol(s)
              a.tpe match {
                case id: Identifier => findClass(id)
                case _ =>
              }
              currentMethod match {
                case Some(m) => {
                  m.params += (a.id.value -> s)
                  m.argList = m.argList ::: List(s)
                }
                case None =>
              }
          }
        }

        @tailrec
        def collectVariableList(list: List[VarDecl]): Unit = {
          if (!list.isEmpty) {
            if (declaredVariables.filter(_.id.value == list.head.id.value).isEmpty) {
              declaredVariables += list.head
              val s = new VariableSymbol(list.head.id.value, list.head.defaultValue).setPos(list.head)
              list.head.setSymbol(s)
              list.head.id.setSymbol(s)
              list.head.tpe match {
                case id: Identifier => findClass(id)
                case _ =>
              }
              declaredVarSymbols += s

              currentMethod match {
                case Some(m) => {
                  m.members += (list.head.id.value -> s)
                  m.params.get(list.head.id.value) match {
                    case Some(v) => error("Argument is shadowed!", list.head.id)
                    case None =>
                  }
                }
                case None => currentClass.members += (list.head.id.value -> s)
              }
            }
            else {
              error("Variable " + list.head.id.value + " was declared multiple times.", list.head)
            }
            collectVariableList(list.tail)
          }
        }

        @tailrec
        def collectMethodList(list: List[MethodDecl]): Unit = {
          if (!list.isEmpty) {
            if (declaredMethods.filter(_.id.value == list.head.id.value).isEmpty) {
              declaredMethods += list.head

              val args = list.head.args
              val varargs = args.filter { x => x.isVarArg }
              if (!varargs.isEmpty) {
                if (!args.last.isVarArg || varargs.size > 1) {
                  error("A vararg (starred parameter) can only be used as the last formal argument of a function.", args.head)
                }
                if (!varargs.filter { f => f.defaultValue.isDefined }.isEmpty) {
                  error("A vararg (starred parameter) cannot have a default value.", args.head)
                }
              }

              val s = new MethodSymbol(list.head.id.value, currentClass, !varargs.isEmpty).setPos(list.head)
              list.head.setSymbol(s)
              list.head.id.setSymbol(s)
              currentClass.methods += (list.head.id.value -> s)
            }
            else {
              error("Method " + list.head.id.value + " was declared multiple times.", list.head)
            }

            collectMethodList(list.tail)
          }
        }
      }

      def collectInstances(p: Program): Unit = {
        var currentClass: Option[ClassSymbol] = None
        var currentMethod: Option[MethodSymbol] = None

        findInstances(p)

        // Check for unused variables.
        (declaredVarSymbols &~ usedVarSymbols).foreach { warning("Declared variable is never accessed", _) }

        @tailrec
        def findInstancesOnList(list: List[Tree]): Unit = {
          if (!list.isEmpty) {
            findInstances(list.head)
            findInstancesOnList(list.tail)
          }
        }

        @tailrec
        def findInstances(t: Tree): Unit = {
          t match {
            case Program(main, classes) => {
              findInstancesOnList(classes)
              findInstances(main)
            }

            case o: MainObject => {
              currentClass = Some(o.getSymbol)
              findInstancesOnList(o.stats)
            }

            case c: ClassDecl => {
              currentClass = Some(c.getSymbol)
              currentMethod = None
              findInstancesOnList(c.vars)
              findInstancesOnList(c.methods)
              c.parent match {
                case Some(p) => findClass(p)
                case None =>

              }
            }

            case m: MethodDecl => {
              currentMethod = Some(m.getSymbol)
              m.retType match {
                case id: Identifier => findClass(id)
                case _ =>
              }
              findInstancesOnList(m.vars)
              findInstancesOnList(m.stats)
              findInstances(m.retExpr)
            }
            
            case VarDecl(tpe, id, defaultValue) => {
              defaultValue match {
                case Some(v) => findInstances(v)
                case None =>
              }
            }

            case Block(stats) => findInstancesOnList(stats)

            case If(expr, thn, els) => {
              els match {
                case Some(s) => findInstancesOnList(List(expr, thn, s))
                case None => findInstancesOnList(List(expr, thn))
              }
            }

            case While(expr, stat) => findInstancesOnList(List(expr, stat))
            case Println(expr) => findInstances(expr)
            case Assign(id, expr) => findInstancesOnList(List(id, expr))
            case ArrayAssign(id, index, expr) => findInstancesOnList(List(id, index, expr))
            case And(lhs, rhs) => findInstancesOnList(List(lhs, rhs))
            case Or(lhs, rhs) => findInstancesOnList(List(lhs, rhs))
            case Plus(lhs, rhs) => findInstancesOnList(List(lhs, rhs))
            case Minus(lhs, rhs) => findInstancesOnList(List(lhs, rhs))
            case Div(lhs, rhs) => findInstancesOnList(List(lhs, rhs))
            case Times(lhs, rhs) => findInstancesOnList(List(lhs, rhs))
            case LessThan(lhs, rhs) => findInstancesOnList(List(lhs, rhs))
            case Equals(lhs, rhs) => findInstancesOnList(List(lhs, rhs))
            case ArrayRead(arr, index) => findInstancesOnList(List(arr, index))
            case ArrayLength(arr) => findInstances(arr)
            case MethodCall(obj, meth, args) => {
              findInstancesOnList(args)
              findInstances(obj)
            }

            // Only for variable identifiers.
            case id: Identifier => {
              currentMethod match {
                case Some(m) => m.lookupVar(id.value) match {
                  case Some(s) => {
                    id.setSymbol(s)
                    usedVarSymbols += s
                  }
                  case None => currentClass match {
                    case Some(c) => c.lookupVar(id.value) match {
                      case Some(s) => {
                        id.setSymbol(s)
                        usedVarSymbols += s
                      }
                      case None => error("Variable " + id.value + " is not declared!", id)
                    }
                    case None => sys.error("Should not happen.")
                  }
                }
                case None => currentClass match {
                  case Some(c) => c.lookupVar(id.value) match {
                    case Some(s) => id.setSymbol(s)
                    case None => error("Variable " + id.value + " is not declared!", id)
                  }
                  case None => sys.error("Should not happen.")
                }
              }
            }

            case Not(expr) => findInstances(expr)
            case New(tpe) => findClass(tpe)
            case NewIntArray(expr) => findInstances(expr)
            case NewBoolArray(expr) => findInstances(expr)
            case NewStringArray(expr) => findInstances(expr)
            case NewObjectArray(size, tpe) =>
              findClass(tpe)
              findInstances(size)

            // Check that 'this' is not referenced from the main object.
            case t: This => currentClass match {
              case Some(c) => {
                if (c.equals(globalScope.mainClass)) {
                  error("'this' referenced from main object!", t)
                }
                t.setSymbol(c)
              }
              case None =>
            }

            case _ =>
          }
        }
      }

      def findClass(id: Identifier): Unit = {
        globalScope.lookupClass(id.value) match {
          case Some(s) => id.setSymbol(s)
          case None => error("Class " + id.value + " is not declared!", id)
        }
      }

      collectClasses(p)
      collectVariablesAndMethods(p)
      collectInstances(p)

      globalScope
    }

    def collectTypes(p: Program, g: GlobalScope): Unit = {

      // Set type for the main object.
      val mainSymbol = p.main.getSymbol
      mainSymbol.setType(TObject(mainSymbol))

      // Double loop, is there a better way?
      g.classes.foreach { cl => cl._2.setType(TObject(cl._2)) }
      p.classes.foreach { collectTypesOfEverythingInClass(_) }

      g.classes.foreach(cl => checkMethOverload(cl._2))

      // Collect types of the class's vars, the class methods (rettype, args, vars).
      def collectTypesOfEverythingInClass(cd: ClassDecl): Unit = {
        cd.vars.foreach { v => v.getSymbol.setType(typeOf(v.tpe)) }
        cd.methods.foreach { m =>
          m.retType.setType(typeOf(m.retType))
          m.getSymbol.setType(m.retType.getType)
          m.vars.foreach { va =>
            va.tpe.setType(typeOf(va.tpe))
            va.getSymbol.setType(va.tpe.getType)
          }
          m.args.foreach { arg =>
            arg.tpe.setType(typeOf(arg.tpe, arg.isVarArg))
            arg.getSymbol.setType(typeOf(arg.tpe, arg.isVarArg))
          }
        }
      }

      def typeOf(tt: TypeTree, isVarArg: Boolean = false): Type = tt match {
        case IntArrayType() => TIntArray
        case BoolArrayType() => TBoolArray
        case StringArrayType() => TStringArray
        case ObjectArrayType(id) =>
          g.lookupClass(id.value) match {
            case Some(cs) => TObjectArray(cs)
            case _ =>
              error(s"Class $id.value is not declared.", id)
              TError
          }
        case IntType() => if (isVarArg) TIntArray else TInt
        case BooleanType() => if (isVarArg) TBoolArray else TBoolean
        case StringType() => if (isVarArg) TStringArray else TString

        case ident: Identifier =>
          val name = ident.value
          g.lookupClass(name) match {
            case Some(cs) => if (isVarArg) TObjectArray(cs) else cs.getType
            case None =>
              error(s"Class $name is not declared.", ident)
              TError
          }
      }

      // Check legality of overloading, set method as overridden when needed.
      def checkMethOverload(c: ClassSymbol): Unit = {
        c.methods.values.foreach { m =>
          c.parent match {
            case Some(p) =>
              p.lookupMethod(m.name) match {
                case Some(meth) =>
                  val methOneLength = meth.argList.length
                  val methTwoLength = m.argList.length

                  // Checking overloading.
                  if (methOneLength != methTwoLength) {
                    error(s"Method ${m.name} is overloaded: original method has $methOneLength arguments and "
                      + "you tried to use $methTwoLength in class ${c.name}.", m)
                  }

                  // Checking for type mismatch.
                  // The return type of a method and its overridden counterpart mismatch.
                  if (!meth.getType.toString.equals(m.getType.toString)) {
                    error(s"Method ${m.name}(${m.argList.length}) overrides parent method with a different return type (${m.getType} and ${meth.getType})", m)
                  }
                  val mVSMeth = meth.argList zip m.argList
                  mVSMeth.foreach {
                    elem =>
                      if (!elem._1.getType.toString.equals(elem._2.getType.toString)) {
                        error(s"Formal type in overriding method ${meth.name}(${m.argList.length}) does not match type in overridden method.", elem._2)
                      }
                  }

                  // Only attach overriding method to original method if no error occured.
                  if (!hasErrors) {
                    meth.overridden = Some(m)
                  }

                case None => // nothing, no method with the same name in some super class.
              }
            case None => // nothing, no parent.
          }
        }
      }
    }

    val globalScope = nameAnalysis(prog)
    collectTypes(prog, globalScope)

    prog
  }
}