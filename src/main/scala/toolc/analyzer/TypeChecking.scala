package toolc
package analyzer

import ast.Trees._
import Symbols._
import Types._
import utils._

object TypeChecking extends Pipeline[Program, Program] {

  /**
   * Typechecking does not produce a new value, but has the side effect of
   * attaching types to trees and potentially outputting error messages.
   */
  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    def tcExpr(expr: ExprTree, expected: Type*): Type = {
      val tpe: Type = expr match {
        case And(lhs, rhs) =>
          tcExpr(lhs, TBoolean)
          tcExpr(rhs, TBoolean)
          TBoolean
        case Or(lhs, rhs) =>
          tcExpr(lhs, TBoolean)
          tcExpr(rhs, TBoolean)
          TBoolean
        case Plus(lhs, rhs) =>
          (tcExpr(lhs, TInt, TString), tcExpr(rhs, TInt, TString)) match {
            case (TInt, TInt) => TInt
            case (TInt, TString) => TString
            case (TString, TInt) => TString
            case (TString, TString) => TString
            case _ =>
              // Should never happen.
              TError
          }
        case Minus(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
          TInt
        case Times(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
          TInt
        case Div(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
          TInt
        case LessThan(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
          TBoolean
        case Equals(lhs, rhs) =>
          tcExpr(lhs) match {
            case TObject(c) => tcExpr(rhs, TAnyObject)
            case TInt => tcExpr(rhs, TInt)
            case TString => tcExpr(rhs, TString)
            case TIntArray => tcExpr(rhs, TIntArray)
            case TBoolArray => tcExpr(rhs, TBoolArray)
            case TStringArray => tcExpr(rhs, TStringArray)
            case TAnyObjectArray => tcExpr(rhs, TAnyObjectArray)
            case TBoolean => tcExpr(rhs, TBoolean)
            case _ => TError
          }
          TBoolean

        case ArrayRead(arr, index) =>
          tcExpr(index, TInt)
          tcExpr(arr, TIntArray, TBoolArray, TStringArray, TAnyObjectArray)
          arr.getType match {
            case TIntArray => TInt
            case TStringArray => TString
            case TBoolArray => TBoolean
            case TObjectArray(c) => c.getType
            case _ => TError
          }
        case ArrayLength(arr) =>
          tcExpr(arr, TIntArray, TBoolArray, TStringArray, TAnyObjectArray)
          TInt
        case MethodCall(obj, meth, args) =>
          args.foreach { tcExpr(_) }
          tcExpr(obj, TAnyObject) match {
            case TObject(c) => c.lookupMethod(meth.value) match {
              case Some(m) =>
                if (args.length > m.argList.length && !m.hasVarArg) error("Too many arguments for method " + meth.value, expr)
                if (m.argList.length > 0) {
                  for (i <- 0 to m.argList.length - 1) {
                    if (args.length <= i) {
                      if (!m.argList(i).defaultValue.isDefined) {
                        //No argument and no default value
                        error("Not enough arguments for method " + meth.value, expr)
                      } //else the default value is used, so there's nothing to checkp
                    }
                    else {
                      tcExpr(args(i), m.argList(i).getType.innerType)
                    }
                  }
                }
                m.getType
              case None => TError
            }
            case _ => TError
          }
        case IntLit(value) => TInt
        case StringLit(value) => TString
        case True() => TBoolean
        case False() => TBoolean
        case id: Identifier => id.getType
        case t: This => t.getSymbol.getType
        case NewIntArray(size) =>
          tcExpr(size, TInt)
          TIntArray
        case NewBoolArray(size) =>
          tcExpr(size, TInt)
          TBoolArray
        case NewStringArray(size) =>
          tcExpr(size, TInt)
          TStringArray
        case NewObjectArray(size, tpe) =>
          tcExpr(size, TInt)
          tcExpr(tpe, TAnyObject)
          tpe.getSymbol match {
            case cs: ClassSymbol => TObjectArray(cs)
            case _ => TError
          }
        case New(tpe) => tcExpr(tpe, TAnyObject)
        case Not(expr) => tcExpr(expr, TBoolean)
        case _ => TError
      }

      // Check result and return a valid type in case of error
      if (expected.isEmpty) {
        expr.setType(tpe)
        tpe
      }
      else {
        if (!expected.exists(e => tpe.isSubTypeOf(e))) {
          error("Type error: Expected: " + expected.toList.mkString(" or ") + ", found: " + tpe, expr)
          expected.head
        }
        else {
          expr.setType(tpe)
          tpe
        }
      }
    }

    def tcStat(stat: StatTree): Unit = {
      stat match {
        case Block(stats) => stats.foreach { tcStat(_) }
        case If(expr, thn, els) =>
          tcExpr(expr, TBoolean)
          tcStat(thn)
          els match {
            case Some(s) => tcStat(s)
            case None => // nothing
          }
        case While(expr, stat) =>
          tcExpr(expr, TBoolean)
          tcStat(stat)
        case Println(expr) => tcExpr(expr, TInt, TString, TBoolean)
        case Assign(id, expr) => tcExpr(expr, id.getType)
        case ArrayAssign(id, index, expr) =>
          // TODO quick hack to allow statements like:
          // var boolArr: Bool[]; boolArr[1] = false;
          // fix if time. Unwanted effect is: boolArr[1] = "hello world" will typecheck and
          // there will be an ugly runtime error thrown by the JVM if bytecode is run.
          //tcExpr(id, TIntArray, TBoolArray, TStringArray, TObjectArray)
          tcExpr(index, TInt)
          tcExpr(expr, TInt, TBoolean, TString, TAnyObject)
        case _ =>
      }
    }

    // Traverse and typecheck the program.

    prog.main.stats.foreach(tcStat(_))
    prog.classes.foreach {
      cl =>
        cl.vars.foreach {
          v =>
            v.defaultValue match {
              case Some(dv) => tcExpr(dv, v.getSymbol.getType)
              case None =>
            }
        }
        cl.methods.foreach {
          m =>
            m.vars.foreach {
              v =>
                v.defaultValue match {
                  case Some(dv) => tcExpr(dv, v.getSymbol.getType)
                  case None =>
                }
            }
            m.stats.foreach { tcStat(_) }
            tcExpr(m.retExpr, m.getSymbol.getType)
        }
    }

    prog
  }
}
