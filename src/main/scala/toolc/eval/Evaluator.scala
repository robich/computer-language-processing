package toolc
package eval

import ast.Trees._
import utils._

class Evaluator(ctx: Context, prog: Program) {
  import ctx.reporter._

  def eval() {
    // Initialize the context for the main method
    val ectx = new MainMethodContext

    // Evaluate each statement of the main method
    prog.main.stats.foreach(evalStatement(ectx, _))
  }

  def evalStatement(ectx: EvaluationContext, stmt: StatTree): Unit = stmt match {
    case Block(stats) =>
      stats.foreach(evalStatement(ectx, _))
    case If(expr, thn, els) =>
      if (evalExpr(ectx, expr).asBool) {
        evalStatement(ectx, thn)
      }
      else {
        els match {
          case Some(stmt) => evalStatement(ectx, stmt)
          case _          => () // if didn't evaluate and there were no else statement
        }
      }
    case While(expr, stat) =>
      while (evalExpr(ectx, expr).asBool) {
        evalStatement(ectx, stat)
      }
    case Println(expr) =>
      evalExpr(ectx, expr) match {
        case ArrayValue(e, s) => println(e)
        case BoolValue(v)     => println(v)
        case IntValue(v)      => println(v)
        case StringValue(v)   => println(v)
        case ObjectValue(v)   => println(v.vars.mkString)
      }
    case Assign(id, expr) =>
      ectx.setVariable(id.value, evalExpr(ectx, expr))
    case ArrayAssign(id, index, expr) =>
      ectx.getVariable(id.value).asArray.setIndex(evalExpr(ectx, index).asInt, evalExpr(ectx, expr).asInt)
    case _ =>
      fatal("unnexpected statement", stmt)
  }

  def evalExpr(ectx: EvaluationContext, e: ExprTree): Value = e match {
    case IntLit(value)    => IntValue(value)
    case StringLit(value) => StringValue(value)
    case True()           => BoolValue(true)
    case False()          => BoolValue(false)
    case And(lhs, rhs)    => BoolValue(evalExpr(ectx, lhs).asBool && evalExpr(ectx, rhs).asBool)
    case Or(lhs, rhs)     => BoolValue(evalExpr(ectx, lhs).asBool || evalExpr(ectx, rhs).asBool)
    case Plus(lhs, rhs) =>
      val pair = (evalExpr(ectx, lhs), evalExpr(ectx, rhs))
      pair match {
        case (IntValue(l), IntValue(r))       => IntValue(l + r)
        case (IntValue(l), StringValue(r))    => StringValue(l + r)
        case (StringValue(l), IntValue(r))    => StringValue(l + r)
        case (StringValue(l), StringValue(r)) => StringValue(l + r)
        case _ => fatal("Type error: Expected one of the following types: int, String, found: " +
          pair._1.getClass.getSimpleName + ", " + pair._2.getClass.getSimpleName)
      }
    case Minus(lhs, rhs) =>
      val pair = (evalExpr(ectx, lhs), evalExpr(ectx, rhs))
      pair match {
        case (IntValue(l), IntValue(r)) => IntValue(l - r)
        case _ => fatal("Type error: Expected one of the following types: int, found: " +
          pair._2.getClass.getSimpleName + ", " + pair._2.getClass.getSimpleName)
      }
    case Times(lhs, rhs) =>
      val pair = (evalExpr(ectx, lhs), evalExpr(ectx, rhs))
      pair match {
        case (IntValue(l), IntValue(r)) => IntValue(l * r)
        case _ => fatal("Type error: Expected one of the following types: int, found: " +
          pair._1.getClass.getSimpleName + ", " + pair._2.getClass.getSimpleName)
      }
    case Div(lhs, rhs) =>
      val pair = (evalExpr(ectx, lhs), evalExpr(ectx, rhs))
      pair match {
        case (IntValue(l), IntValue(r)) => IntValue(l / r)
        case _ => fatal("Type error: Expected one of the following types: int, found: " +
          pair._1.getClass.getSimpleName + ", " + pair._2.getClass.getSimpleName)
      }
    case LessThan(lhs, rhs) =>
      val pair = (evalExpr(ectx, lhs), evalExpr(ectx, rhs))
      pair match {
        case (IntValue(l), IntValue(r)) => BoolValue(l < r)
        case _ => fatal("Type error: Expected one of the following types: int, found: " +
          pair._1.getClass.getSimpleName + ", " + pair._2.getClass.getSimpleName)
      }
    case Not(expr) =>
      val evald = evalExpr(ectx, expr)
      evald match {
        case BoolValue(b) => BoolValue(!b)
        case _            => fatal("Type error: Expected one of the following types: Boolean, found: " + evald.getClass.getSimpleName)
      }
    case Equals(lhs, rhs) =>
      val lv = evalExpr(ectx, lhs)
      val rv = evalExpr(ectx, rhs)
      val res = (lv, rv) match {
        case (IntValue(l), IntValue(r))   => l == r
        case (BoolValue(l), BoolValue(r)) => l == r
        case (lr, rr)                     => lr eq rr
      }
      BoolValue(res)

    case ArrayRead(arr, index) => IntValue(evalExpr(ectx, arr).asArray.getIndex(evalExpr(ectx, index).asInt))
    case ArrayLength(arr)      => IntValue(evalExpr(ectx, arr).asArray.size)
    case MethodCall(obj, meth, args) =>
      val objv = evalExpr(ectx, obj).asObject
      val mctx = new MethodContext(objv)
      val met = findMethod(objv.cd, meth.value)
      mctx.declareVariable("this")
      mctx.setVariable("this", objv)

      // Add the parameters and the variables defined inside the function to the meth. cont.
      //met.args foreach { v => mctx.declareVariable(v.id.value) }
      met.vars foreach { v => mctx.declareVariable(v.id.value) }

      // Set the values for all the args, we can't do that for the vars because we don't know their values yet :)
      met.args zip args foreach { case (me, argsExpr) => 
        mctx.declareVariable(me.id.value)
        mctx.setVariable(me.id.value, evalExpr(ectx, argsExpr)) }

      // Evaluate method's statements
      met.stats.foreach(evalStatement(mctx, _))

      // Return
      evalExpr(mctx, met.retExpr)

    case Identifier(name) => ectx.getVariable(name)
    case New(tpe) =>
      val cd = findClass(tpe.value)
      val objv = new ObjectValue(cd)

      fieldsOfClass(cd).foreach { f => objv.declareField(f) }
      objv
    case This() => ectx match {
      case mctx: MethodContext => mctx.obj
    }
    case NewIntArray(size) =>
      val sz = evalExpr(ectx, size).asInt
      ArrayValue(new Array[Int](sz), sz)
  }

  // Define the scope of evaluation, with methods to access/declare/set local variables(or arguments)
  abstract class EvaluationContext {
    def getVariable(name: String): Value
    def setVariable(name: String, v: Value): Unit
    def declareVariable(name: String): Unit
  }

  // A Method context consists of the execution context within an object method.
  // getVariable can fallback to the fields of the current object
  class MethodContext(val obj: ObjectValue) extends EvaluationContext {
    var vars = Map[String, Option[Value]]()

    def getVariable(name: String): Value = {
      vars.get(name) match {
        case Some(ov) =>
          ov.getOrElse(fatal("Uninitialized variable '" + name + "'"))
        case _ =>
          obj.getField(name)
      }
    }

    def setVariable(name: String, v: Value) {
      if (vars contains name) {
        vars += name -> Some(v)
      }
      else {
        obj.setField(name, v)
      }
    }

    def declareVariable(name: String) {
      vars += name -> None
    }
  }

  // Special execution context for the main method, which is very limitted.
  class MainMethodContext extends EvaluationContext {
    def getVariable(name: String): Value = fatal("The main method contains no variable and/or field")
    def setVariable(name: String, v: Value): Unit = fatal("The main method contains no variable and/or field")
    def declareVariable(name: String): Unit = fatal("The main method contains no variable and/or field")
  }

  // Helper functions to query the current program
  def findMethod(cd: ClassDecl, name: String): MethodDecl = {
    cd.methods.find(_.id.value == name).orElse(
      cd.parent.map(p => findMethod(findClass(p.value), name))).getOrElse(fatal("Unknown method " + cd.id + "." + name))
  }

  def findClass(name: String): ClassDecl = {
    prog.classes.find(_.id.value == name).getOrElse(fatal("Unknown class '" + name + "'"))
  }

  def fieldsOfClass(cl: ClassDecl): Set[String] = {
    cl.vars.map(_.id.value).toSet ++
      cl.parent.map(p => fieldsOfClass(findClass(p.value))).getOrElse(Set())
  }

  // Runtime evaluation values, with as* methods which act as typecasts for convenience.
  sealed abstract class Value {
    def asInt: Int = fatal("Unnexpected value, found " + this + " expected Int")
    def asString: String = fatal("Unnexpected value, found " + this + " expected String")
    def asBool: Boolean = fatal("Unnexpected value, found " + this + " expected Boolean")
    def asObject: ObjectValue = fatal("Unnexpected value, found " + this + " expected Object")
    def asArray: ArrayValue = fatal("Unnexpected value, found " + this + " expected Array")
  }

  case class ObjectValue(cd: ClassDecl) extends Value {
    var fields = Map[String, Option[Value]]()

    def setField(name: String, v: Value) {
      if (fields contains name) {
        fields += name -> Some(v)
      }
      else {
        fatal("Unknown field '" + name + "'")
      }
    }

    def getField(name: String) = {
      fields.get(name).flatten.getOrElse(fatal("Unknown field '" + name + "'"))
    }

    def declareField(name: String) {
      fields += name -> None
    }

    override def asObject = this
  }

  case class ArrayValue(var entries: Array[Int], val size: Int) extends Value {
    def setIndex(i: Int, v: Int) {
      if (i >= size || i < 0) {
        fatal("Index '" + i + "' out of bounds (0 .. " + size + ")")
      }
      entries(i) = v
    }

    def getIndex(i: Int) = {
      if (i >= size || i < 0) {
        fatal("Index '" + i + "' out of bounds (0 .. " + size + ")")
      }
      entries(i)
    }

    override def asArray = this
  }

  case class StringValue(var v: String) extends Value {
    override def asString = v
  }

  case class IntValue(var v: Int) extends Value {
    override def asInt = v
  }

  case class BoolValue(var v: Boolean) extends Value {
    override def asBool = v
  }
}
