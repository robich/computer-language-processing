package toolc
package analyzer

import utils._
import Types._
import toolc.ast.Trees
import scala.annotation.tailrec
import toolc.ast.Trees.ExprTree

object Symbols {
  trait Symbolic[S <: Symbol] {
    private var _sym: Option[S] = None

    def setSymbol(sym: S): this.type = {
      _sym = Some(sym)
      this
    }

    def getSymbol: S = _sym match {
      case Some(s) => s
      case None => sys.error("Accessing undefined symbol.")
    }
  }

  sealed abstract class Symbol extends Positioned with Typed {
    val id: Int = ID.next
    val name: String
  }

  private object ID {
    private var c: Int = 0

    def next: Int = {
      val ret = c
      c = c + 1
      ret
    }
  }

  class GlobalScope {
    var mainClass: ClassSymbol = _
    var classes = Map[String, ClassSymbol]()

    def lookupClass(n: String): Option[ClassSymbol] = classes.get(n)
  }

  class ClassSymbol(val name: String) extends Symbol {
    var parent: Option[ClassSymbol] = None
    var methods = Map[String, MethodSymbol]()
    var members = Map[String, VariableSymbol]()

    // We check methods first, then lookup in the parent class if it exists.
    @tailrec
    final def lookupMethod(n: String): Option[MethodSymbol] = {
      methods.get(n) match {
        case Some(m) => methods.get(n)
        case None => parent match {
          case Some(p) => p.lookupMethod(n)
          case None => None
        }
      }
    }

    // We check members first, then lookup in the parent class if it exists.
    @tailrec
    final def lookupVar(n: String): Option[VariableSymbol] = {
      members.get(n) match {
        case Some(v) => members.get(n)
        case None => parent match {
          case Some(p) => p.lookupVar(n)
          case None => None
        }
      }
    }
  }

  class MethodSymbol(val name: String, val classSymbol: ClassSymbol, val hasVarArg: Boolean = false) extends Symbol {
    var params = Map[String, VariableSymbol]()
    var members = Map[String, VariableSymbol]()
    var argList: List[VariableSymbol] = Nil
    var overridden: Option[MethodSymbol] = None

    // We check members, then arguments, then the containing class.
    def lookupVar(n: String): Option[VariableSymbol] = {
      members.get(n) match {
        case Some(v) => members.get(n)
        case None => params.get(n) match {
          case Some(v) => params.get(n)
          case None => classSymbol.lookupVar(n)
        }
      }
    }
  }

  class VariableSymbol(val name: String, val defaultValue: Option[ExprTree] = None) extends Symbol
}
