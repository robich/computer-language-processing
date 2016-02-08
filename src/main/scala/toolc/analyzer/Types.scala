package toolc
package analyzer

import Symbols._
import scala.Equals
import toolc.ast.Trees.Identifier

object Types {
  trait Typed {
    self =>

    private var _tpe: Type = TUntyped

    def setType(tpe: Type): self.type = { _tpe = tpe; this }
    def getType: Type = _tpe
  }

  sealed abstract class Type {
    def isSubTypeOf(tpe: Type): Boolean
    def innerType: Type
  }

  case object TError extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = true
    override def toString: String = "[error]"
    override def innerType: Type = this
  }

  case object TUntyped extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = false
    override def toString: String = "[untyped]"
    override def innerType: Type = this
  }

  case object TInt extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TInt => true
      case _ => false
    }
    override def toString: String = "int"
    override def innerType: Type = this
  }

  case object TBoolean extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TBoolean => true
      case _ => false
    }
    override def toString: String = "bool"
    override def innerType: Type = this
  }

  case object TIntArray extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TIntArray => true
      case _ => false
    }
    override def toString: String = "Int[]"
    override def innerType: Type = TInt
  }

  case object TBoolArray extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TBoolArray => true
      case _ => false
    }
    override def toString: String = "Bool[]"
    override def innerType: Type = TBoolean
  }

  case object TStringArray extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TStringArray => true
      case _ => false
    }
    override def toString: String = "String[]"
    override def innerType: Type = TString
  }

  case class TObjectArray(cs: ClassSymbol) extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TAnyObjectArray => true
      case TObjectArray(classSymbol) => classSymbol.name.equals(cs.name)
      case _ => false
    }
    override def toString: String = cs.name + "[]"
    override def innerType: Type = cs.getType
  }

  case object TAnyObjectArray extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TAnyObjectArray => true
      case _ => false
    }
    override def toString: String = "Array"
    override def innerType: Type = TAnyObject
  }
  
  case object TString extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TString => true
      case _ => false
    }
    override def toString: String = "String"
    override def innerType: Type = this
  }

  case class TObject(classSymbol: ClassSymbol) extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = {
      if ((tpe == TAnyObject) || (tpe == classSymbol.getType)) {
        true
      }
      else {
        classSymbol.parent match {
          case Some(p) => p.getType.isSubTypeOf(tpe)
          case None => false
        }
      }
    }
    override def toString: String = classSymbol.name
    override def innerType: Type = this
  }

  // Special object to implement the fact that all objects are its subclasses.
  case object TAnyObject extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TAnyObject => true
      case _ => false
    }
    override def toString: String = "Object"
    override def innerType: Type = this
  }
}
