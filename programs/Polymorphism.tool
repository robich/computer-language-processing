object Polymorphism {
    def main() : Unit = {

		if (42 == 42) { println("Ok"); } else { println("error"); }
		if (new A() == new A()) { println("error"); } else { println("Ok"); }
		if (new A() == new B()) { println("error"); } else { println("Ok"); }
		if ("f" + "oo" == "fo" + "o") { println("error"); } else { println("Ok"); }
		if ("fo" + "o" == "" + "foo") { println("error"); } else { println("Ok"); }
		
        if(new Computer().test()) { println("Polymorphism Ok"); } else { println("Polymorphism error"); }
    }
}
 
class Computer {
    def test() : Bool = {
		var progCorrect : Bool;
		var objA : A;
		var objB : B;
		var objC:C; // test no space
		var objD : D;
		
		objA = new A();
		objB = new B();
		objC = new C();
		objD = new D();
		
		progCorrect = (objA.getNumber() == 1 && objB.getNumber() == 2 && (objC.getNumber() == 1));
		progCorrect = progCorrect && ((objD.getNumber() == 3)) && (objD.getZero() == 0);
		progCorrect = progCorrect && (objD.getSum() == 3) && (objD.getLength() == 10);
		
		return progCorrect;
	}
}

class A {
	def getNumber() : Int = {
		return 1;
	}
	
	def getZero() : Int = {
		return 0;
	}
}

class B {
	def getNumber() : Int = {
		return 2;
	}
}

class C extends A {
	// empty
}

class D extends A {
	var array : Int[];
	
	def getNumber() : Int = {
		return 3;
	}
	
	def getSum() : Int = {
		return this.getNumber(); // + super.getNumber(); // no super in TOOL ?
	}

	def getLength() : Int = {
		array = new Int[10];
		return array.length;
	}
}