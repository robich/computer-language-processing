object Trololo {
    def main() : Unit = {
		println(new B().seta(420) == 420 && new B().getOne() == 1);
    }
}

class A {
	var a : Int;
	def getB(): B = {
		return new B();
	}
	
	def getA(): A = {
		return new A();
	}
	
	def getOne(): Int = {
		return 1;
	}
	
	def getGetB(): B = {
		return this.getB();
	}
}

class B extends A {
	def toString(): String = {
		return "B";
	}
	
	def test(arg: Int): Bool= {
		while(arg == 0){
			arg = arg - 1;
		}
		return true;
	}
	
	def seta(x: Int): Int = {
		a = x;
		return a;
	}
}