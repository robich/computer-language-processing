object TestCodeGen {
    def main() : Unit = {
        if(new TestSuite().passAllTests()) {
            println("");
            println("All tests passed!");
        } else {
            println("");
            println("There were errors.");
        }
    }
}

class TestSuite {

    var hasErrors: Bool;
    var tmp: Bool;
    
    def passAllTests(): Bool = {
        var s1: String;
        var s2: String;
        var s3: String;
        
        s1 = "s1";
        s2 = "s2";
        s3 = "s3";
        hasErrors = true;
        
        println("Testing code generation: should only print true");
        
        // Test expressions:
        
        println("");
        println("-- TESTING EXPRESSIONS --");
        println("");
        
        // AND
        println("----AND----");
        tmp = true && true; hasErrors = hasErrors && tmp;
        println(tmp);
        tmp = true && true && true; hasErrors = hasErrors && tmp;
        println(tmp);
        tmp = !(false && (1/0 == 1)); hasErrors = hasErrors && tmp;
        println(tmp);  // test lazy AND
        
        // OR
        println("----OR----");
        tmp = true || true; hasErrors = hasErrors && tmp;
        println(tmp);
        tmp = false || false || true; hasErrors = hasErrors && tmp;
        println(tmp);
        tmp = (true || (1/0 == 1)); hasErrors = hasErrors && tmp;
        println(tmp); // test lazy OR
        
        // PLUS
        println("----PLUS----");
        tmp = 2+2 == 4; hasErrors = hasErrors && tmp;
        println(tmp);
        tmp = 5+5 == 10; hasErrors = hasErrors && tmp;
        println(tmp);
        tmp = 1+2+3+4+5+6+7+8+10 == 46; hasErrors = hasErrors && tmp;
        println(tmp);
        
        // PLUS (Expression Tree has type String)
        println("----PLUS(STRING CONCAT)----");
        println("" + "true");
        println("t" + "rue");
        println("tr" + "ue");
        println("tru" + "e");
        println("true" + "");
        println(0 + "true");
        println("true" + 55);
        println(0 + "true" + 12345 + 14725);
        
        // MINUS
        println("----MINUS----");
        tmp = 1-1 == 0; hasErrors = hasErrors && tmp;
        println(tmp);
        tmp = 2-2 == 0; hasErrors = hasErrors && tmp;
        println(tmp);
        tmp = 5-1 == 4; hasErrors = hasErrors && tmp;
        println(tmp);
        tmp = 100-9-8-7 == 76; hasErrors = hasErrors && tmp;
        println(tmp);
        tmp = 0-0 == 0; hasErrors = hasErrors && tmp;
        println(tmp);
        tmp = 1-2 < 0; hasErrors = hasErrors && tmp;
        println(tmp);
        
        // TIMES
        println("----TIMES----");
        tmp = 0*0 == 0; hasErrors = hasErrors && tmp;
        println(tmp);
        tmp = 0 * 1 == 0; hasErrors = hasErrors && tmp;
        println(tmp);
        tmp = 0 * (1*2*3*4*5*6) == 0; hasErrors = hasErrors && tmp;
        println(tmp);
        tmp = 1*1 == 1; hasErrors = hasErrors && tmp;
        println(tmp);
        tmp = 2*2 == 4; hasErrors = hasErrors && tmp;
        println(tmp);
        tmp = 6*6*1*2*3 == 216; hasErrors = hasErrors && tmp;
        println(tmp);
        
        // DIV
        println("----DIV----");
        tmp = 0/1 == 0; hasErrors = hasErrors && tmp;
        println(tmp);
        tmp = 0/4 == 0; hasErrors = hasErrors && tmp;
        println(tmp);
        tmp = 1/1 == 1; hasErrors = hasErrors && tmp;
        println(tmp);
        tmp = 3/2 == 1; hasErrors = hasErrors && tmp;
        println(tmp);
        tmp = 10/2 == 5; hasErrors = hasErrors && tmp;
        println(tmp);
        
        // LESSTHAN
        println("----LESSTHAN----");
        tmp = 0 < 1; hasErrors = hasErrors && tmp;
        println(tmp);
        tmp = 2 < 3; hasErrors = hasErrors && tmp;
        println(tmp);
        tmp = 2-3 < 1; hasErrors = hasErrors && tmp;
        println(tmp);
        tmp = 4-8 < 0; hasErrors = hasErrors && tmp;
        println(tmp);
        
        // EQUALS
        println("----EQUALS----");
        tmp = 2 == 2; hasErrors = hasErrors && tmp;
        println(tmp);
        tmp = (6-1) == 5; hasErrors = hasErrors && tmp;
        println(tmp);
        tmp = 0 == 0; hasErrors = hasErrors && tmp;
        println(tmp);
        
        // ArrayRead
        println("----ArrayRead----");
        tmp = new C().read(); hasErrors = hasErrors && tmp;
        println(tmp);
        
        // ArrayLength
        println("----ArrayLength----");
        tmp = new C().testArrayLength(7); hasErrors = hasErrors && tmp;
        println(tmp);
        
        // MethodCall
        println("----MethodCall----");
        tmp = new A().getOne() == 1; hasErrors = hasErrors && tmp;
        println(tmp);
        tmp = new A().getSquare(2) == 4; hasErrors = hasErrors && tmp;
        println(tmp);
        tmp = new A().getString() == "true"; hasErrors = hasErrors && tmp;
        println(tmp);
        tmp = new A().getTrue(); hasErrors = hasErrors && tmp;
        println(tmp);
        tmp = new A().printTrue() == 0; hasErrors = hasErrors && tmp;
        println(tmp);
        tmp = new A().getSum(7, 8) == 15; hasErrors = hasErrors && tmp;
        println(tmp);
        println(new C().testReturnString());
        
        // This
        println("----This----");
        tmp = this.getTwice(6) == 12; hasErrors = hasErrors && tmp;
        println(tmp);
        
        
        // Not
        println("----NOT----");
        tmp = !false; hasErrors = hasErrors && tmp;
        println(tmp);
        tmp = !!!false; hasErrors = hasErrors && tmp;
        println(tmp);
        tmp = !!true; hasErrors = hasErrors && tmp;
        println(tmp);
        tmp = !(!!false); hasErrors = hasErrors && tmp;
        println(tmp);
        
        // Test statements:
        
        println("");
        println("-- TESTING STATEMENTS --");
        println("");
        
        // IF
        println("----IF----");
        if (true) { println(true); } else { println(false); hasErrors = true;}
        if (false) { println(false); hasErrors = true;} else { println(true); }
        if (true) { println(true); }
        if (false) { println(false); hasErrors = true;}
        
        // WHILE
        println("----WHILE----");
        tmp = new B().loop(5) == 5; hasErrors = hasErrors && tmp;
        println(tmp);
        
        // ArrayAssign
        println("----ArrayAssign----");
        println("----IntArrayType----");
        tmp = new C().getOne() == 1; hasErrors = hasErrors && tmp;
        println(tmp);
        println("----BoolArrayType----");
        tmp = new C().testBoolArray(); hasErrors = hasErrors && tmp;
        println(tmp);
        println("----StringArrayType----");
        /*tmp = new C().testStringArray(); hasErrors = hasErrors && tmp;
        println(tmp);*/
        
        // MINI-PROJECT NEW FEATURES
        println("");
        println("-- MINI-PROJECT NEW FEATURES --");
        println("");
        
        // Arbitrary param number
        println("----Arbitrary Param Number----");
        tmp = new C().multiArgs0(42, s1) == 42;
        // TODO: new C().multiArgs0(s1, 42) isn't rejected by NameAnalysis/TypeChecker.
        println(tmp); hasErrors = hasErrors && tmp;
        tmp = new C().multiArgs(42, true, "hello") == 42;
        println(tmp); hasErrors = hasErrors && tmp;
        
        return hasErrors;
    }
    
    def getTwice(n: Int): Int = {
        return 2 * n;
    }

}

class A {
    def getOne(): Int = {
        var x: Int;
        x = 0;
        x = x + 1;
        x = x + 7;
        x = x + 8;
        x = x - 15;
        return x;
    }
    
    def getSquare(n: Int): Int = {
        return n*n;
    }
    
    
    def getString(): String = {
        return "true";
    }
    
    def getTrue(): Bool = {
        return true;
    }
    
    def printTrue(): Int = {
        println(true);
        return 0;
    }
    
    def getSum(a: Int, b: Int): Int = {
        return a + b;
    }
}

class B {
	var x : Int;
	def loop(i: Int): Int = {
		x = 0;
		while(x < i){
			x = x + 1;
		}
		return x;
	}
}

class C {
    var tmpBool: Bool;
	var arr : Int[];
	var boolArr: Bool[];
	var tmpString: String;
	//var stringArr: String[];
	
	def testReturnString(): String = {
	   return "true";
	}
	
	def getOne(): Int = {
		arr = new Int[42];
		arr[4] = 1;
		return arr[4];
	}
	
	def testBoolArray(): Bool = {
	   boolArr = new Bool[42];
	   boolArr[7] = true;
	   tmpBool = boolArr[7];
	   return tmpBool;
	}
	
	def testStringArray(): Bool = {
	   tmpString = "hello, wolrd ;-) !";
	   //stringArr = new String[42];
	   return false;
	}
	
	def testArrayLength(n: Int): Bool = {
	   arr = new Int[n];
	   return arr.length == n;
	}
	
	def read(): Bool = {
	   arr = new Int[7];
	   arr[6] = 5;
	   return arr[6] == 5;
	}
	
	def multiArgs0(a1: Int, a2: String*): Int = {
	   return a1;
	}
	
	def multiArgs(a1: Int, a2: Bool, a3: String*): Int = {
	   return a1;
	}
}