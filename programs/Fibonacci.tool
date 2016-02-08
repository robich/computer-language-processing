object Factorial {
    def main() : Unit = {
        println(new Computer().printFib(25));        
    }
}

class Computer {
    def fib(n : Int) : Int = {
		var res: Int;
		res = 0;
        if ((n == 1) || (n == 0)) {
			res = 1;
		}
		else {
			res = this.fib(n - 1) + this.fib(n - 2);
		}
		
		return res;
    }
	
	def printFib(n: Int): String = {
		var i: Int;
		var out: Int;
		
		i = 0;
		out = 0;
		
		while (i < n + 1) {
			out = this.fib(i);
			println("fib(" + i + ") = " + out);
			i = i + 1;
		}
		
		return "Finished";
	}
}
