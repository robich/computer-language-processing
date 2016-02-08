object Else {
	def main() : Unit = {
		if(new Computer().compute()) {
			println("Ok");
		} else {
		  println("Error");
		}
	}
}

class Computer {

    def returnArgPlusOne(n: Int): Int = {
        return n + 1;
    }
    def compute() : Bool = {
		var counter: Int;
		var b: Bool;
		var c: Int;
		
		b = false;
		
		counter = 0;
		if (1==1) {
			counter = counter + 1;
		}
		if (1==0) {
			counter = 1000;
		} 
		
		if (1==0) {
			counter = 1000;
		}
		
		if (!b) {
			counter = counter + 1;
		}
		
		b = (counter == 3);
		
		c = this.returnArgPlusOne(41);
		
		return b;
	}
}