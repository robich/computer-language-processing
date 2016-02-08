object TestEquality {
    def main() : Unit = {
        if(new Computer().test()) { println("Ok"); } else { println("error"); }
    }
}
 
class Computer {
    def test() : Bool = {
		var s1: String;
		var s2: String;
		var i1: Int;
		var i2: Int;
		var i3: Int;
		var progOk: Bool;
		
		progOk = true;
		
		s1 = "hello, world!";
		s2 = "hello, world!";
		
		i1 = 42;
		i2 = 42;
		i3 = 7;
		
		progOk = !(s1 == s2);
		progOk = progOk && !(s1 == s1); // String equality in tool works by reference!
		progOk = progOk && (i1 == i2);
		progOk = progOk && !(i1 == i3);
		progOk = progOk && (s1 == s1);
		
		return true;
	}
}