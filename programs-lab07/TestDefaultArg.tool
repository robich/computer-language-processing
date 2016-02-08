object TestDefaultArg {
    def main() : Unit = {
        println(new A().foo(6));
        println(new A().foo(1, 2, "foo"));
        println(new A().bar(1, "a", 42));
    }
}

class A {
    def foo(m: Int, n: Int = 7, o: String = "bar"): Bool = {
        println("m: " + m + " n: " + n + " o: " + o);
        return true;
    }
    
    def bar(m: Int, n: String = "hello", o: Int): Bool = {
        println("m: " + m + " n: " + n + " o: " + o);
        return true;
    }
}