object BoolArray {
    def main() : Unit = {
        println(new A().foo());
    }
}

class A {
    def foo(): Bool = {
        var ba: Bool[];
        ba = new Bool[3];
        ba[0] = true;
        ba[1] = false;
        ba[2] = ba[0];
        println(ba[0]);
        println(ba[1]);
        
        return ba[2];
    }
}