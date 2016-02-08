object Simple {
    def main() : Unit = {
        println("Should fail at type checking");
    }
}

class A {
    var res: Int;
    var a: Bool;
    var b: Bool;
    var c: Bool;
    def compute(arg: Int): Int = {
        res = 0;
        a = true;
        b = false;
        c = a && "b";
        if (c) {
            res = 1;
        } else {
            res = 2;
        }
        
        return res;
    }
}

class B extends A {
    def compute(arg: Int): Int = {
        return 2;
    }
}