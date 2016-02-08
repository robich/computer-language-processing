object Simple {
    def main() : Unit = {
        println("Should fail at name analysis");
    }
}

class A {
    def compute(a: Int): Int = {
        return 1;
    }
}

class B extends A {
    // illegal override
    def compute(a: Int, b: Int): Int = {
        return 2;
    }
}