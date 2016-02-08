object Simple {
    def main() : Unit = {
        println("This program should not pass name analysis.");
    }
}

class A extends B {
    def hiA(): Int = {
        return 1;
    }
}

class B extends A {
    def hiB(): Int = {
        return 2;
    }
}

