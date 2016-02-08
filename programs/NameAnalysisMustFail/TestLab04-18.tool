// Must fail: "Error: Declaration of a as local shadows method parameter of the same name."
object Main {

    def main(): Unit = {

        {}

    }

}



class A {

    def foo(a: Int): Int = {

        var a: Bool;

        a = false;

        return 52;

    }

}