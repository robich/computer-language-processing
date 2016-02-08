// Must fail: "Error: Parameter name a is used twice in foo."
object Main {

    def main(): Unit = {

        {}

    }

}



class A {

    def foo(a: Int, b: Int, a: Bool): Int = {

        return 52;

    }

}