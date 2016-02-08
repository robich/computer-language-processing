// Must fail: "Error: foo overrides previous definition from programs\TestLab04-16:16:5 with a different number of parameters."
object Main {

    def main(): Unit = {

        {}

    }

}



class A {

    def foo(a: Int, b: Bool): Int = {

        return 52;

    }

}



class B extends A { }



class C extends B {

    def foo(a: Int): Int = {

        return 52;

    }

}