// Must fail: "Error: member declaration overrides previous declaration at programs\TestLab04-09.tool:16:9."
object Main {

    def main(): Unit = {

        {}

    }

}



class A {

    var mem: Int;

}



class B extends A {}



class C extends B {

    var mem: Int;

}