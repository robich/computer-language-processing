object StringVarArg {
    def main() : Unit = {
        println(new A().foo("b", "a", "r"));
    }
}

class A {
    def foo(array: String*): String = {
        var i: Int = 0;
        var a: String;
        i = 0;
        a = "";
        while (i < array.length) {
            a = a + array[i];
            i = i + 1;
        }
        return a;
    }
}