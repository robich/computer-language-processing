object TestIntArray {
    def main() : Unit = {
        println(new Tester().run(1));
    }
}

class Tester {
    def run(a: Int): Int = {
        var arr: Int[];
        arr = new Int[5];
        arr[0] = 2;
        arr[1] = 4;
        arr[2] = 8;
        arr[3] = 16;
        arr[4] = 32;
        println(this.printIntArray(1, 2, 3));
        return 0;
    }
    
    def nothing(): Bool = { return true; }
    
    def printIntArray(array: Int*): Bool = {
        var i: Int;
        var a: String;
        i = 0;
        a = "array: ";
        while (i < array.length) {
            a = a + array[i] + " ";
            i = i + 1;
        }
        println(a);
        return true;
    }
}