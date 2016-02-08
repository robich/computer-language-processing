object TestDefaultMembers {
    def main() : Unit = {
        println(new Tester().run(1));
    }
}

class Tester {
	var n: Int = 5;
    def run(a: Int): Int = {
        var b: Int = a + 1;
		n = 2;
		return b + n;
    }
}