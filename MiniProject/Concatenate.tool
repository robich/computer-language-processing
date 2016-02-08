object Concatenate {
    def main() : Unit = {
		println(new Concatenator().run("a", "b", "c"));
    }
}

class Concatenator {
	def run(strings: String*): String = {
		var i: Int;
		var result: String;
		i = 0;
		result = "";
		while(i < strings.length){
			result = result + strings[i];
			i = i + 1;
		}
		return result;
	}
}