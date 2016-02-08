object TestObjectArray {
    def main() : Unit = {
        println(new Tester2().run(new Cat(), new Cat()));
    }
}

class Tester {
    def run(a: Int): Int = {
        var cats: Cat[];
        
        cats = new Cat[42];
        cats[0] = new Cat();
		
        println(cats[0].meow());
        return 0;
    }
}

class Tester2 {
    def run(cats: Cat*): String = {
        var i: Int;
		var s: String;
		i = 0;
		s = "";
		
		while(i < cats.length){
			s = s + cats[i].meow();
			i = i + 1;
		}
		
        return s;
    }
}

class Cat {    
    def meow(): String = {
        return "Meow!";
    }
}