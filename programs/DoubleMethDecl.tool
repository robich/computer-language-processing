object Simple {
    def main() : Unit = {
        if(new Computer().compute()) {
            println("Ok");
            println("Yes, ok!");
            while(1 < 0) {
                println("Not ok");
            }
            while (0 < 1) {
                println("Ok");
            }
        } else {
          println("Error");
        }
    }
}

class Computer {
    var a: Int;
    var b: Bool;
    def compute(): Int = {
        return 1;
    }
    
    def compute(): Int = {
        return 2;
    }
}