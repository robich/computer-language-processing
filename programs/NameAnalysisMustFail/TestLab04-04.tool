// Must fail: "Class ABCD is defined more than once. First definition here: programs\TestLab04-04.tool:7:1"
object BinarySearch {
    def main(): Unit = {
        {}
    }
}

class ABCD {

}

class ABCD extends W {
    
}

class W {
    
}