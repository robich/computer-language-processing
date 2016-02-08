/*
	This simple program uses the eratostene's sieve to print all primes below 20.
	It is used to test the correctness of our interpreter/compiler.
	// test
	/* test
}*/

object ListPrimes {
    def main() : Unit = {
        if(new Computer().computeAndTestPrimes(100)) { println("Ok"); } else { println("error"); }
    }
}
 
class Computer {
    def computeAndTestPrimes(n : Int) : Bool = {
		// variables declaration
		var isPrime : Int[];
		var refPrimes : Int[];
		var rpIndex : Int;
		var i : Int;
		var j : Int;
		var programCorrect : Bool;
		
		// actual program
        println("--- ListPrimes tool program ---");
		
		programCorrect = true;
		
		isPrime = new Int[n + 1];
		
		i = 2;
		while (i < n + 1) {
			isPrime[i] = 1; // test inline comment
			i = i + 1; // with trailing slashes / //
		}
		
		i = 2; /* test block comments // */
		while (i*i < n + 1) {
			if (isPrime[i] == 1) {
				j = i;
				while (i*j < n + 1) {
					isPrime[i*j] = 0;
					j = j + 1;
				}
			}
			i = i + 1;
		}
		
		refPrimes = new Int[8];
		refPrimes[0] = 2;
		refPrimes[1] = 3;
		refPrimes[2] = 5;
		refPrimes[3] = 7;
		refPrimes[4] = 11;
		refPrimes[5] = 13;
		refPrimes[6] = 17;
		refPrimes[7] = 19;
		
		rpIndex = 0;
		i = 2;
		while (i < n + 1) {
			if (isPrime[i] == 1) {
				if (i < 8) {
					programCorrect = programCorrect && (refPrimes[rpIndex] == i);
				}
				rpIndex = rpIndex + 1;
				println(i);
			}
			i = i + 1;
		}
 
        return programCorrect;
    }
}