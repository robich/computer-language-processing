object Prime {
	def main() : Unit = {
		println(new PrimeList().computePrimes(10));     
	}
}

class PrimeList {
	//uses a dumb algorithm to return a list of prime numbers
    def computePrimes(max_numbers : Int) : String = {
        var number : Int;
		var counter : Int;
		var i : Int;
		var isPrime : Bool;
		var numList : String;
		numList = "";
		counter = 0;
		number = 2;
		while (counter < max_numbers) {
			i = 2;
			isPrime = true;
			while (i < number) {
				if(this.mod(number, i) == 0){
					isPrime = false;
				}
				i = i+1;
			}
			if(isPrime) {
				numList = numList + " " + number;
				counter = counter + 1;
			}
			number = number + 1;
		}
		return numList;
	}
	
	def mod(m : Int, n : Int) : Int = {
        return m - (n * (m / n));
    }
}