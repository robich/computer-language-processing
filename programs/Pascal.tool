object Pascal {
	def main() : Unit = {
		println(new PascalTriangle().printTriangle(10));     
	}
}

class PascalTriangle {
    def printTriangle(rows : Int) : String = {
        var row : Int[];
		var k : Int[];
		var i : Int;
		var j : Int;
		var triangle : String;
		row = new Int[1];
		row[0] = 1;
		i = 1;
		triangle = "";
		println(1);
		while (i < rows+1){
			k = new Int[i+1];
			k[0] = row[0];
			j = 1;
			while(j < i){
				k[j] = row[j-1] + row[j];
				j = j + 1;
			}
			k[i] = row[0];
			row = k;
			println(this.rowToString(k));
			i = i + 1;
		}
		return "Done!";
	}
	
	def rowToString(row : Int[]) : String = {
		var i : Int;
		var output : String;
		i = 0;
		output = "";
		while(i < row.length){
			output = output + row[i] + " ";
			i = i+1;
		}
		return output;
	}
}