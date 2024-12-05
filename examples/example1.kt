fun main() {
    var x: Int = 42;
    var teste: Boolean = false;
    var y: Int = 10;
    var z = readln();
   
    if (x > y) {
        var scopeTest: Int = 10;
        return x;
    } else {
        return y;
    }
    scopeTest = 10;
}
