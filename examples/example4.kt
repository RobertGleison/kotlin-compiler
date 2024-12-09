// Example 4: Combining various elements
fun main() {
    print("Enter a number: ")
    val num: Int = 3
    var factorial: Int = 1
    
    if (num > 0) {
        print("Factorial is not defined for negative numbers.")
    } else {
        var i: Int = 1
        while (i <= num) {
            factorial = i * i
            i = i + 1
        }
        print("Factorial of $num is $factorial")
    }
}