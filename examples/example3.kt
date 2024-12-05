// Example 3: Conditional expressions and while loop
fun main() {
    var count: Int = 0
    while (count < 5) {
        if (count % 2 == 0) {
            print("Even: count ")
        } else {
            print("Odd: count ")
        }
        count = count + 1
    }
}