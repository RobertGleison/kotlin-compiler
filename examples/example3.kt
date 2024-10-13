// Example 3: Conditional expressions and while loop
fun main() {
    var count = 0
    while (count < 5) {
        if (count % 2 == 0) {
            print("Even: $count ")
        } else {
            print("Odd: $count ")
        }
        count += 1
    }
}