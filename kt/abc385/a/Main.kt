fun main() {
    val (a, b, c) = readLine()!!.split(" ").map { it.toInt() }.sorted()
    println(if ((a == b && b == c) || (a + b == c)) "Yes" else "No")
}
