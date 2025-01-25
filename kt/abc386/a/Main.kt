fun main() {
    val xs = readLine()!!.split(" ").map { it.toInt() }.toSet()
    println(if (xs.size == 2) "Yes" else "No")
}
