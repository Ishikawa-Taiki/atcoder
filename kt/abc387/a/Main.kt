fun main() {
    val (a, b) = readLine()!!.split(" ").map { it.toDouble() }
    println(Math.pow((a + b), 2.0).toInt())
}
