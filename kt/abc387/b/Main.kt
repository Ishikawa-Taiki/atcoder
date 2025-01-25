fun main() {
    val x = readLine()!!.toInt()
    val result = (1..9).flatMap { i -> (1..9).map { j -> i * j } }.filter { it != x }.sum()
    println(result)
}
