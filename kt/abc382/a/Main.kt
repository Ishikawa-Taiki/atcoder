fun main() {
    val (n,d) = readIntPair()
    val s = readLine()!!
    println(n - (s.filter { it == '@' }.length - d))
}

fun readIntPair(): Pair<Int,Int> {
    val (a,b) = readLine()!!.split(" ").map { it.toInt() }
    return Pair(a,b)
}
