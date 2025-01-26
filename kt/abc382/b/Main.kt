fun main() {
    val (n, d) = readIntPair()
    val s = readLine()!!
    val result = s.reversed().fold(Pair(arrayListOf<Char>(), d)) { (str, count), c ->
        if (0 < count && c == '@') (Pair(str.apply { add('.') }, count - 1)) else (Pair(str.apply { add(c) }, count))
    }.first.reversed().joinToString("")
    println(result)
}

fun readIntPair(): Pair<Int,Int> {
    val (a, b) = readLine()!!.split(" ").map { it.toInt() }
    return Pair(a, b)
}
