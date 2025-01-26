fun main() {
    val (n, d) = readIntPair()
    val s = readLine()!!
    val result = s.reversed().fold(Pair(arrayListOf<Char>(), d)) { (str, count), c ->
        if (0 < count && c == '@') {
            val newStr = str.apply { add('.') }
            return@fold Pair(newStr, count - 1)
        } else {
            val newStr = str.apply { add(c) }
            return@fold Pair(newStr, count)
        }
    }.first.reversed().joinToString("")
    println(result)
}

fun readIntPair(): Pair<Int,Int> {
    val (a, b) = readLine()!!.split(" ").map { it.toInt() }
    return Pair(a, b)
}
