fun main() {
    val n = readLine()!!.toInt()
    val (b, c) = readLine()!!.split(" ").map { it.toInt() }
    val s = readLine()!!
    println(s)
}

fun runLengthEncoding(s: String): List<Pair<Char, Int>> {
    val result = mutableListOf<Pair<Char, Int>>()
    var count = 1
    for (i in 1 until s.length) {
        if (s[i] == s[i - 1]) {
            count++
        } else {
            result.add(Pair(s[i - 1], count))
            count = 1
        }
    }
    result.add(Pair(s.last(), count))
    return result
}
