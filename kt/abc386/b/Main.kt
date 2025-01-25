fun main() {
    val s = readLine()!!
    val result =
            runLengthEncoding(s)
                    .map { (key, value) -> if (key == '0') (value / 2 + value % 2) else value }
                    .sum()
    println(result)
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
