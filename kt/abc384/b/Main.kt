fun main() {
    val (n, r) = readIntPair()
    val xs = (1..n).map { _ -> readIntPair() }
    val result = xs.fold(r) { acc, (d, a) ->
        val isRated = when (d) {
            1 -> 1600 <= acc && acc <= 2799
            2 -> 1200 <= acc && acc <= 2399
            else -> false
        }
        if (isRated) (acc + a) else acc
    }
    println(result)
}

fun readIntPair(): Pair<Int,Int> {
    val (a, b) = readLine()!!.split(" ").map { it.toInt() }
    return Pair(a, b) // 2Tupleはこう返せば良い
}
