fun main() {
    // 前処理： 標準入力のデータをメモリに読み込む
    IO.readData()

    // メイン処理部分
    val n = IO.getLineToInt()
    val a = IO.getLineToIntList()
    val result = solve(n, a)
    IO.printYesNo(result)

    // 後処理： 出力用バッファのデータを標準出力に書き込む
    IO.flush()
}

// 与えられた問題を解く純粋関数
fun solve(n: Int, a: List<Int>): Boolean {
    return false
}

// 標準入出力の制御用ユーティリティ
object IO {
    /** 標準入力の制御用(入力バッファ) */
    private val input = mutableListOf<String>()
    private var index = 0
    fun readData() = generateSequence { readLine() }.forEach { input.add(it) }

    /** 標準出力の制御用(出力バッファ) */
    private val output = StringBuilder()
    fun flush() = println(output.toString())

    /** 入力バッファからのデータの取得系処理 */
    private val readSafe: () -> String = { input.getOrNull(index++) ?: "" }
    private val stringToIntList = { s: String -> s.split(" ").map { it.toInt() } }
    fun getLineToString(): String = readSafe()
    fun getLineToInt(): Int = readSafe().toInt()
    fun getLineToIntList(): List<Int> = stringToIntList(readSafe())
    fun getContentsToStringList(): List<String> =
            input.subList(index, input.size).also { index = input.size }
    fun getContentsToIntMatrix(): List<List<Int>> =
            input.subList(index, input.size).map(stringToIntList).also { index = input.size }

    /** 出力バッファへのデータの記録系処理 */
    private fun writeLine(s: String) = output.appendLine(s)
    fun print(data: Any) = writeLine(data.toString())
    fun printYesNo(isYes: Boolean) = writeLine(Util.boolToYesNo(isYes))
    fun printListWithLn(array: List<Any>) = array.forEach { writeLine(it.toString()) }
    fun printListWithSpace(array: List<Any>) =
            writeLine(array.map { it.toString() }.joinToString(""))
}

// 便利関数群
object Util {
    fun reverse(s: String): String = s.reversed()
    fun range(from: Int, to: Int, step: Int = 1): List<Int> = (from..to step step).toList()
    fun repeat(n: Int, value: Any): List<Any> = List(n) { value }

    // 次の関数は Kotlinの標準ライブラリにあるため、コメントアウトして説明だけ残しておく。
    /*
    fun sum(array: List<Int>): Int = array.sum()
    fun product(array: List<Int>): Int = array.fold(1) { acc, i -> acc * i }
    fun max(array: List<Int>): Int = array.maxOrNull() ?: 0
    fun min(array: List<Int>): Int = array.minOrNull() ?: Int.MAX_VALUE
    fun sortAsc(array: List<Int>): List<Int> = array.sorted()
    fun sortDesc(array: List<Int>): List<Int> = array.sortedDescending()
    */

    fun <T> countIf(array: List<T>, predicate: (T) -> Boolean): Int = array.count(predicate)
    fun <T> countElements(array: List<T>): Map<T, Int> = array.groupingBy { it }.eachCount()
    fun <T> rle(array: List<T>): List<Pair<T, Int>> =
            array.fold(mutableListOf<Pair<T, Int>>()) { acc, cur ->
                if (acc.isEmpty() || acc.last().first != cur) acc.add(Pair(cur, 1))
                else acc[acc.size - 1] = Pair(acc.last().first, acc.last().second + 1)
                acc
            }

    fun binarySearch(ok: Int, ng: Int, check: (Int) -> Boolean): Pair<Int, Int> {
        var low = ok
        var high = ng
        while (high - low > 1) {
            val mid = (low + high) / 2
            if (check(mid)) low = mid else high = mid
        }
        return low to high
    }

    fun manhattanDistance(point1: Pair<Int, Int>, point2: Pair<Int, Int>): Int {
        val (x1, y1) = point1
        val (x2, y2) = point2
        return (x1 - x2).let { if (it < 0) -it else it } + (y1 - y2).let { if (it < 0) -it else it }
    }

    fun boolToYesNo(bool: Boolean): String = if (bool) "Yes" else "No"
    fun <T1, T2, T3> fst3(triple: Triple<T1, T2, T3>): T1 = triple.first
    fun <T1, T2, T3> snd3(triple: Triple<T1, T2, T3>): T2 = triple.second
    fun <T1, T2, T3> thd3(triple: Triple<T1, T2, T3>): T3 = triple.third
    fun <T> listToPair(list: List<T>): Pair<T, T> = list[0] to list[1]
    fun <T> listToTriple(list: List<T>): Triple<T, T, T> = Triple(list[0], list[1], list[2])
    fun <T> pairToList(pair: Pair<T, T>): List<T> = listOf(pair.first, pair.second)
    fun <T> tripleToList(triple: Triple<T, T, T>): List<T> =
            listOf(triple.first, triple.second, triple.third)
}
