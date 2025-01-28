fun main() {
    // 前処理： 標準入力のデータをメモリに読み込む
    IO.readData()

    // メイン処理部分
    val s = IO.getLineToString()
    val result = solve(s)
    IO.printListWithSpace(result)

    // 後処理： 出力用バッファのデータを標準出力に書き込む
    IO.flush()
}

// 与えられた問題を解く純粋関数
fun solve(s: String): List<String> {
    val ca = s.toCharArray()
    val a = ca[0]
    val b = ca[1]
    val c = ca[2]
    return listOf("${b}${c}${a}", "${c}${a}${b}")
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

    /** 出力バッファへのデータの記録系処理 */
    private fun writeLine(s: String) = output.appendLine(s)
    fun print(data: Any) = writeLine(data.toString())
    fun printListWithSpace(array: List<Any>) =
            writeLine(array.map { it.toString() }.joinToString(" "))
}
