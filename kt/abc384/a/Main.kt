fun main() {
    val (n, c1, c2) = readLine()!!.split(" ")
    val c1c = c1.get(0) // 文字列型から指定位置の文字型の値をとる
    val c2c = c2.get(0)
    val s = readLine()!!
    println(s.map { c -> if (c != c1c) c2c else c }.joinToString(""))
}
