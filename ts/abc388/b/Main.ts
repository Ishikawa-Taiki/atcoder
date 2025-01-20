import * as fs from "fs";

function main() {
  const head = IO.getLineToIntList();
  const snakes = IO.getContentsToIntMatrix();
  const n = head[0]
  const d = head[1]
  const result = Util.range(1, d).map((k) =>
    snakes.map((s) => s[0] * (s[1] + k)).sort((a, b) => a - b)[n - 1]
  )
  IO.printListWithLn(result)
}

namespace IO {
  // TODO: Haskell踏襲で良い感じに内部関数を分ける
  // TODO: 構造を見直す(入出力だけで時間かかりすぎかもしれない)
  // IO 入力系
  const input = fs.readFileSync("/dev/stdin", "utf8").trim().split("\n")
  let index = 0;
  const readLine = () => input[index++]
  const stringToIntList = (s: string) => s.split(" ").map(Number)
  export const getLineToString = readLine
  export const getLineToInt = () => Number(readLine())
  export const getLineToIntList = () => stringToIntList(readLine());
  export const getContentsToStringList = () => {
    const result = input.slice(index)
    index = input.length
    return result;
  }
  export const getContentsToIntMatrix = () => {
    const result = input.slice(index).map(stringToIntList);
    index = input.length
    return result;
  }
  // IO 出力系
  const logBuffer: string[] = [];
  const writeLine = (s: string) => logBuffer.push(s)
  export const printYesNo = (isYes: boolean) => writeLine(isYes ? "Yes" : "No")
  export const printListWithLn = (array: Array<any>) => array.forEach((v) => writeLine(v))
  export const flush = () => console.log(logBuffer.join('\n'))
}

namespace Util {
  /**
   * 文字列を反転する
   * @returns 反転した文字列
   */
  export function reverse(s: string): string {
    return s.split("").reverse().join("");
  }
  export const range =
    (start: number, end: number) => Array.from({ length: (end - start + 1) }, (v, k) => k + start);
}

main();
IO.flush();
