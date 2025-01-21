import * as fs from "fs";

function main() {
  const head = IO.getLineToIntList()
  const elems = head.reduce((m, v) => m.set(v, m.get(v) ? m.get(v) + 1 : 1), new Map()).values()
  const ptn = Array.from(elems).sort((a, b) => a - b);
  IO.printYesNo(ptn.length === 2 && ((ptn[0] === 1 && ptn[1] === 3) || (ptn[0] === 2 && ptn[1] === 2)));
}

namespace IO {
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
  export const reverse = (s: string) => s.split("").reverse().join("");
  export const range =
    (from: number, to: number) => Array.from({ length: (to - from + 1) }, (_, i) => from + i);
}

main();
IO.flush()
