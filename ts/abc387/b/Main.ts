import * as fs from "fs";

function main() {
  const x = IO.getLineToInt();
  const value = range(1, 9).flatMap((i) => range(1, 9).map((j) => i * j)).filter((v) => v !== x).reduce((acc, v) => acc + v, 0)
  console.log(value);
}

namespace IO {
  // TODO: Haskell踏襲で良い感じに内部関数を分ける
  // TODO: 構造を見直す(入出力だけで時間かかりすぎかもしれない)
  const input = fs.readFileSync("/dev/stdin", "utf8").trim().split("\n")
  let index = 0;
  export function getLineToString(): string {
    return input[index++];
  }
  export function getLineToInt(): number {
    return +input[index++];
  }
  export function getLineToIntList(): number[] {
    return input[index++].split(" ").map(Number);
  }
  export function getContentsToStringList(): string[] {
    const result = input.slice(index)
    index = input.length
    return result;
  }
  export function getContentsToIntMatrix(): number[][] {
    const result = input.slice(index).map((s) => s.split(" ").map(Number));
    index = input.length
    return result;
  }
  export function printYesNo(p: boolean): void {
    console.log(p ? "Yes" : "No");
  }
}

namespace StringUtil {
  /**
   * 文字列を反転する
   * @returns 反転した文字列
   */
  export function reverse(s: string): string {
    return s.split("").reverse().join("");
  }
}

export const range =
  (start: number, end: number) => Array.from({ length: (end - start + 1) }, (v, k) => k + start);

main();
