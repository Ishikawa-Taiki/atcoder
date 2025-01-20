import * as fs from "fs";

function main() {
  const head = IO.getLineToIntList();
  const snakes = IO.getContentsToIntMatrix();
  const n = head[0]
  const d = head[1]
  Util.range(1, d).map((k) => {
    return [...snakes].map((s) => s[0] * (s[1] + k)).sort()[n - 1]
  }).forEach((v) => console.log(v))
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
