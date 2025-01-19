import * as fs from "fs";

function main() {
  const head = IO.getLineToIntList();
  const lines = IO.getContentsToIntMatrix();
  console.log(head);
  console.log(lines);
}

namespace IO {
  const input = fs.readFileSync("/dev/stdin", "utf8").trim().split("\n")
  export function getLineToString(): string {
    return input.shift()!;
  }
  export function getLineToInt(): number {
    return +input.shift()!;
  }
  export function getLineToIntList(): number[] {
    // TODO: Haskell踏襲で良い感じに内部関数を分ける
    return input.shift()!.split(" ").map(Number);
  }
  export function getContentsToStringList(): string[] {
    // TODO: inputを空にする
    return input;
  }
  export function getContentsToIntMatrix(): number[][] {
    // TODO: inputを空にする
    return input.map((s) => s.split(" ").map(Number))
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

main();
