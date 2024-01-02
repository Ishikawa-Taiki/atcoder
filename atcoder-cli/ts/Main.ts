import * as fs from "fs";

function main() {
  const lines = IOUtil.readAllLines();
  console.log(lines);
}

export module IOUtil {
  /**
   * 全ての標準入力を読み取って返却する
   * @returns 行毎の配列
   */
  export function readAllLines() {
    return fs.readFileSync("/dev/stdin", "utf8").trim().split("\n");
  }
}

export module StringUtil {
  export function reverse(s: string): string {
    return s.split("").reverse().join("");
  }
}

main();
