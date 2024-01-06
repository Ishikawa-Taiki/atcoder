// © 2024 Ishikawa-Taiki
import * as fs from "fs";

function main() {
  const [a, b] = IOUtil.readAllLines()[0].split(/\s/).map(Number).sort();
  // const result = b == a * 2 || b == a * 2 + 1 ? "Yes" : "No";
  const result = a == Math.floor(b / 2) ? "Yes" : "No";
  console.log(result);
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
  /**
   * 文字列を反転する
   * @returns 反転した文字列
   */
  export function reverse(s: string): string {
    return s.split("").reverse().join("");
  }
}

main();
