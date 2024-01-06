// © 2024 Ishikawa-Taiki
import * as fs from "fs";

function main() {
  const [a, b, c] = IOUtil.readAllLines()[0].split(/\s/).map(Number);
  const maxCount = Math.floor(1000 / c);
  const candidate = [...Array(maxCount)].map((_, i) => c * (i + 1));
  const result = candidate.filter((item) => a <= item && item <= b)[0] ?? -1;
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
