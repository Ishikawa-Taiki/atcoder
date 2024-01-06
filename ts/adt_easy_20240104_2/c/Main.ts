// © 2024 Ishikawa-Taiki
import * as fs from "fs";

function main() {
  const lines = IOUtil.readAllLines();
  const [h, w] = lines[0].split(/\s/).map(Number);
  const matrix = lines.slice(1).map((l, i) => l.split(/\s/).map(Number));

  const newMatrix = matrix[0].map((_, i) => matrix.map((v) => v[i]));
  newMatrix.forEach((inner) => {
    console.log(inner.reduce((p, v) => p + String(v) + " ", ""));
  });
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
