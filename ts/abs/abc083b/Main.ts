import * as fs from "fs";

function main() {
  // template
  const numbers = IOUtil.readAllLines()[0].split(/\s/).map(Number);
  const n = numbers[0];
  const a = numbers[1];
  const b = numbers[2];
  const sum = (x: number, y: number) => x + y;
  const result = [...Array(n + 1)]
    .map((_, i) => i)
    .filter((item) => {
      // 文字列を経由して毎桁ごとに分解して、値の和を計算する
      const sumValue = item.toString().split("").map(Number).reduce(sum, 0);
      return a <= sumValue && sumValue <= b;
    })
    .reduce(sum, 0);
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

main();
