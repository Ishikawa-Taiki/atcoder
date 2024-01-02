// © 2024 Ishikawa-Taiki
import * as fs from "fs";

function main() {
  // template
  // const [a, b] = nextNums(2);
  // println(a*b%2 === 0 ? `Even`:`Odd`);
  const lines = IOUtil.readAllLines();
  const itemCount = Number(lines[0]);
  let numbers = lines[1].split(/\s/).map(Number).slice(0, itemCount);
  let operationCount = 0;
  while (numbers.every((n) => n % 2 === 0)) {
    numbers = numbers.map((n) => n / 2);
    operationCount++;
  }
  console.log(operationCount);
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
