import * as fs from "fs";

function main() {
  // template
  const lines = IOUtil.readAllLines();
  const itemCount = Number(lines[0]);
  let numbers = lines[1].split(/\s/).map(Number).slice(0, itemCount);
  const sortedCard = numbers.sort((a, b) => b - a);
  const isAlice = (index: number) => index % 2 === 0;
  const sum = (x: number, y: number) => x + y;
  const aliceScore = numbers.filter((_, i) => isAlice(i)).reduce(sum, 0);
  const bobsScore = numbers.filter((_, i) => !isAlice(i)).reduce(sum, 0);
  console.log(aliceScore - bobsScore);
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
