// © 2024 Ishikawa-Taiki
import * as fs from "fs";

function main() {
  // template
  const lines = IOUtil.readAllLines();

  const aMax = Number(lines[0]); // 500
  const aList = [...Array(aMax + 1)].map((_, i) => i * 500);

  const bMax = Number(lines[1]); // 100
  const bList = [...Array(bMax + 1)].map((_, i) => i * 100);

  const cMax = Number(lines[2]); // 50
  const cList = [...Array(cMax + 1)].map((_, i) => i * 50);

  const x = Number(lines[3]); // Sum
  let count = 0;

  aList.forEach((a) => {
    bList.forEach((b) => {
      cList.forEach((c) => {
        if (a + b + c === x) {
          // console.log(" a: " + a + " b: " + b + " c: " + c);
          count++;
        }
      });
    });
  });

  console.log(count);
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
