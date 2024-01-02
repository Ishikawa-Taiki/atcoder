import * as fs from "fs";

function main() {
  // template
  const [n, y] = IOUtil.readAllLines()[0].split(/\s/).map(Number).slice(0, 2);
  let found = false;
  // 前から数えると遅くて終わらない
  // search: for (let i10000 = 0; i10000 <= n; i10000++) {
  //   for (let i5000 = 0; i5000 <= n - i10000; i5000++) {
  //     for (let i1000 = 0; i1000 <= n - i10000 - i5000; i1000++) {
  //       const sum = i10000 * 10000 + i5000 * 5000 + i1000 * 1000; // 合計金額
  //       const count = i10000 + i5000 + i1000; // お札の枚数
  //       if (sum === y && count === n) {
  //         console.log("found!");
  //         found = true;
  //         break search;
  //       }
  //     }
  //   }
  // }

  search: for (let i10000 = n; i10000 >= 0; i10000--) {
    for (let i5000 = n - i10000; i5000 >= 0; i5000--) {
      const i1000 = n - i10000 - i5000;
      const sum = i10000 * 10000 + i5000 * 5000 + i1000 * 1000; // 合計金額
      const count = i10000 + i5000 + i1000; // お札の枚数
      if (sum === y && count === n) {
        console.log(`${i10000} ${i5000} ${i1000}`);
        found = true;
        break search;
      }
    }
  }
  if (!found) {
    console.log(`${-1} ${-1} ${-1}`);
  }
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
