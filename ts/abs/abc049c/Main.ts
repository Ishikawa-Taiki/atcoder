import * as fs from "fs";

function main() {
  const s = IOUtil.readAllLines()[0];
  // 貪欲法で解くのが良いらしいので書いてみる(反転した文字列で確認していく)
  let target = StringUtil.reverse(s);
  const reverseWords = ["dream", "dreamer", "erase", "eraser"].map(
    StringUtil.reverse
  );
  do {
    const searchResult = reverseWords.find((w) => target.startsWith(w));
    if (!searchResult) {
      break;
    }
    target = target.substring(searchResult.length);
  } while (target.length > 0);
  console.log(target.length === 0 ? "YES" : "NO");
}

// TLEが解消できないので書き直し
// function main() {
//   const s = IOUtil.readAllLines()[0];
//   const found = findMatchString(s, "");
//   console.log(found ? "YES" : "NO");
// }

// const WORDS = ["dream", "dreamer", "erase", "eraser"];
// // currentから始まるtargetの文字列を探し続ける
// const findMatchString = (target: string, current: string): boolean => {
//   if (target === current) {
//     return true;
//   }
//   if (target.length < current.length) {
//     return false;
//   }
//   return WORDS.reduce(
//     (acc, v) => acc || findMatchString(target, `${current}${v}`),
//     false
//   );
// };

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
