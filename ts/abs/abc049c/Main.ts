import * as fs from "fs";

function main() {
  const s = IOUtil.readAllLines()[0];
  const found = findMatchString(s, "").length !== 0;
  console.log(found ? "YES" : "NO");
}

// currentから始まるtargetの文字列を探し続け、見つかる限り候補の配列を返す
// 一致したらtargetと同等の文字列を返し、一致しなくなったら空配列になる
const findMatchString = (target: string, current: string): string[] => {
  if (target === current) {
    return [current];
  }
  return ["dream", "dreamer", "erase", "eraser"]
    .filter((word) => target.startsWith(`${current}${word}`))
    .map((word) => findMatchString(target, `${current}${word}`))
    .flat();
};

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
