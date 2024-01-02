// © 2024 Ishikawa-Taiki
import * as fs from "fs";

function main() {
  // template
  const lines = IOUtil.readAllLines().map(Number);
  const itemCount = lines[0] + 1;
  const sortedDiameters = lines.slice(1, itemCount);
  // Set利用で要素の重複を省く
  const result = new Set(sortedDiameters).size;
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
