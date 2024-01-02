import * as fs from "fs";

type Position = {
  x: number;
  y: number;
};

type Schedule = {
  t: number;
} & Position;

function main() {
  const lines = IOUtil.readAllLines();
  const itemCount = Number(lines[0]);
  const schedules: Schedule[] = ["0 0 0"]
    .concat(lines.slice(1, itemCount + 1))
    .map((line) => {
      const [t, x, y] = line.split(/\s/).map(Number);
      return { t, x, y };
    });
  const sheduleDiff: (prev: Schedule, next: Schedule) => Schedule = (
    prev,
    next
  ) => {
    return {
      t: Math.abs(next.t - prev.t),
      x: Math.abs(next.x - prev.x),
      y: Math.abs(next.y - prev.y),
    };
  };

  let current = 0;
  const scheduleCount = schedules.length - 1; // 初期位置を除いた数が予定なので-1
  while (scheduleCount > current) {
    const diff = sheduleDiff(schedules[current], schedules[current + 1]);
    const needsMove = diff.x + diff.y; // 少なくとも必要な移動量
    // 最短で時間内に該当座標へ移動可能、余り時間が偶数なら調整可能(行って戻れる)
    if (needsMove <= diff.t && (needsMove - diff.t) % 2 === 0) {
      current++;
    } else {
      break;
    }
  }
  console.log(scheduleCount > current ? "No" : "Yes");
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
