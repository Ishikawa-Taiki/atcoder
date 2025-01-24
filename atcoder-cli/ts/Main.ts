import * as fs from "fs";

function main() {
  const n = IO.getLineToInt();
  const xs = IO.getLineToIntList();
  IO.print(n);
  IO.printListWithLn(xs);
}

namespace IO {
  // IO 入力系
  const input = fs.readFileSync("/dev/stdin", "utf8").trim().split("\n")
  let index = 0;
  const readLine = () => input[index++]
  const stringToIntList = (s: string) => s.split(" ").map(Number)
  export const getLineToString = readLine
  export const getLineToInt = () => Number(readLine())
  export const getLineToIntList = () => stringToIntList(readLine());
  export const getContentsToStringList = () => {
    const result = input.slice(index)
    index = input.length
    return result;
  }
  export const getContentsToIntMatrix = () => {
    const result = input.slice(index).map(stringToIntList);
    index = input.length
    return result;
  }
  // IO 出力系
  const logBuffer: string[] = [];
  const writeLine = (s: string) => logBuffer.push(s)
  export const print = (data: any) => writeLine(data.toString())
  export const printYesNo = (isYes: boolean) => writeLine(Util.boolToYesNo(isYes))
  export const printListWithLn = (array: Array<any>) => array.forEach((v) => writeLine(v))
  export const flush = () => console.log(logBuffer.join('\n'))
}

namespace Util {
  /**
   * 文字列を反転する
   * @returns 反転した文字列
   */
  export const reverse = (s: string) => s.split("").reverse().join("");
  export const range = (from: number, to: number, step: number = 1) =>
    Array.from(
      { length: Math.floor((to - from) / step) + 1 },
      (_, i) => from + i * step
    );
  export const repeat = (n: number, value: any) => Array.from({ length: n }).fill(value);
  export const sum = (array: number[]) => array.reduce((m, v) => m + v, 0);
  export const product = (array: number[]) => array.reduce((m, v) => m * v, 1);
  export const max = (array: number[]) => array.reduce((m, v) => Math.max(m, v), 0);
  export const min = (array: number[]) => array.reduce((m, v) => Math.min(m, v), Infinity);
  export const sortAsc = (array: number[]) => array.slice().sort((a, b) => a - b);
  export const sortDesc = (array: number[]) => array.slice().sort((a, b) => b - a);
  export const countIf = (array: Array<any>, predicate: (v: any) => boolean) => array.filter(predicate).length;
  export const countElements = <T>(array: Array<T>): Map<T, number> => array.reduce((m, v) => m.set(v, m.get(v) ? m.get(v)! + 1 : 1), new Map<T, number>());
  export const rle = <T>(array: Array<T>): Array<[T, number]> => {
    if (array.length === 0) return [];
    return array.slice(1).reduce((acc, cur) => {
      const last = acc[acc.length - 1];
      if (last[0] === cur) {
        last[1]++;
      } else {
        acc.push([cur, 1]);
      }
      return acc;
    }, [[array[0], 1]] as Array<[T, number]>);
  }
  export const binarySearch = ([ok, ng]: [number, number], check: (mid: number) => boolean): [number, number] => {
    const mid = (ok + ng) >> 1;
    return (Math.abs(ng - ok) === 1) ?
      [ok, ng] :
      check(mid) ?
        Util.binarySearch([mid, ng], check) :
        Util.binarySearch([ok, mid], check);
  }
  export const manhattanDistance = (point1: [number, number], point2: [number, number]): number => {
    const [y1, x1] = point1;
    const [y2, x2] = point2;
    return Math.abs(x1 - x2) + Math.abs(y1 - y2);
  }
  // データ変換共通
  export const boolToYesNo = (bool: boolean): string => (bool ? "Yes" : "No");
  export const fst3 = <T1, T2, T3>([a, ,]: [T1, T2, T3]): T1 => a;
  export const snd3 = <T1, T2, T3>([, b,]: [T1, T2, T3]): T2 => b;
  export const thd3 = <T1, T2, T3>([, , c]: [T1, T2, T3]): T3 => c;
  export const listToTuple2 = <T>([a, b]: T[]): [T, T] => [a, b];
  export const listToTuple3 = <T>([a, b, c]: T[]): [T, T, T] => [a, b, c];
  export const tuple2ToList = <T>([a, b]: [T, T]): T[] => [a, b];
  export const tuple3ToList = <T>([a, b, c]: [T, T, T]): T[] => [a, b, c];
}

main();
IO.flush()
