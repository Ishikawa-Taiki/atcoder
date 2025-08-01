{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Redundant flip" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas -Wno-incomplete-patterns -Wno-unused-imports -Wno-unused-top-binds -Wno-name-shadowing -Wno-unused-matches #-}

import Data.Bifunctor (Bifunctor (first, second))
import Data.Bool (bool)
import Data.Char (intToDigit)
import Data.Fixed (Fixed, HasResolution (resolution), showFixed)
import Data.List (delete, elemIndex, group, nub, sort, tails, transpose)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Monoid (Sum (Sum, getSum))
import Data.Ord (Down (..), comparing)
import Data.Ratio (Ratio, (%))
import Data.Set (fromList, toList)
import Data.Set qualified as S
import GHC.Float (int2Float)
import Numeric (showIntAtBase)

{- Library -}
-- 便利関数系
-- 繰り上げ除算
ceilDiv :: (Integral a) => a -> a -> a
ceilDiv x y = (x + pred y) `div` y

-- 切り捨て除算
floorDiv :: (Integral a) => a -> a -> a
floorDiv = div

-- 階乗を求める
factorial :: Int -> Int
factorial = product . flip take [1 ..]

-- 素数判定
isPrime :: Int -> Bool
isPrime n
  | n <= 2 = True
  | otherwise =
    let max = ceiling . sqrt $ int2Float n
     in null [i | i <- [2, 3 .. max], n `mod` i == 0]

-- 2タプルを昇順にソートする
sortTuple2 :: (Ord a) => (a, a) -> (a, a)
sortTuple2 (x, y) = bool (y, x) (x, y) $ x <= y

-- エラトステネスの篩 での素数リスト取得
-- https://ja.wikipedia.org/wiki/エラトステネスの篩
-- 確定した素数リストと未確定リストを受け取り、素数リストを返す
eratosthenes :: [Int] -> [Int] -> [Int]
eratosthenes primes [] = sort primes
eratosthenes primes (x : xs) = eratosthenes (x : primes) [v | v <- xs, v `mod` x /= 0]

-- 約数列挙
enumerateDivisor :: Int -> [Int]
enumerateDivisor n = do
  let max = ceiling . sqrt $ int2Float n
  toList . fromList $ concat [[x, y] | x <- [1 .. max], n `mod` x == 0, let y = n `div` x]

-- 2進数文字列を10進数に変換する
binary2ToInteger :: String -> Integer
binary2ToInteger xs = foldl (\x y -> 2 * x + y) 0 $ (\x -> read [x]) <$> xs

-- 数値xをbase進数文字列にする
intToDigitString :: Int -> Integer -> String
intToDigitString base x = showIntAtBase base intToDigit (fromIntegral x) ""

-- nCr は 組み合わせ (combination)　の計算
-- n個からr個選ぶ場合の組み合わせの数を求めるときに利用する
nCr :: Int -> Int -> Int
nCr n r =
  let numerator = product $ take r [n, n - 1 ..]
      denominator = product $ take r [1 ..]
   in numerator `div` denominator

-- n個から2個選ぶ場合の組み合わせの数を求める
-- nCr の頻繁に利用するケースとして、効率よく計算するために個別で用意しておく
nC2 :: (Integral a) => a -> a
nC2 n
  | n < 2 = 0
  | otherwise = n * (n - 1) `div` 2

-- 精度の高い少数型定義( :: TypeE100 みたいに使う)
-- https://hackage.haskell.org/package/base-4.14.3.0/docs/Data-Fixed.html#t:Pico
data E100 = E100

instance HasResolution E100 where
  resolution _ =
    10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000

type TypeE100 = Fixed E100

-- 回文かどうかを返却する
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- 長さNの文字列中に長さKの回文が含まれているかを返却する(N>=K)　※ nは文字列から求められるので消したい気もする
containsPalindromeNK :: Int -> Int -> String -> Bool
containsPalindromeNK n k = any (isPalindrome . take k) . take (n - k + 1) . tails -- K文字の部分文字列として考えられるものを列挙しながら、それらが回文であるかどうかを確認する

-- 二次元平面で回転を行った時の座標値を取得する(近似値)
-- x軸が右向き/y軸が上向きの xy 座標平面において、反時計回りに theta 度回転させたxy座標を得る
rotate :: Int -> (Double, Double) -> (Double, Double)
rotate theta (srcX, srcY) =
  let sinTheta = sin (fromIntegral theta * (pi / 180.0))
      cosTheta = cos (fromIntegral theta * (pi / 180.0))
      dstX = (srcX * cosTheta) - (srcY * sinTheta)
      dstY = (srcX * sinTheta) + (srcY * cosTheta)
   in (dstX, dstY)

-- 二次元平面上の2点間のユークリッド距離を計算する(近似値)
distanceTwoPoints :: (Double, Double) -> (Double, Double) -> Double
distanceTwoPoints (x1, y1) (x2, y2) =
  let distanceX = abs (x2 - x1)
      distanceY = abs (y2 - y1)
   in sqrt (distanceX ^ 2 + distanceY ^ 2)

-- 二次元平面上の2点間のユークリッド距離がD以内かどうかを返す
distanceTwoPointsInD :: Int -> (Int, Int) -> (Int, Int) -> Bool
distanceTwoPointsInD d (y1, x1) (y2, x2) =
  let yDiff = (y1 - y2)
      xDiff = (x1 - x2)
      distance = (yDiff ^ 2) + (xDiff ^ 2)
   in distance <= d ^ 2

-- 二次元平面上の2点間のマンハッタン距離を計算する
manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
manhattanDistance (y1, x1) (y2, x2) = abs (x1 - x2) + abs (y1 - y2)

-- 二次元平面上の2点間のチェビシェフ距離(チェス盤距離)を計算する
chebyshevDistance :: (Int, Int) -> (Int, Int) -> Int
chebyshevDistance (y1, x1) (y2, x2) =
  let x = abs (x2 - x1)
      y = abs (y2 - y1)
   in max x y

-- 二次元平面上の特定の座標から指定のマンハッタン距離「以内」である座標リストを得る
manhattanPoints :: Int -> (Int, Int) -> [(Int, Int)]
manhattanPoints distance (y, x) =
  [ (y + i, x + j)
    | i <- [- distance .. distance],
      let absI = abs i,
      let absJ = distance - absI,
      j <- [- absJ .. absJ]
  ]

-- 二次元マトリクスを時計回りに90度回転させた二次元マトリクスを得る
rotRight90 :: [[a]] -> [[a]]
rotRight90 = transpose . reverse

-- 二次元マトリクスを反時計回りに90度回転させた二次元マトリクスを得る
rotLeft90 :: [[a]] -> [[a]]
rotLeft90 = reverse . transpose

-- h行二次元マトリクス全ての要素を縦方向にn回シフトした二次元マトリクスを得る
vShift :: Int -> Int -> [[a]] -> [[a]]
vShift h n xxs = take h . drop n . concat $ repeat xxs

-- w列二次元マトリクス全ての要素を横方向にn回シフトした二次元マトリクスを得る
hShift :: Int -> Int -> [[a]] -> [[a]]
hShift w n xxs = take w . drop n . concat . repeat <$> xxs

-- 与えられた方向に対し、二次元マトリクス上を移動する
move :: Char -> (Int, Int) -> (Int, Int)
move 'U' = first pred
move 'D' = first succ
move 'R' = second succ
move 'L' = second pred
move _ = id

-- 座標圧縮(リストから重複を取り除いた上で、値が小さい順に [0,1..]　としてマッピングする)
compressPoints :: (Ord a) => [a] -> [Int]
compressPoints xs = (indexMap M.!) <$> xs
  where
    indexMap = M.fromList $ flip zip [0 ..] $ S.toList . S.fromList $ xs

-- 2つのパターン(AorB)それぞれの比率を有理数として返す
ratios :: Integral a => (a, a) -> (Ratio a, Ratio a)
ratios (a, b) = let total = a + b in (a % total, b % total)

-- タプルのソート条件の述語(第一要素昇順、第二要素降順)
compareAscFirstDescSecond2 :: (Ord a, Ord b) => (a, b) -> (a, b) -> Ordering
compareAscFirstDescSecond2 = comparing fst <> comparing (Down . snd)

-- 論理包含/含意(ならば)
implication :: Bool -> Bool -> Bool
implication a b = not a || b

-- キー毎のカウンター
type CounterMap k = M.Map k Int

-- リストの各要素を数える
countElements :: (Ord k) => [k] -> CounterMap k
countElements = M.fromListWith (+) . map (,1)

-- キーの値をインクリメントする(キーがなければ1で追加)
increment :: (Ord k) => k -> CounterMap k -> CounterMap k
increment = flip (M.insertWith (+)) 1

-- キーの値をデクリメントする(0になる場合はキーを削除)
decrement :: (Ord k) => k -> CounterMap k -> CounterMap k
decrement = M.update (\c -> if c <= 1 then Nothing else Just (pred c))

-- リスト中の条件を満たす要素の数を返却する
countIf :: (Eq a) => (a -> Bool) -> [a] -> Int
countIf f = getSum . foldMap (bool (Sum 0) (Sum 1) . f)

-- リストの指定インデックスのデータを指定の値に書き換える
replaceAt :: [a] -> (Int, a) -> [a]
replaceAt [] _ = []
replaceAt (_ : xs) (0, y) = y : xs
replaceAt (x : xs) (n, y) = x : xs `replaceAt` (n - 1, y)

-- Arrayで言うところの(//)相当のことがしたかったため、一旦同じ形で用意しておく
(//) :: [a] -> (Int, a) -> [a]
(//) = replaceAt

-- デリミタを基準に、１つのリストを複数のリストへ分割する
splitList :: (Eq a) => a -> [a] -> [[a]]
splitList delimiter source = checkOneItem delimiter source []
  where
    checkOneItem :: (Eq a) => a -> [a] -> [a] -> [[a]]
    checkOneItem delimiter [] tmp = [tmp]
    checkOneItem delimiter (x : xs) tmp
      | x == delimiter = tmp : checkOneItem delimiter xs []
      | otherwise = checkOneItem delimiter xs (tmp ++ [x])

-- リストをn個ずつの要素数のリストに分解する
chunksOfList :: Int -> [a] -> [[a]]
chunksOfList n [] = []
chunksOfList n xs = as : chunksOfList n bs
  where
    (as, bs) = splitAt n xs

-- 三角数を求める(1からnまでの整数の和を計算する)
triangularNumber :: (Integral a) => a -> a
triangularNumber n = n * (n + 1) `div` 2

-- 連続した数の総和を求める(sum [from..to])相当の値を返却する
consecutiveNumbersSum :: Integer -> Integer -> Integer
consecutiveNumbersSum from to
  | from == to = from
  | otherwise =
    let sumValue = from + to
        count = (to - from) + 1
     in (sumValue * count) `div` 2

-- 等差数列の和を求める(初項init　/　末項last　/　項数count　に対する等差数列の和を返す)
sumOfArithmeticProgressions :: Integer -> Integer -> Integer -> Integer
sumOfArithmeticProgressions init last count = (init + last) * count `div` 2

-- リストの要素を並び替えた全組み合わせを返却する(重複項目は除く) ※ 標準の permutations が微妙らしい(?)
uniquePermutations :: (Eq a, Show a) => [a] -> [[a]]
uniquePermutations [] = [[]]
uniquePermutations s = do
  x <- nub s -- リストモナドなので、xはnubが返す各要素に順に取り出す変数になる
  map (x :) . uniquePermutations $ delete x s -- 上記より、この行が要素数分実行される形になる deleteは最初に見つかったxを消すので組み合わせを作れる

-- ランレングス圧縮する(リスト上の連続したデータを、データ一つ+連続した長さのリストに変換する)
rle :: (Eq a) => [a] -> [(a, Int)]
rle = map (\x -> (head x, length x)) . group

-- ランレングス圧縮されているものをリストに戻す
rld :: (Eq a) => [(a, Int)] -> [a]
rld = concatMap (\(x, len) -> replicate len x)

-- 関数fをn回適用する関数を得る
repeatF :: (b -> b) -> Int -> b -> b
repeatF f n = foldr (.) id (replicate n f)

-- 二分探索
-- 値が有効化どうかを確認する関数と、現在のOK/NG範囲を受け取り、最終的なOK/NG範囲を返却する
-- (ok, ng は見に行かないので、両端が確定しない場合は1つ外側を指定すると良い)
binarySearch :: (Int -> Bool) -> (Int, Int) -> (Int, Int)
binarySearch check (ok, ng)
  | abs (ng - ok) == 1 = (ok, ng)
  | otherwise =
    let mid = (ok + ng) `div` 2
     in if check mid
          then binarySearch check (mid, ng)
          else binarySearch check (ok, mid)

-- 同じ要素が再登場しない最大の区間の長さを求める
lengthOfLongestUniqueSublist :: (Ord a) => [a] -> Int
lengthOfLongestUniqueSublist xs = go xs S.empty 0 0
  where
    go [] _ _ maxLength = maxLength
    go (y : ys) seen currentLength maxLength
      | y `S.member` seen = go ys (S.delete (head xs) seen) (currentLength - 1) maxLength
      | otherwise = go ys (S.insert y seen) (currentLength + 1) (max maxLength (currentLength + 1))

-- 任意の順序に基づいて転倒数を求める関数: 計算量O(N log N)
countInversionsWithOrder :: (Ord a) => [a] -> [a] -> Int
countInversionsWithOrder order xs = fst (mergeSortAndCount order xs)
  where
    -- マージソートを利用して転倒数を数える
    mergeSortAndCount :: (Ord a) => [a] -> [a] -> (Int, [a])
    mergeSortAndCount _ [] = (0, [])
    mergeSortAndCount _ [x] = (0, [x])
    mergeSortAndCount order xs = (leftCount + rightCount + splitCount, merged)
      where
        (left, right) = splitAt (length xs `div` 2) xs
        (leftCount, sortedLeft) = mergeSortAndCount order left
        (rightCount, sortedRight) = mergeSortAndCount order right
        (splitCount, merged) = mergeAndCount order sortedLeft sortedRight

    -- マージしながら転倒数を数える
    mergeAndCount :: (Ord a) => [a] -> [a] -> [a] -> (Int, [a])
    mergeAndCount _ xs [] = (0, xs)
    mergeAndCount _ [] ys = (0, ys)
    mergeAndCount order (x : xs) (y : ys)
      | index x <= index y =
        let (count, merged) = mergeAndCount order xs (y : ys)
         in (count, x : merged)
      | otherwise =
        let (count, merged) = mergeAndCount order (x : xs) ys
         in (count + length (x : xs), y : merged)
      where
        index a = fromJust (elemIndex a order)
