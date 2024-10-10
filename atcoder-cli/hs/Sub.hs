{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Redundant flip" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas -Wno-incomplete-patterns -Wno-unused-imports -Wno-unused-top-binds -Wno-name-shadowing -Wno-unused-matches #-}

-- © 2024 Ishikawa-Taiki

import Data.Bool (bool)
import Data.Char (intToDigit)
import Data.Fixed (Fixed, HasResolution (resolution), showFixed)
import Data.List (delete, group, nub, sort, tails, transpose)
import qualified Data.Map.Strict as M
import Data.Monoid (Sum (Sum, getSum))
import Data.Set (fromList, toList)
import GHC.Float (int2Float)
import Numeric (showIntAtBase)

{- Library -}
-- 便利関数系
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

-- 約数列挙
enumerateDivisor :: Int -> [Int]
enumerateDivisor n = do
  let max = ceiling . sqrt $ int2Float n
  toList . fromList $ concat [[x, y] | x <- [1 .. max], n `mod` x == 0, let y = n `div` x]

-- 2進数文字列を10進数に変換する
binary2ToInteger :: String -> Integer
binary2ToInteger xs = foldl (\x y -> 2 * x + y) 0 $ (\x -> read [x]) <$> xs

-- 数値xをbase進数文字列にする
intToDigitString :: Int -> Int -> String
intToDigitString base x = showIntAtBase base intToDigit x ""

-- nCr は 組み合わせ (combination)　の計算
-- n個からr個選ぶ場合の組み合わせの数を求めるときに利用する
nCr :: Int -> Int -> Int
nCr n r =
  let numerator = product $ take r [n, n - 1 ..]
      denominator = product $ take r [1 ..]
   in numerator `div` denominator

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

-- 二次元平面上の2点間の距離を計算する(近似値)
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

-- 二次元マトリクスを反時計回りに90度回転させた二次元マトリクスを得る
rot90 :: [[a]] -> [[a]]
rot90 = reverse . transpose

-- h行二次元マトリクス全ての要素を縦方向にn回シフトした二次元マトリクスを得る
vShift :: Int -> Int -> [[a]] -> [[a]]
vShift h n xxs = take h . drop n . concat $ repeat xxs

-- w列二次元マトリクス全ての要素を横方向にn回シフトした二次元マトリクスを得る
hShift :: Int -> Int -> [[a]] -> [[a]]
hShift w n xxs = take w . drop n . concat . repeat <$> xxs

-- 論理包含/含意(ならば)
implication :: Bool -> Bool -> Bool
implication a b = not a || b

-- リストの各要素を数える
countElements :: (Ord a) => [a] -> M.Map a Int
countElements = M.fromList . map count . group . sort
  where
    count xs = (head xs, length xs)

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

-- 連続した数の総和を求める(sum [from..to])相当の値を返却する
consecutiveNumbersSum :: Integer -> Integer -> Integer
consecutiveNumbersSum from to
  | from == to = from
  | otherwise =
      let sumValue = from + to
          count = (to - from) + 1
       in (sumValue * count) `div` 2

-- リストの要素を並び替えた全組み合わせを返却する(重複項目は除く) ※ 標準の permutations が微妙らしい(?)
uniquePermutations :: (Eq a, Show a) => [a] -> [[a]]
uniquePermutations [] = [[]]
uniquePermutations s = do
  x <- nub s -- リストモナドなので、xはnubが返す各要素に順に取り出す変数になる
  map (x :) . uniquePermutations $ delete x s -- 上記より、この行が要素数分実行される形になる deleteは最初に見つかったxを消すので組み合わせを作れる

-- runLengthEncoding / ランレングス圧縮(リスト上の連続したデータを、データ一つ+連続した長さのリストに変換する)
rle :: (Eq a) => [a] -> [(a, Int)]
rle = map (\x -> (head x, length x)) . group
