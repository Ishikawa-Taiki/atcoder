{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Redundant flip" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas -Wno-incomplete-patterns -Wno-unused-imports -Wno-unused-top-binds -Wno-name-shadowing -Wno-unused-matches #-}

-- © 2024 Ishikawa-Taiki
module Main (main) where

import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.List (delete, elemIndices, foldl', group, nub, permutations, sort, tails)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Monoid (Sum (Sum, getSum))
import qualified Data.Set as S
import Debug.Trace (trace)

main :: IO ()
main = do
  (n, k) <- getLineToIntTuple2
  xs <- getLineToString
  print $ solve xs n k

-- 問題概略
--  文字列と数値が与えられる
--  各文字を並べ替えて得られる文字列パターンのうち、長さKの回文を含まない文字列は何種類あるか？を答える
-- 戦略
--  同じ文字が含まれている場合はどちらを使っても同じ文字列になるので、文字列数の最大としては全部違う文字の時になる
--  制約上の文字数が最大10であることから 10!(3628800) くらいの文字列数になる想定
--  10^6(1000000)台と結構大きい数字なので、各文字列に対するチェックは10^2(N^2)以内くらいには終わらせなければならなさそう
--  10文字全部違うときは回文を含み得ないので、特別扱いをすることで1桁分だけ猶予は出せそう
--  分けなくてもACは出たが、そちらの方が良さそうなので分岐版に書き換えておく
--
--  まずは各文字を並べ替えて得られる文字列パターンを一通り出したい
--  同じ文字から出来上がるものを別物扱いするとチェック数が増えるだけなので、文字列パターンを作る時点で省略しながら作る
--
--  部分文字列の回文判定は、元の文字列から考えられるK文字の文字列リストを作り、それぞれが回文であるかを確認する
--  先頭や末尾から、ずらしながらK文字ずつ取ったものになる
--  回文判定は反転させて一致するかで確認する
solve :: [Char] -> Int -> Int -> Int
solve xs n k =
  if n == (S.size . S.fromList $ xs)
    then factorial n
    else countIf (not . containsPalindromeNK n k) . uniquePermutations $ xs

-- 階乗を求める
factorial :: Int -> Int
factorial = product . flip take [1 ..]

-- リスト中の条件を満たす要素の数を返却する
countIf :: Eq a => (a -> Bool) -> [a] -> Int
countIf f = getSum . foldMap (bool (Sum 0) (Sum 1) . f)

--　　標準の permutations が微妙みたいなのを見かけるので、一旦シンプルそうな以下の実装を参考にさせていただく
-- https://atcoder.jp/contests/abc363/submissions/55938113
-- リストの要素を並び替えた全組み合わせを返却する(重複項目は除く)
uniquePermutations :: (Eq a, Show a) => [a] -> [[a]]
uniquePermutations [] = [[]]
uniquePermutations s = do
  x <- nub s -- リストモナドなので、xはnubが返す各要素に順に取り出す変数になる
  map (x :) . uniquePermutations $ delete x s -- 上記より、この行が要素数分実行される形になる deleteは最初に見つかったxを消すので組み合わせを作れる

-- 回文かどうかを返却する
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- 長さNの文字列中に長さKの回文が含まれているかを返却する(N>=K)
containsPalindromeNK :: Int -> Int -> String -> Bool
containsPalindromeNK n k = any (isPalindrome . take k) . take (n - k + 1) . tails -- K文字の部分文字列として考えられるものを列挙しながら、それらが回文であるかどうかを確認する

{- Library -}
-- データ変換共通
boolToYesNo :: Bool -> String
boolToYesNo = bool "No" "Yes"

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

arrayToTuple2 :: [a] -> (a, a)
arrayToTuple2 (a : b : _) = (a, b)

arrayToTuple3 :: [a] -> (a, a, a)
arrayToTuple3 (a : b : c : _) = (a, b, c)

tuple2ToArray :: (a, a) -> [a]
tuple2ToArray (a, b) = [a, b]

tuple3ToArray :: (a, a, a) -> [a]
tuple3ToArray (a, b, c) = [a, b, c]

bsToInt :: ByteString -> Int
bsToInt = fst . fromJust . BS.readInt

bsToIntList :: ByteString -> [Int]
bsToIntList = fmap bsToInt . BS.words

bsToIntTuple2 :: ByteString -> (Int, Int)
bsToIntTuple2 = arrayToTuple2 . bsToIntList

bsToIntTuple3 :: ByteString -> (Int, Int, Int)
bsToIntTuple3 = arrayToTuple3 . bsToIntList

bsToIntMatrix :: ByteString -> [[Int]]
bsToIntMatrix = fmap bsToIntList . BS.lines

bsToIntTuples2 :: ByteString -> [(Int, Int)]
bsToIntTuples2 = fmap (arrayToTuple2 . bsToIntList) . BS.lines

bsToIntTuples3 :: ByteString -> [(Int, Int, Int)]
bsToIntTuples3 = fmap (arrayToTuple3 . bsToIntList) . BS.lines

bsToInteger :: ByteString -> Integer
bsToInteger = fst . fromJust . BS.readInteger

bsToIntegerList :: ByteString -> [Integer]
bsToIntegerList = fmap bsToInteger . BS.words

-- IO 出力系
printYesNo :: Bool -> IO ()
printYesNo = putStrLn . boolToYesNo

printArrayWithSpace :: (Show a) => [a] -> IO ()
printArrayWithSpace = putStrLn . unwords . fmap show

printArrayWithLn :: (Show a) => [a] -> IO ()
printArrayWithLn = putStr . unlines . fmap show

printMatrix :: (Show a) => [[a]] -> IO ()
printMatrix mtx = putStr . unlines $ unwords . fmap show <$> mtx

-- IO 入力系
getLineToString :: IO String
getLineToString = BS.unpack <$> BS.getLine

getLineToInt :: IO Int
getLineToInt = bsToInt <$> BS.getLine

getLineToIntArray :: IO [Int]
getLineToIntArray = bsToIntList <$> BS.getLine

getLineToIntTuple2 :: IO (Int, Int)
getLineToIntTuple2 = bsToIntTuple2 <$> BS.getLine

getLineToIntTuple3 :: IO (Int, Int, Int)
getLineToIntTuple3 = bsToIntTuple3 <$> BS.getLine

getLineToInteger :: IO Integer
getLineToInteger = bsToInteger <$> BS.getLine

getLineToIntegerArray :: IO [Integer]
getLineToIntegerArray = bsToIntegerList <$> BS.getLine

getContentsToStringArray :: IO [String]
getContentsToStringArray = fmap BS.unpack . BS.lines <$> BS.getContents

getContentsToIntMatrix :: IO [[Int]]
getContentsToIntMatrix = bsToIntMatrix <$> BS.getContents

getContentsToIntTuples2 :: IO [(Int, Int)]
getContentsToIntTuples2 = bsToIntTuples2 <$> BS.getContents

getContentsToIntTuples3 :: IO [(Int, Int, Int)]
getContentsToIntTuples3 = bsToIntTuples3 <$> BS.getContents

-- デバッグ用
#ifndef ATCODER

debugProxy :: (Show a) => a -> a
debugProxy value =
  let !_ = debug "[DebugProxy]" value
   in value

debug :: (Show a) => String -> a -> ()
debug key value = trace (key ++ " : " ++ show value) ()

#else

debugProxy :: (Show a) => a -> a
debugProxy = id

debug :: (Show a) => String -> a -> ()
debug _ _ = ()

#endif
