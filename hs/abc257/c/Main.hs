{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Redundant flip" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas -Wno-incomplete-patterns -Wno-unused-imports -Wno-unused-top-binds -Wno-name-shadowing -Wno-unused-matches #-}

module Main (main) where

import Control.Monad (forM_, replicateM, unless, when)
import Control.Monad.Fix (fix)
import Data.Array.Unboxed (Array, IArray (bounds), Ix (range), UArray, accumArray, listArray, (!), (//))
import Data.Bifunctor (bimap, first, second)
import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char (digitToInt, intToDigit, isLower, isUpper, toLower, toUpper)
import Data.List
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid (Sum (..))
import Data.STRef (modifySTRef, newSTRef, readSTRef, writeSTRef)
import qualified Data.Set as S
import Data.Tuple (swap)
import Debug.Trace (trace)

main :: IO ()
main = do
  n <- getLineToInt
  s <- getLineToString
  ws <- getLineToIntList
  print $ solve ws s n

{-
問題概要
大人or子供と体重の情報の人リストが与えられる
体重を基準に大人/子供を判定しようとする場合、最大で何人を正しく判定できるか

戦略
リストを大人と子供に分け、それぞれソートしておく
大人の値を基準に少しずつ最小値から大きくしていった時、何人の子供が正しく判定できるようになっていくかを求めると良さそう
候補の数値は大人の値自体(ぎりぎり入る)と、値+1(ぎりぎり外れる)を見ていく
体重が同じ人が複数いる可能性を考えると、RLEしておけば値の種類と人数を切り離して考えやすい？
(そうすると、何人増えるか/何人減るかを高速に求めるために、累積の人数が必要そう？)

-}

solve :: [Int] -> String -> Int -> Int
solve ws s n = result
  where
    f = rle . sort . map fst
    !(adult, child) = debugProxy "a,c" $ bimap f f $ partition ((== '1') . snd) $ zip ws s
    result = undefined

-- runLengthEncoding / ランレングス圧縮(リスト上の連続したデータを、データ一つ+連続した長さのリストに変換する)
rle :: (Eq a) => [a] -> [(a, Int)]
rle = map (\x -> (head x, length x)) . group

-- 二分探索
-- 値が有効化どうかを確認する関数と、現在のOK/NG範囲を受け取り、最終的なOK/NG範囲を返却する
-- (ok, ng は見に行かないので、両端が確定しない場合は1つ外側を指定すると良さそう？)
binarySearch :: (Int -> Bool) -> (Int, Int) -> (Int, Int)
binarySearch check (ok, ng)
  | abs (ng - ok) == 1 = (ok, ng)
  | otherwise =
    let mid = (ok + ng) `div` 2
     in if check mid
          then binarySearch check (mid, ng)
          else binarySearch check (ok, mid)

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

listToTuple2 :: [a] -> (a, a)
listToTuple2 (a : b : _) = (a, b)

listToTuple3 :: [a] -> (a, a, a)
listToTuple3 (a : b : c : _) = (a, b, c)

tuple2ToList :: (a, a) -> [a]
tuple2ToList (a, b) = [a, b]

tuple3ToList :: (a, a, a) -> [a]
tuple3ToList (a, b, c) = [a, b, c]

bsToInt :: ByteString -> Int
bsToInt = fst . fromJust . BS.readInt

bsToIntList :: ByteString -> [Int]
bsToIntList = fmap bsToInt . BS.words

bsToIntTuple2 :: ByteString -> (Int, Int)
bsToIntTuple2 = listToTuple2 . bsToIntList

bsToIntTuple3 :: ByteString -> (Int, Int, Int)
bsToIntTuple3 = listToTuple3 . bsToIntList

bsToIntMatrix :: ByteString -> [[Int]]
bsToIntMatrix = fmap bsToIntList . BS.lines

bsToIntTuples2 :: ByteString -> [(Int, Int)]
bsToIntTuples2 = fmap (listToTuple2 . bsToIntList) . BS.lines

bsToIntTuples3 :: ByteString -> [(Int, Int, Int)]
bsToIntTuples3 = fmap (listToTuple3 . bsToIntList) . BS.lines

bsToInteger :: ByteString -> Integer
bsToInteger = fst . fromJust . BS.readInteger

bsToIntegerList :: ByteString -> [Integer]
bsToIntegerList = fmap bsToInteger . BS.words

-- IO 出力系
printYesNo :: Bool -> IO ()
printYesNo = putStrLn . boolToYesNo

printListWithSpace :: (Show a) => [a] -> IO ()
printListWithSpace = putStrLn . unwords . fmap show

printListWithLn :: (Show a) => [a] -> IO ()
printListWithLn = putStr . unlines . fmap show

printMatrix :: (Show a) => [[a]] -> IO ()
printMatrix mtx = putStr . unlines $ unwords . fmap show <$> mtx

-- IO 入力系
getLineToString :: IO String
getLineToString = BS.unpack <$> BS.getLine

getLineToInt :: IO Int
getLineToInt = bsToInt <$> BS.getLine

getLineToIntList :: IO [Int]
getLineToIntList = bsToIntList <$> BS.getLine

getLineToIntTuple2 :: IO (Int, Int)
getLineToIntTuple2 = bsToIntTuple2 <$> BS.getLine

getLineToIntTuple3 :: IO (Int, Int, Int)
getLineToIntTuple3 = bsToIntTuple3 <$> BS.getLine

getLineToInteger :: IO Integer
getLineToInteger = bsToInteger <$> BS.getLine

getLineToIntegerList :: IO [Integer]
getLineToIntegerList = bsToIntegerList <$> BS.getLine

getContentsToStringList :: IO [String]
getContentsToStringList = fmap BS.unpack . BS.lines <$> BS.getContents

getContentsToIntMatrix :: IO [[Int]]
getContentsToIntMatrix = bsToIntMatrix <$> BS.getContents

getContentsToIntTuples2 :: IO [(Int, Int)]
getContentsToIntTuples2 = bsToIntTuples2 <$> BS.getContents

getContentsToIntTuples3 :: IO [(Int, Int, Int)]
getContentsToIntTuples3 = bsToIntTuples3 <$> BS.getContents

-- デバッグ用
#ifndef ATCODER

debugProxy :: (Show a) => String -> a -> a
debugProxy tag value =
  let !_ = debug tag value
   in value

debug :: (Show a) => String -> a -> ()
debug key value = trace (key ++ " : " ++ show value) ()

#else

debugProxy :: (Show a) => String -> a -> a
debugProxy _ = id

debug :: (Show a) => String -> a -> ()
debug _ _ = ()

#endif
