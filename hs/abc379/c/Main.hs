{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Redundant flip" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas -Wno-incomplete-patterns -Wno-unused-imports -Wno-unused-top-binds -Wno-name-shadowing -Wno-unused-matches #-}

-- © 2024 Ishikawa-Taiki
module Main (main) where

import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.List (sort)
import Data.Maybe (fromJust)
import Debug.Trace (trace)

main :: IO ()
main = do
  (n : m : _) <- getLineToIntegerList
  xs <- getLineToIntegerList
  as <- getLineToIntegerList
  print $ solve xs as n m

{-
問題概要：
一列に並んだN個のマスがあり、石が何個か入ったマスがいくつか存在している
石を一つ後ろのマスにずらす操作を繰り返すことで、石がちょうど一個ずつ入っている状態を作り出したい
操作することが可能な場合は操作回数を、不可能な場合は-1を出力せよ

戦略：
石のないマスまで愚直に操作をすると計算量が多くなってしまうので、石が置かれているマスのリストを基準に考えたい
石のあるマスから次の石のあるマスまで石を1個ずつ残す場合、何回の操作が必要かとどれだけの石を持ち越すかを考慮しながら処理を繰り返すこととする
以下に考慮する
・石が置かれているマスのリストは昇順に並んでるとは限らないので、ソートが必要
・最終的に石の数がちょうど1個ずつ出なければならないので、道中に足りないところがあったり最後の石が2個以上余ってたらNG
畳み込みを行いやすくするため、最後に番兵としてデータを追加して処理をする

-}

solve :: [Integer] -> [Integer] -> Integer -> Integer -> Integer
solve xs as n m =
  let (p : ps) = sort $ zip (xs ++ [succ n]) (as ++ [1])
      result = foldl f (p, 0, fst p == 1) ps
   in if thd3 result && snd (fst3 result) == 1 && sum as == n
        then snd3 result
        else -1

-- 前回までの石の入ったマス情報累計、操作回数合計、継続中か
type Stone = (Integer, Integer)

type R = (Stone, Integer, Bool)

f :: R -> Stone -> R
f ((x1, a1), operationCount, continue) (x2, a2) =
  let needs = x2 - x1
      remainingStoneCount = a1 - needs
      currentOperation = fastSum (pred needs) + needs * remainingStoneCount
   in ((x2, a2 + remainingStoneCount), operationCount + currentOperation, continue && (remainingStoneCount >= 0))

-- 等差数列の和
fastSum :: Integer -> Integer
fastSum n = n * (n + 1) `div` 2

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
