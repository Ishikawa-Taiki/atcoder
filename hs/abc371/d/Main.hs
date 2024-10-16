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

import Data.Array (Array)
import Data.Array.IArray (listArray, (!))
import Data.Array.Unboxed (UArray)
import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Debug.Trace (trace)

main :: IO ()
main = do
  n <- getLineToInt
  xs <- getLineToIntList
  ps <- getLineToIntegerList
  q <- getLineToInt
  lr <- getContentsToIntTuples2
  printListWithLn $ solve xs n ps lr

{-
問題概要
村の座標と村人のリストが与えられる
クエリLRのリストが与えられるので、座標L以上R以下の村人の総数を答えよ

戦略
区間の村人総数は累積和を使えば短時間で求められそう
村の数とクエリの数的に、LRが具体的にどの村なのかを愚直に操作するとTLEするので、探し方を工夫する必要がある
ある座標以下の村は二分探索で高速に求めることができるので、R側はその座標までの人数なのでそのままの値になる
L側はその座標の手前までの人数が知りたいため、L-1の位置以下の村の人数を利用すれば良い

-}

solve :: [Int] -> Int -> [Integer] -> [(Int, Int)] -> [Integer]
solve xs n ps lr =
  let v = listArray @UArray (1, n) xs -- village
      p = listArray @Array (1, n) $ scanl1 (+) ps -- people
   in convert p v <$> lr
  where
    f v = calcLT v (0, succ n)
    convert p v (l, r) =
      let li = f v (pred l)
          lv = bool (p ! li) 0 $ li <= 0
          ri = f v r
          rv = bool (p ! ri) 0 $ ri <= 0
       in rv - lv

calcLT :: UArray Int Int -> (Int, Int) -> Int -> Int
calcLT v range target = fst $ binarySearch (\i -> v ! i <= target) range

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
