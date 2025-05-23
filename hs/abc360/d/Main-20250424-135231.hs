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
  (n, t) <- listToTuple2 <$> getLineToIntegerList
  s <- getLineToString
  xs <- getLineToIntegerList
  print $ solve xs s t

{-
問題概要
数直線上に並んだそれぞれの蟻の位置と方向のリスト、時間Tが与えられる
時間T+1の方向に進む時、すれ違う蟻のペアの数はいくつになるか

戦略
全ての蟻の速度は同じで同じ方向に向いている蟻同士がすれ違うことはない
どちらかの方向の蟻だけ2倍進めて、すれ違う数を求めれば良い
すれ違う数については、移動後の蟻のリストとそれをソートしたものの転倒数を求めれば良い

-}

solve :: [Integer] -> String -> Integer -> Int
solve xs s t = result
  where
    moved = zipWith f xs s
    sorted = sort moved
    f x '0' = x
    f x '1' = x + t * 2 + 1
    result = countInversionsWithOrder sorted moved

-- -- 任意の順序に基づいて転倒数を求める関数: 計算量O(N log N)
-- countInversionsWithOrder :: (Ord a) => [a] -> [a] -> Int
-- countInversionsWithOrder order xs = fst (mergeSortAndCount order xs)
--   where
--     -- マージソートを利用して転倒数を数える
--     mergeSortAndCount :: (Ord a) => [a] -> [a] -> (Int, [a])
--     mergeSortAndCount _ [] = (0, [])
--     mergeSortAndCount _ [x] = (0, [x])
--     mergeSortAndCount order xs = (leftCount + rightCount + splitCount, merged)
--       where
--         (left, right) = splitAt (length xs `div` 2) xs
--         (leftCount, sortedLeft) = mergeSortAndCount order left
--         (rightCount, sortedRight) = mergeSortAndCount order right
--         (splitCount, merged) = mergeAndCount order sortedLeft sortedRight
--     -- マージしながら転倒数を数える
--     mergeAndCount :: (Ord a) => [a] -> [a] -> [a] -> (Int, [a])
--     mergeAndCount _ xs [] = (0, xs)
--     mergeAndCount _ [] ys = (0, ys)
--     mergeAndCount order (x : xs) (y : ys)
--       | index x <= index y =
--         let (count, merged) = mergeAndCount order xs (y : ys)
--          in (count, x : merged)
--       | otherwise =
--         let (count, merged) = mergeAndCount order (x : xs) ys
--          in (count + length (x : xs), y : merged)
--       where
--         index a = fromJust (elemIndex a order)

-- 任意の順序に基づいて転倒数を求める関数: 計算量O(N log N)
countInversionsWithOrder :: (Ord a) => [a] -> [a] -> Int
countInversionsWithOrder order xs = fst (mergeSortAndCount orderMap xs)
  where
    orderMap = M.fromList (zip order [0 ..])
    -- マージソートを利用して転倒数を数える
    mergeSortAndCount _ [] = (0, [])
    mergeSortAndCount _ [x] = (0, [x])
    mergeSortAndCount orderMap xs = (leftCount + rightCount + splitCount, merged)
      where
        (left, right) = splitAt (length xs `div` 2) xs
        (leftCount, sortedLeft) = mergeSortAndCount orderMap left
        (rightCount, sortedRight) = mergeSortAndCount orderMap right
        (splitCount, merged) = mergeAndCount orderMap sortedLeft sortedRight
    -- マージしながら転倒数を数える
    mergeAndCount _ xs [] = (0, xs)
    mergeAndCount _ [] ys = (0, ys)
    mergeAndCount orderMap (x : xs) (y : ys)
      | orderMap M.! x <= orderMap M.! y =
          let (count, merged) = mergeAndCount orderMap xs (y : ys)
           in (count, x : merged)
      | otherwise =
          let (count, merged) = mergeAndCount orderMap (x : xs) ys
           in (count + length (x : xs), y : merged)

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
foldDebugProxy :: (Show a, Show b) => (a -> b -> a) -> a -> b -> a
foldDebugProxy f p1 p2 =
  let r = f p1 p2
      !_ = debug "before, param, result" (p1, p2, r)
   in r

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
