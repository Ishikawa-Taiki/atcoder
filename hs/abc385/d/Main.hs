{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Redundant flip" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas -Wno-incomplete-patterns -Wno-unused-imports -Wno-unused-top-binds -Wno-name-shadowing -Wno-unused-matches #-}

module Main (main) where

import Control.Monad
import Control.Monad (forM_, replicateM, unless, when)
import Control.Monad.Fix
import Control.Monad.Fix (fix)
import Control.Monad.ST
import Data.Array.ST
import Data.Array.ST (runSTUArray)
import Data.Array.Unboxed (Array, IArray (bounds), Ix (range), UArray, accumArray, listArray, (!), (//))
import Data.Bifunctor (Bifunctor (second), bimap, first, second)
import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char (digitToInt, intToDigit, isLower, isUpper, toLower, toUpper)
import Data.Foldable (maximumBy)
import Data.List
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid (Sum (..))
import Data.STRef
import Data.STRef (modifySTRef, newSTRef, readSTRef, writeSTRef)
import qualified Data.Set as S
import Data.Tuple (swap)
import Debug.Trace (trace)

main :: IO ()
main = do
  (n : m : sx : sy : _) <- getLineToIntegerList
  xys <- replicateM (fromInteger n) $ do
    [x, y] <- getLineToIntegerList
    pure (x, y)
  dcs <- replicateM (fromInteger m) $ do
    ((d : _) : c : _) <- fmap words getLineToString
    pure (d, read @Integer c)
  print $ solve xys dcs n m sx sy

solve :: [(Integer, Integer)] -> [(Char, Integer)] -> Integer -> Integer -> Integer -> Integer -> Int
solve xys dcs n m sx sy = result
  where
    result = undefined
    mx = M.map sortF $ M.fromListWith (++) $ second (: []) <$> xys
    my = M.map sortF $ M.fromListWith (++) $ second (: []) . swap <$> xys
    sortF list =
      let sorted = sort list
          len = length sorted
       in (len, listArray @Array (1, len) sorted)
    calc = foldl f ((sx, sy), S.empty) dcs
    f ((x, y), s) op = ((x', y'), s')
      where
        (x', y') = move op (x, y)
        visited =
          if x == x'
            then inRangeHouse (sortF y y') $ M.findWithDefault (0, listArray (1, 0) []) x my
            else inRangeHouse (sortF y y') $ M.findWithDefault (0, listArray (1, 0) []) y mx
        sortF a b = listToTuple2 $ sort [a, b]
        rangeF a b = (\(i, j) -> [i .. j]) $ sortF a b
        s' = S.union s (S.fromList visited)
        inRangeHouse (from, to) (len, list) = [] -- TODO

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

-- 与えられた方向に対し、二次元マトリクス上を移動する
move :: (Char, Integer) -> (Integer, Integer) -> (Integer, Integer)
move ('R', n) = first (+ n)
move ('L', n) = first (subtract n)
move ('U', n) = second (+ n)
move ('D', n) = second (subtract n)
move _ = id

-- solve :: [(Integer, Integer)] -> [(Char, Integer)] -> Integer -> Integer -> Integer -> Integer -> Int
-- solve xys dcs n m sx sy = result
--   where
--     houses = S.fromList xys
--     result = S.size . snd $ foldl f ((sx, sy), S.empty) dcs
--     f ((x, y), s) op = ((x', y'), s')
--       where
--         (x', y') = move op (x, y)
--         visited = S.fromList [(i, j) | i <- rangeF x x', j <- rangeF y y']
--         rangeF a b = [min a b .. max a b]
--         s' = S.union s visited

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
foldlDebugProxy :: (Show a, Show b) => (a -> b -> a) -> a -> b -> a
foldlDebugProxy f p1 p2 =
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
