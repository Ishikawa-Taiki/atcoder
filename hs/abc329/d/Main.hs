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

import Control.Monad (forM_)
import Control.Monad.ST (ST)
import Data.Array.Base (elems, readArray, writeArray)
import Data.Array.ST (MArray (newArray), STUArray, runSTUArray)
import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Debug.Trace (trace)

main :: IO ()
main = do
  (n, m) <- getLineToIntTuple2
  xs <- getLineToIntList
  printListWithLn $ solve xs n m

{-
問題概要
N人の候補者にM票の投票が行われている
1票づつ開けていくので、各時点で一番票数の多い候補者の番号を表示せよ
同数の時は若い番号を優先する

戦略
データを保持しながら畳み込めばできそうだが、ミュータブル配列で試してみる
候補者毎の合計票数の他に一番票数の多い候補者番号と票数を保持しながら、各段階での票数を調べる

-}

solve :: [Int] -> Int -> Int -> [Int]
solve xs n m = elems $
  runSTUArray $ do
    counts <- newArray (1, n) 0 :: ST s (STUArray s Int Int)
    results <- newArray (1, m) 0 :: ST s (STUArray s Int Int)
    bestRef <- newSTRef (1 :: Int)
    bestValue <- newSTRef (0 :: Int)

    forM_ (zip [1 ..] xs) \(i, nextIndex) -> do
      bestCount <- readSTRef bestValue
      bestIndex <- readSTRef bestRef
      nextCount <- succ <$> readArray counts nextIndex
      writeArray counts nextIndex nextCount -- 該当位置データをカウントアップ
      if bestCount == nextCount
        then do
          -- 同じ数値ならIndexの小さい方が採用
          if bestIndex < nextIndex
            then do
              writeArray results i bestIndex
              writeSTRef bestRef bestIndex
              writeSTRef bestValue bestCount
            else do
              writeArray results i nextIndex
              writeSTRef bestRef nextIndex
              writeSTRef bestValue nextCount
        else do
          -- 違う数値なら値を比較
          if bestCount > nextCount
            then do
              writeArray results i bestIndex
              writeSTRef bestRef bestIndex
              writeSTRef bestValue bestCount
            else do
              writeArray results i nextIndex
              writeSTRef bestRef nextIndex
              writeSTRef bestValue nextCount

    return results

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
