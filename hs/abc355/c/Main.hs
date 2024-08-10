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

import Data.Array.Unboxed (IArray (bounds), Ix (range), UArray, listArray, (!), (//))
import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Debug.Trace (trace)

main :: IO ()
main = do
  (n, t) <- getLineToIntTuple2
  xs <- getLineToIntArray
  putStrLn $ maybe "-1" show (solve xs n t)

-- 縦横斜めの進捗状態(bingoまでの残数)を更新していく
-- いずれかが0になったら、その時点のターン数を記録にする
solve :: [Int] -> Int -> Int -> Maybe Int
solve xs n _ =
  let vCount = listArray @UArray (0, n -1) $ repeat n
      hCount = listArray @UArray (0, n -1) $ repeat n
      d1 = n
      d2 = n
      allBingoList = zip xs [1 ..] -- 進めるデータをターン進捗状態に紐づけてペアに管理しておく 畳み込む
      initialCount = Count vCount hCount d1 d2
   in fst $ foldl (acc n) (Nothing, initialCount) allBingoList
  where
    acc :: Int -> (Maybe Int, Count) -> (Int, Int) -> (Maybe Int, Count)
    acc n all@(Just _, status) _ = all
    acc n (Nothing, status) current@(currentItem, currentTurn) =
      let (isBingo, nextStatus) = update status n currentItem
          nextResult = bool Nothing (Just currentTurn) isBingo
       in (nextResult, nextStatus)

data Count = Count
  { v :: UArray Int Int,
    h :: UArray Int Int,
    d1 :: Int,
    d2 :: Int
  }
  deriving (Show)

update :: Count -> Int -> Int -> (Bool, Count)
update count n item =
  let itemBase = item -1 -- modもdivも3の倍数が先に桁上がりしてしまうので、0ベースで考えられるように値を一つずらす
      vIndex = (itemBase `mod` n)
      hIndex = (itemBase `div` n)
      vSum = (v count ! vIndex) - 1
      hSum = (h count ! hIndex) - 1
      currentD1 = d1 count
      currentD2 = d2 count
      d1Sum = bool currentD1 (currentD1 - 1) (vIndex == hIndex)
      d2Sum = bool currentD2 (currentD2 - 1) (vIndex == (n - (hIndex - 1)))
      isBingo = elem 0 [vSum, hSum, d1Sum, d2Sum]
      newCount = Count (v count // [(vIndex, vSum)]) (h count // [(hIndex, hSum)]) d1Sum d2Sum
   in (isBingo, newCount)

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
