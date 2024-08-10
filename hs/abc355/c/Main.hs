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

{-
問題概略:
 左上1から順番に番号が振られたN行N列の　Bingoシートと、進行として番号のリストが与えられる
 一番最初にBingoを達成するターン数を、最後まで達成しないならその旨を報告する
戦略:
 Bingoシートの行列数もターン数も結構大きいので、毎ターン全ての状況を確認するのは重くなりそう
 一回の進行でBingo達成の可能性があるのは縦横両斜めの4方向だけなので、残を基準にするのが良さそう
 それぞれの残初期値をNにしておいて、ターン進行毎に残数を減らしながら完了確認をする方法を取る
-}

solve :: [Int] -> Int -> Int -> Maybe Int
solve xs n _ =
  let vCount = listArray @UArray (0, n -1) $ repeat n
      hCount = listArray @UArray (0, n -1) $ repeat n
      d1 = n
      d2 = n
      stepList = zip xs [1 ..] -- 進めるデータとターン数のペア
      initialCount = RemainingCount vCount hCount d1 d2
   in fst $ foldl (acc n) (Nothing, initialCount) stepList
  where
    acc :: Int -> BingoStatus -> BingoStep -> BingoStatus
    acc n all@(Just _, _) _ = all
    acc n (Nothing, count) current@(currentItem, currentTurn) =
      let (isBingo, nextCount) = update count n currentItem
          nextResult = bool Nothing (Just currentTurn) isBingo
       in (nextResult, nextCount)

-- Bingoシートの各方向に対する残(埋まっていない)マス数
data RemainingCount = RemainingCount
  { v :: UArray Int Int, -- 列方向　の残リスト(0-n)
    h :: UArray Int Int, -- 行方向　の残リスト(0-n)
    d1 :: Int, -- 左上〜右下方向
    d2 :: Int -- 右上〜左下方向
  }
  deriving (Show)

-- Bingo状況(最初に揃ったターン数, 揃うまでの残)
type BingoStatus = (Maybe Int, RemainingCount)

-- Bingo進行1Step(進めるもの, 現在のターン数)
type BingoStep = (Int, Int)

update :: RemainingCount -> Int -> Int -> (Bool, RemainingCount)
update count n item =
  let itemBase = item -1 -- modもdivもnの倍数が先に桁上がりしてしまうので、0ベースで考えられるように値を一つずらす

      {- 今回の列行方向の更新 -}
      vIndex = (itemBase `mod` n)
      hIndex = (itemBase `div` n)
      vSum = (v count ! vIndex) - 1
      hSum = (h count ! hIndex) - 1
      {- 斜め方向については、対角線上のマスだった場合のみ残数を減らす -}
      currentD1 = d1 count
      currentD2 = d2 count
      d1Sum = bool currentD1 (currentD1 - 1) (vIndex == hIndex)
      d2Sum = bool currentD2 (currentD2 - 1) (vIndex == (n - (hIndex - 1)))
      {- どれか一つでも揃ったならBingo達成となる(今回更新した部分以外の行列は見てもしょうがないので省略する) -}
      isBingo = elem 0 [vSum, hSum, d1Sum, d2Sum]
      newCount = RemainingCount (v count // [(vIndex, vSum)]) (h count // [(hIndex, hSum)]) d1Sum d2Sum
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
