{-# LANGUAGE BangPatterns #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas -Wno-incomplete-patterns -Wno-unused-imports -Wno-unused-top-binds -Wno-name-shadowing #-}

-- © 2024 Ishikawa-Taiki
module Main (main) where

import Control.Monad.Trans.State (State, evalState, runState, state)
import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char (isSpace)
import Data.List (sort, sortBy, transpose)
import qualified Data.List as L
import Data.Maybe (fromJust)
import Data.Set (fromList, toList)
import Debug.Trace (trace)
import GHC.Float (int2Float)

main :: IO ()
main = do
  xs <- getLineToIntArray
  a <- getLineToIntArray
  b <- getLineToIntArray
  printArrayWithLn $ solve xs $ zip3 [1 ..] a b

solve :: [Int] -> [(Int, Int, Int)] -> [Int]
solve (_ : x : y : z : _) record = sort $ fst3 <$> evalState (betterStudents x y z) record

-- 受験番号(index)、数学の点数、英語の点数
type RecordData = (Int, Int, Int)

-- それぞれの条件(数学,英語,合計点)の合格人数と受験生レコード情報を受け取り、合格者のリスト値と残候補者のリスト状態を返す
betterStudents :: Int -> Int -> Int -> State [RecordData] [RecordData]
betterStudents x y z = do
  math <- filterMath x
  english <- filterEnglish y
  total <- filterTotal z
  return (math ++ english ++ total)

-- 受験生レコードから、数学の得点が高い人X人を合格にする(合格者を返しつつ、受験生レコードから取り除く)
-- 得点が同点であった場合は受験生の番号の小さい方を優先とする
filterMath :: Int -> State [RecordData] [RecordData]
filterMath x = state $ \record -> splitAt x $ sortBy compareMath record

compareMath :: RecordData -> RecordData -> Ordering
compareMath (aNo, aMath, _) (bNo, bMath, _) =
  if aMath /= bMath
    then compare bMath aMath
    else compare aNo bNo

-- 受験生レコードから、英語の得点が高い人Y人を合格にする(合格者を返しつつ、受験生レコードから取り除く)
-- 得点が同点であった場合は受験生の番号の小さい方を優先とする
filterEnglish :: Int -> State [RecordData] [RecordData]
filterEnglish y = state $ \record -> splitAt y $ sortBy compareEnglish record

compareEnglish :: RecordData -> RecordData -> Ordering
compareEnglish (aNo, _, aEnglish) (bNo, _, bEnglish) =
  if aEnglish /= bEnglish
    then compare bEnglish aEnglish
    else compare aNo bNo

-- 受験生レコードから、数学と英語の合計得点が高い人Y人を合格にする(合格者を返しつつ、受験生レコードから取り除く)
-- 得点が同点であった場合は受験生の番号の小さい方を優先とする
filterTotal :: Int -> State [RecordData] [RecordData]
filterTotal z = state $ \record -> splitAt z $ sortBy compareTotal record

compareTotal :: RecordData -> RecordData -> Ordering
compareTotal (aNo, aMath, aEnglish) (bNo, bMath, bEnglish) =
  let aTotal = aMath + aEnglish
      bTotal = bMath + bEnglish
   in if aTotal /= bTotal
        then compare bTotal aTotal
        else compare aNo bNo

{- Library -}
-- データ変換共通
boolToYesNo :: Bool -> String
boolToYesNo = bool "No" "Yes"

fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a

snd3 :: (a, b, c) -> b
snd3 (a, b, c) = b

thd3 :: (a, b, c) -> c
thd3 (a, b, c) = c

arrayToTuple2 :: [a] -> (a, a)
arrayToTuple2 (a : b : _) = (a, b)

arrayToTuple3 :: [a] -> (a, a, a)
arrayToTuple3 (a : b : c : _) = (a, b, c)

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

-- IO 出力系
printYesNo :: Bool -> IO ()
printYesNo = putStrLn . boolToYesNo

printArrayWithSpace :: (Show a) => [a] -> IO ()
printArrayWithSpace = putStrLn . unwords . fmap show

printArrayWithLn :: (Show a) => [a] -> IO ()
printArrayWithLn = putStr . unlines . fmap show

-- IO 入力系
getLineToInt :: IO Int
getLineToInt = bsToInt <$> BS.getLine

getLineToIntArray :: IO [Int]
getLineToIntArray = bsToIntList <$> BS.getLine

getLineToIntTuple2 :: IO (Int, Int)
getLineToIntTuple2 = bsToIntTuple2 <$> BS.getLine

getLineToIntTuple3 :: IO (Int, Int, Int)
getLineToIntTuple3 = bsToIntTuple3 <$> BS.getLine

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

-- 便利関数系
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

-- nCr は 組み合わせ (combination)　の計算
nCr :: Int -> Int -> Int
nCr n r =
  let numerator = product $ take r [n, n -1 ..]
      denominator = product $ take r [1 ..]
   in numerator `div` denominator
