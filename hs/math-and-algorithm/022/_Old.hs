{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

-- © 2024 Ishikawa-Taiki
module Main (main) where

import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char (isSpace)
import qualified Data.List as L
import Data.Map as Map
  ( Map,
    delete,
    empty,
    insert,
    keys,
    member,
    update,
    (!?),
  )
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (fromList, toList)
import Debug.Trace (trace)
import GHC.Float (int2Float)

main :: IO ()
main = do
  n <- getLineToInt
  a <- getLineToIntArray
  print $ solve a

-- 先に各カードの枚数を数えてから、条件を満たすペアの数を数え上げていく(計算量削減)
solve :: [Int] -> Int
solve xs = countUpCards $ debugProxy $ convertMap xs Map.empty

type CardMap = Map Int Int

-- 各カードの枚数を数える処理
convertMap :: [Int] -> CardMap -> CardMap
convertMap [] map = map
convertMap (x : xs) map =
  if Map.member x map
    then convertMap xs $ Map.update (\x -> Just (x + 1)) x map
    else convertMap xs $ Map.insert x 1 map

-- ペア毎の組み合わせ数と残りを数える部分を統合する処理
countUpCards :: CardMap -> Int
countUpCards map = do
  let cards = keys map
      pairs = getPairs map
  if null cards
    then 0
    else countPairs pairs map + countUpCards (removePairs pairs map)
  where
    getPairs map =
      let x = head $ keys map
          y = 100000 - x
       in debugProxy (x, y)

-- ペアに対する組み合わせ数を返却する
countPairs :: (Int, Int) -> CardMap -> Int
countPairs (x, y) map = do
  let firstValue = Data.Maybe.fromMaybe 0 $ map Map.!? x
      secondValue = Data.Maybe.fromMaybe 0 $ map Map.!? y
      !_ = debug "first" firstValue
      !_ = debug "second" secondValue
  if firstValue == secondValue && firstValue >= 2
    then debugProxy $ nCr firstValue 2
    else firstValue * secondValue

-- 確認したペアを消す
removePairs :: (Int, Int) -> CardMap -> CardMap
removePairs (x, y) map = delete y $ delete x map

{- Library -}
-- データ変換共通
boolToYesNo :: Bool -> String
boolToYesNo = bool "No" "Yes"

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

-- nCr は 組み合わせ (combination)　の計算
nCr :: Int -> Int -> Int
nCr n r =
  let numerator = product $ take r [n, n -1 ..]
      denominator = product $ take r [1 ..]
   in numerator `div` denominator
