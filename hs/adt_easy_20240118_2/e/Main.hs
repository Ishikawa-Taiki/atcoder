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
import Data.Maybe (fromJust)
import Data.Set (fromList, toList)
import Debug.Trace (trace)
import GHC.Float (int2Float)

main :: IO ()
main = do
  (n, q) <- getLineToIntTuple2
  s <- getLine
  xxs <- getContentsToIntMatrix
  printArrayWithLn $ solve s xxs

-- 部分文字列を毎回作ると計算量的に間に合わないため、連続した文字かどうかを示す0/1のリストを作って、クエリー毎に範囲を畳み込んで計算する
solve :: String -> [[Int]] -> [Int]
solve s [] = []
solve s (query : xxs) =
  let checklist =
        0 : fmap (\index -> if (s !! index) == (s !! (index -1)) then 1 else 0) [1 .. length s -1]
      check list (l : r : _) = sum $ debugProxy $ take (r - l) . drop l $ list :: Int
   in check checklist query : solve s xxs

-- query を元に部分文字列を抽出したのち、連続を除去した文字列の長さと差分を取る
-- solve s [] = []
-- solve s (query : xxs) = check s query : solve s xxs
--   where
--     check :: String -> [Int] -> Int
--     check s (l : r : _) =
--       let substr = debugProxy $ take (r - (l -1)) . drop (l -1) $ s
--        in length substr - length (trimStr (head substr) (tail substr))

-- -- 前の文字と残りの文字列を受け取り、連続した文字を取り除いた文字列を返却する
-- trimStr :: Char -> String -> String
-- trimStr before [] = [before]
-- trimStr before (x : xs) =
--   if before == x
--     then trimStr x xs
--     else before : trimStr x xs

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
