{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas -Wno-incomplete-patterns -Wno-unused-imports -Wno-unused-top-binds -Wno-name-shadowing -Wno-unused-matches #-}

-- © 2024 Ishikawa-Taiki
module Main (main) where

import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char (isSpace)
import Data.List (sort)
import qualified Data.List as L
import Data.Maybe (catMaybes, fromJust)
import Data.Set (fromList, toList)
import Debug.Trace (trace)
import GHC.Float (int2Float)

main :: IO ()
main = do
  n <- getLineToInt
  q <- getLineToIntArray
  a <- getLineToIntArray
  b <- getLineToIntArray
  print $ solve q a b n

{-
問題概要
 食材リストqと料理1、2を一人前作るのに必要な食材量リストa bが与えられる
 最大何人前の料理を作れるか？を求めよ
戦略
 料理1を0-作れる最大の個数まで一通り作ったとき、料理2が最大いくつ作れるかを考える
 料理が最大でいくつ作れるかは、必要食材がゼロでないものに対して、何回分あるかを求めた時の最小値に等しい
 料理1の個数事に残りの食材量を求め、残りの食材で料理2が何回分作れるかを求める
 この料理1と2の合計個数が答えとなる
-}

solve :: [Int] -> [Int] -> [Int] -> Int -> Int
solve q a b n =
  maximum
    [ aNum + bNum
      | aNum <- [0 .. calcMax q a],
        let bNum = flip calcMax b $ zipWith (-) q $ calcCost a aNum
    ]
  where
    calcMax :: [Int] -> [Int] -> Int
    calcMax resource cost = minimum . catMaybes $ zipWith safeDiv resource cost
    calcCost :: [Int] -> Int -> [Int]
    calcCost cost num = fmap (* num) cost

safeDiv :: Int -> Int -> Maybe Int
safeDiv x 0 = Nothing
safeDiv x y = Just $ x `div` y

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
  let numerator = product $ take r [n, n - 1 ..]
      denominator = product $ take r [1 ..]
   in numerator `div` denominator
