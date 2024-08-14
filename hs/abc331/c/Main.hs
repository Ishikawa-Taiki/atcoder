{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Redundant flip" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas -Wno-incomplete-patterns -Wno-unused-imports -Wno-unused-top-binds -Wno-name-shadowing -Wno-unused-matches #-}

-- © 2024 Ishikawa-Taiki
module Main (main) where

import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.List (sort)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Debug.Trace (trace)

main :: IO ()
main = do
  n <- getLineToInt
  xs <- getLineToIntArray
  printArrayWithSpace $ solve xs

{-
問題概略:
 数列が与えられる
 最初に与えられた数列の各要素に対し、より大きな要素全ての和をその順で出力せよ
戦略:
 数列の要素数が結構大きく、2*10^5ある
  各要素に対して和を求め直すような処理O(N^2)だと計算量超過でTLEするので、効率よく計算したい
 与えられた数値より大きな数の和を求める形であるため、一番大きい値を基準に少しずつ加算対象が増えていくような形になる
 数列をソートをした上で大きい方からの累積和を取っておけば、ある値より大きな数の和が簡単に求められる状態が作れる
 ソートした数列と累積和を紐づけることで元の値と和の対応が取れるので、これをベースに対応表を構築する
  最大値が選ばれたときにより大きな値は存在しないので、累積和側の最大部分に0を詰めておく
  最小値を含む累積和の結果は利用しないので、元の数列の先頭側に0を詰めておく
  数列中には同じ値が入り得るが、対応表としては足し切った値だけを利用したいので間引く
  (Map化するときに重複キーは後勝ちするので、そのまま採用すれば良い)
-}

solve :: [Int] -> [Int]
solve xs =
  let s = sort xs
      base = M.fromList . zip (0 : s) . scanr (+) 0 $ s
   in (base M.!) <$> xs

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
