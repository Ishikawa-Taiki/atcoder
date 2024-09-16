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

import Control.Monad (replicateM)
import Data.Array.Unboxed (UArray, accumArray, listArray, (!))
import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.List (inits, permutations, transpose)
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Debug.Trace (trace)

main :: IO ()
main = do
  n <- getLineToInt
  mg <- getLineToInt
  g <- replicateM mg getLineToIntTuple2
  mh <- getLineToInt
  h <- replicateM mh getLineToIntTuple2
  a <- getContentsToIntMatrix
  print $ solve n g h a

{-
問題概要
2つの単純無向グラフの現在の辺リストと辺毎の接続状態切り替えコストが与えられる
2つ目のグラフを一つ目のグラフと同型にしたい時、最小の切り替えコストは幾つになるかを求めよ

戦略
制約上の頂点数の最大が8なので、同型にするための全ての操作パターンを試して一番安いコストを算出しても十分に高速になる
同型はあくまで構造としての同じさなので、頂点の番号の違いは区別しないようにする必要がある
 これを考慮する際、どちらかのグラフの頂点番号を入れ替えたパターンを全部列挙してそれぞれ試せば良い
 比較元、比較先どちらを触っても同じ確認が行えるが、比較先をいじると接続状態の切り替えを行う関係上混乱を招きやすいので、比較元側の番号を入れ替えたものと比較する
接続状態を確認する際やコスト切り替えを行う際の辺を扱いやすくするため、頂点を入れ替えたもののデータも用意しておくと良さそう
重みのない無効グラフであるため 、 頂点の2次元マトリクスのような構造で辺を持つかどうかを管理する

-}

solve :: Int -> [(Int, Int)] -> [(Int, Int)] -> [[Int]] -> Int
solve n g h a =
  let -- 二次元マトリクスにするためのコードは以下を参考にさせていただいた
      -- https://atcoder.jp/contests/abc371/submissions/57814605
      gGraph = accumArray @UArray (||) False ((1, 1), (n, n)) $ concat [[(pair, True), (swap pair, True)] | pair <- g]
      hGraph = accumArray @UArray (||) False ((1, 1), (n, n)) $ concat [[(pair, True), (swap pair, True)] | pair <- h]
      tmpCost = zipWith (++) (tail $ inits $ repeat 0) (a ++ [[]])
      costs = listArray @UArray ((1, 1), (n, n)) $ concat $ zipWith (zipWith max) tmpCost $ transpose tmpCost
   in minimum
        [ sum
            [ costs ! (i, j)
              | i <- [1 .. n -1],
                j <- [i + 1 .. n],
                gGraph ! (indexPettern ! i, indexPettern ! j) /= hGraph ! (i, j)
            ]
          | indexPettern <- listArray @UArray (1, n) <$> permutations [1 .. n]
        ]

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
