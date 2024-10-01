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

import Control.Monad (forM_, when)
import Control.Monad.Fix (fix)
import Control.Monad.ST (ST, runST)
import Data.Array.IArray (listArray)
import Data.Array.MArray (newListArray, readArray, writeArray)
import Data.Array.ST (STUArray)
import Data.Array.Unboxed (UArray)
import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Debug.Trace (trace)

main :: IO ()
main = do
  (h, w) <- getLineToIntTuple2
  xs <- getContentsToStringList
  print $ solve xs h w

{-
問題概要
H行W列のグリッドとして、センサーの配置状況が与えられる
上下左右1マス以内に隣接するセンサーは連動して動作する
連動するセンサーを1つのセンサーとみなした時、グリッド中にセンサーはいくつあるかを求めよ

戦略
DFSを用いて連動する一連のセンサーをマークするアクションを、全てのマスに対して順に行っていく
連動するセンサーは距離1の範囲なので、端を超えないように注意しつつ各マスを確認していく
連動するセンサーは一括でマークされることになるので、センサーを新しく見つけた時だけカウントアップする様にする
カウンタが出力結果なのでマークの方法はなんでも良いが、詰まった時のデバッグを考慮して見つけた時点のグループ(カウンタ)の最後の一文字を書いておくようにする

-}

solve :: [String] -> Int -> Int -> Int
solve xs h w = runST $ do
  ref <- newSTRef (0 :: Int) -- 連動するセンサーを1まとまりとした時のセンサー検出数(最終結果)
  sensors <- newListArray ((1, 1), (h, w)) $ concat xs :: ST s (STUArray s (Int, Int) Char) -- センサー配置情報(見たものを塗りつぶしていく)

  -- 全範囲を確認する
  forM_ [(i, j) | i <- [1 .. h], j <- [1 .. w]] $ \p -> do
    sensor <- readArray sensors p

    -- 1つ目のセンサー検出
    when (sensor == '#') $ do
      count <- readSTRef ref
      let mark = succ count
      writeSTRef ref mark

      -- DFSで連動するセンサーを全てマーキング
      flip fix p \dfs current@(i, j) -> do
        writeArray sensors current $! last . show $ mark

        -- グリッドの端を超えないように距離1の隣接マスを一通り確認
        let arounds = [(y, x) | y <- [max 1 (pred i) .. min h (succ i)], x <- [max 1 (pred j) .. min w (succ j)], not (y == i && x == j)]
        forM_ arounds $ \next -> do
          nextSensor <- readArray sensors next
          when (nextSensor == '#') $ dfs next

  readSTRef ref

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
