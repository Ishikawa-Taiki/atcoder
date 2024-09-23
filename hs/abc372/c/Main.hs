{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Redundant flip" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas -Wno-incomplete-patterns -Wno-unused-imports -Wno-unused-top-binds -Wno-name-shadowing -Wno-unused-matches #-}

-- © 2024 Ishikawa-Taiki
module Main (main) where

import Control.Monad (forM_, replicateM, when)
import Control.Monad.ST
import Data.Array (Array)
import Data.Array.IArray
import Data.Array.IO
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed (UArray)
import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Debug.Trace (trace)

main :: IO ()
main = do
  (n, q) <- getLineToIntTuple2
  s <- getLineToString
  query <- replicateM q $ do
    query <- getLineToString
    let (x : c : _) = words query
    return (read x :: Int, head c)
  printListWithLn $ solve n q s query

{-
問題概要
クエリのリストと文字列が与えられる
クエリは何文字目をどの文字に置き換えるかの情報が入っている
各クエリを処理した後 、 文字列中にABCの部分文字列がいくつ含まれているかを出力せよ

戦略
文字列及びクエリのリストはそれなりに長いので、置き換えながら毎回数えるわけにはいかなさそう
最初にABCの部分文字列が何個含まれているかを数えておき、それをベースに置き換える毎に影響を与える部分だけ再確認すれば良さそう

UArrayで実行すると文字列書き換え時にコピーが発生する関係上O(N)かかってしまうので、ミュータブルな配列を用いた実装が必要
IOは切り離したい派なので、STUArrayの方を利用してみる
更新カウンタも必要なので、STモナドも調べながら試す

-}

solve :: Int -> Int -> String -> [(Int, Char)] -> [Int]
solve n q s query = elems $
  runSTUArray $ do
    -- ABCの部分文字列の数を示す全体カウンタ
    countRef <- newSTRef (0 :: Int)
    -- クエリ毎のABCの部分文字列の数を示すカウンタ
    counts <- newArray (1, q) 0 :: ST s (STUArray s Int Int)
    -- 操作対象の文字列
    str <- newListArray (1, n) s :: ST s (STUArray s Int Char)

    -- 最初の文字列に含まれるABCの部分文字列の数を数えておく
    forM_ [1 .. n - 2] $ \(!idx) -> do
      count <- readSTRef countRef
      check <- checkABC n str idx (idx + 2)
      Control.Monad.when (check == 1) $ writeSTRef countRef (succ count)

    -- 該当文字を書き換えながら、ABCの部分文字列の増減を反映させていく
    forM_ (zip [1 .. q] query) $ \(!i, (!idx, !char)) -> do
      -- 変更前：書き換え場所の左側と真ん中と右側にどれだけABCの部分文字列があるか
      beforeL <- checkABC n str (idx -2) idx
      beforeC <- checkABC n str (idx -1) (idx + 1)
      beforeR <- checkABC n str idx (idx + 2)
      -- 文字列の該当データを実際に書き換える
      writeArray str idx $! char
      -- 変更後：書き換え場所の左側と真ん中と右側にどれだけABCの部分文字列があるか(再度同じ計算を実施)
      afterL <- checkABC n str (idx -2) idx
      afterC <- checkABC n str (idx -1) (idx + 1)
      afterR <- checkABC n str idx (idx + 2)
      count <- readSTRef countRef
      let before = sum [beforeL, beforeC, beforeR]
          after = sum [afterL, afterC, afterR]
          diff = after - before
          currentResult = count + diff
      -- 変更前後のABCの部分文字列の増減数を全体カウンタとクエリ毎のカウンタに反映
      writeSTRef countRef currentResult
      writeArray counts i $! currentResult
      return ()

    return counts

-- 文字列の長さ、文字列、開始位置、終了位置を受け取る
-- 該当位置の文字列がABCで構成されているなら1を、そうでなければ0を返す
checkABC :: Int -> STUArray s Int Char -> Int -> Int -> ST s Int
checkABC n str startIdx endIdx = do
  if startIdx <= 0 || endIdx > n
    then return 0
    else do
      a <- readArray str startIdx
      b <- readArray str (startIdx + 1)
      c <- readArray str endIdx
      return $ if a == 'A' && b == 'B' && c == 'C' then 1 else 0

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
