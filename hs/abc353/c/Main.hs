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

import Data.Array.Unboxed (IArray (bounds), Ix (range), UArray, listArray, (!))
import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.List (sort)
import Data.Maybe (fromJust)
import Debug.Trace (trace)

main :: IO ()
main = do
  n <- getLineToInt
  xs <- getLineToIntArray
  print $ solve xs n

{-
問題概要：
数列が与えられる
重複を除いた各組み合わせについて、f(x,y)を算出し、その総和を求めよ
f(x,y)はx+yを足した結果を10^8で割った余りとする

戦略：
数列がとても長いので、愚直に組み合わせて計算しては処理が間に合わない
数列の値は10^8未満であることから、余りを算出するのは超えている時に10^8を引くことと等しい

最初に単純なペアの加算結果の総和を求め、そこからペアの加算結果が10^8を超える回数分引くような対応を行う
単純なペアの総和を求める場合は自分以外の要素の回数分加算される形になるため、要素の加算結果にN-1をかければベースとなる単純な総和は算出できる

和が10^8を超える組み合わせについては、要素をソートすればラインができる
しゃくとり法や二分探索を用いることで超えるラインを見極められるので、これを各要素に対して行えば超えた数が求められる
算出した数*10^8を引けば答えを求めることができる
-}

-- 中断
-- ベースとなる数を求めるところは実装済み
-- 10^8を超える数の算出について、調整中
solve :: [Int] -> Int -> Integer
solve xs n =
  let border = 10 ^ 8
      diffPointsNum = n - 1
      !base = toInteger diffPointsNum * sum (toInteger <$> xs) -- 各項は自分以外の値の数回足されるので、掛け算で求められる
      !sorted = listArray @UArray (1, n) $ sort xs
      -- !diff = debugProxy $ head sorted : zipWith (-) (tail sorted) sorted
      -- !shaku = debugProxy $ shakutori (\r total -> border > debugProxy (r + total)) (+) (-) 0 diff
      -- !overNum = debugProxy $ sum . zipWith (-) (reverse [1 .. diffPointsNum]) $ shaku
      !overNums = debugProxy $
        flip fmap [1 .. n -1] $ \index ->
          let currentPairCount = n - index
              firstNgIndex = snd $ binarySearch (not . isOver sorted border index) (index, n)
              -- okCount = bool lastOkIndex 0 $ lastOkIndex <= index
              !_ = debug "pairCount,firstNgIndex" (currentPairCount, firstNgIndex)
           in 0
      overNum = 0
      !minusValue = border * overNum -- 10^8を超えた時は引けば辻褄が合うので、超えた回数から算出する
   in base -- - minusValue

-- リスト、ボーダーライン、ベースIndex、対象のIndexを受け取り、ボーダーを越えるかどうかを返却する
isOver :: UArray Int Int -> Int -> Int -> Int -> Bool
isOver list border base target = (list ! target) > border - (list ! base)

-- 値が有効化どうかを確認する関数と、現在のOK/NG範囲を受け取り、最終的なOK/NG範囲を返却する
binarySearch :: (Int -> Bool) -> (Int, Int) -> (Int, Int)
binarySearch check (ok, ng)
  | abs (ng - ok) == 1 = (ok, ng)
  | otherwise =
    let mid = (ok + ng) `div` 2
     in if check mid
          then binarySearch check (mid, ng)
          else binarySearch check (ok, mid)

-- shakutori :: (a -> b -> Bool) -> (b -> a -> b) -> (b -> a -> b) -> b -> [a] -> [Int]
-- shakutori p op invOp identity as = go as as 0 identity
--   where
--     go lls@(l : ls) [] len res = len : go ls [] (len - 1) (invOp res l)
--     go lls@(l : ls) rrs@(r : rs) len res
--       | p r res = go lls rs (len + 1) (op res r)
--       | len == 0 = 0 : go ls rs 0 identity
--       | otherwise = len : go ls rrs (len - 1) (invOp res l)
--     go _ _ _ _ = []

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
