{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Redundant flip" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas -Wno-incomplete-patterns -Wno-unused-imports -Wno-unused-top-binds -Wno-name-shadowing -Wno-unused-matches #-}

-- © 2024 Ishikawa-Taiki
module Main (main) where

import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.List (sortOn)
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import qualified GHC.Base as Control.Monad

main :: IO ()
main = do
  n <- getLineToInteger
  xxs <- fmap (arrayToTuple2 . bsToIntegerList) . BS.lines <$> BS.getContents
  case solve xxs n of
    Nothing -> do
      printYesNo False
      return ()
    Just chooseNumbers -> do
      printYesNo True
      printArrayWithSpace chooseNumbers

type Range = (Integer, Integer)

-- 問題概略
--  最小値〜最大値の範囲のリストが与えられる
--  範囲内の値を選択し続けることで合計を0にできる組み合わせがあるか？を答える
--  ある場合は、加えてそのサンプルも出力すること
-- 戦略
--  問題の性質より、最小値を選び続けていった結果が合計の最小値、最大値が合計の最大値となる
--  負の値は加算すると減り、正の値は加算すると増え続けてしまうので、
--   合計の最小値が0より小さく、合計の最大値が０より大きくなければ合計を0に調整することができない
--　　　　要素数Nが2*10^5乗の程度の制約なので、O(N^2)以上の走査はTLEする可能性が高い見込み
--  数値の分布が均等でない場合など、今あるものを単純に0に近づけていくだけでは解に到達できない可能性があるため、
--   各項目を算出する過程で向かうべき方向が明確になっていてほしい
--　　　　合計の最小値と合計の最大値はO(N)で求められるのでこれを判断に利用しつつ、
--　　　　　最小値から必要数分増やす or 最大値から必要数分減らす ための方向性として利用していく
--　　　　　正の数の方が扱いやすいので、最小から最大を目指す方向で進める

solve :: [Range] -> Integer -> Maybe [Integer]
solve ranges n =
  let (sumL, sumR) = debugProxy $ foldl (\(lSum, rSum) (l, r) -> (lSum + l, rSum + r)) (0, 0) ranges
      firstTarget = debugProxy $ - sumL
   in if sumL > 0 || sumR < 0
        then Nothing
        else Just (snd . foldl closestToZero (firstTarget, []) $ ranges)

closestToZero :: (Integer, [Integer]) -> Range -> (Integer, [Integer])
closestToZero (target, chooseList) (l, r) =
  let canBeIncreasedValue = r - l -- lをベースに考えると、最大これだけ(rまで)増やすことが出来る
      approachedValue = min canBeIncreasedValue target -- 今回目標に近づける分: 増やすことが出来る値 残り増やしたい値 を比較して、可能な限界まで近づけていく
   in (target - approachedValue, chooseList ++ [l + approachedValue])

-- 以下過去提出分(タイムアップ＆誤り)
-- -- 解法出てこないので、極力近づける方針で実装
-- solve :: [Range] -> Integer -> (Bool, [Integer])
-- solve ranges n =
--   let results = foldl acc (0, []) ranges
--    in if fst results == 0
--         then (True, snd results)
--         else (False, [])
--   where
--     acc :: (Integer, [Integer]) -> Range -> (Integer, [Integer])
--     acc (currentSum, chooseNumbers) range =
--       let choose = debugProxy $ closestToZero currentSum range
--        in debugProxy (choose + currentSum, chooseNumbers ++ [choose])
--
-- closestToZero :: Integer -> Range -> Integer
-- closestToZero current (l, r) =
--   let
--    in if l <= negate current && negate current <= r
--         then negate current -- l から r に値が入っているなら、0にできるのでその値を使う
--         else
--           if abs (current + l) <= abs (current + r)
--             then l -- 0にできない場合は絶対値が近づく方を使う l
--             else r -- 0にできない場合は絶対値が近づく方を使う r

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
