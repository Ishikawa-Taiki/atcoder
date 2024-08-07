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
import Data.List (sort)
import Data.Maybe (fromJust)
import Debug.Trace (trace)

main :: IO ()
main = do
  (n, m) <- arrayToTuple2 . bsToIntegerList <$> BS.getLine
  xs <- getLineToIntegerArray
  putStrLn $ maybe "infinite" show (solve xs n m)

-- 問題概略
--  交通費のリストと予算が与えられる
--  合計予算を超えないように交通費補助額の上限を設定したいが、最大いくらにできるか？
--  上限額を無限に大きくできる場合、そのことを報告する
-- 戦略
--  交通費の合計額が予算より小さい場合、全て支払ってあげて問題ないので無限を報告する
--  そうでない場合はOKとなる最大値を見つけなければならないが、リスト内の分布が分からないので実際に値を入れながら試した方が良さそう
--  リストの要素数が結構大きいので、実際に入れる値を探す処理はある程度効率よく見つけたいところ
--  問題の特性より予算の閾値には明確なボーダーライン(単調性)があるため、二分探索を用いることで効率よく間引く事が出来る
--  二分探索をする際は絶対にOK/NGになる値を初期値にする必要があるので、
--  　予算0(支払わない)をOKの初期値、予算m+1(予算より大きな値まで払う)をNGの初期値にする
--  計算量的には　最初に合計を取るO(N) + ボーダーラインを探る二分探索O(logN) * ボーダーラインを探る過程での全要素のお試し計算O(N) あたりなので、問題ない見込みとなる
solve :: [Integer] -> Integer -> Integer -> Maybe Integer
solve xs n m =
  if sum xs <= m
    then Nothing
    else Just . fst . binarySearch (not . isOver xs m) $ (0, m + 1)

-- 支払いリストに予算と経費を設定した際、予算超過になるかどうかを確認する
isOver :: [Integer] -> Integer -> Integer -> Bool
isOver list budget expenses = budget < sum (min expenses <$> list)

-- 値が有効化どうかを確認する関数と現在のOK/NG範囲を受け取り、最終的なOK/NG範囲を返却する
binarySearch :: (Integer -> Bool) -> (Integer, Integer) -> (Integer, Integer)
binarySearch check (ok, ng)
  | abs (ng - ok) == 1 = (ok, ng)
  | otherwise =
    let mid = (ok + ng) `div` 2
     in if check mid
          then binarySearch check (mid, ng)
          else binarySearch check (ok, mid)

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
