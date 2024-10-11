{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Redundant flip" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas -Wno-incomplete-patterns -Wno-unused-imports -Wno-unused-top-binds -Wno-name-shadowing -Wno-unused-matches #-}

-- © 2024 Ishikawa-Taiki
module Main (main) where

import Data.Array.IArray (listArray, (!))
import Data.Array.Unboxed (UArray)
import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.List (find, sort, sortBy)
import Data.Maybe (fromJust)
import Debug.Trace (trace)

main :: IO ()
main = do
  (n, m) <- getLineToIntTuple2
  a <- getLineToIntList
  b <- getLineToIntList
  print $ solve a b n m

{-
問題概要
買い手と売り手の金額掲示リストが与えられる
売り手の人数が買い手の人数以上になる最小の金額はいくらか？

戦略
最大でも売り手と買い手の人数の小さい数しか売買は成立しないので、売りやすい人と買いやすい人からその人数分考えれば良さそう
一番売りやすく一番買いづらい人、もしくは売りづらく買いやすい人を紐付けたい

↓仕切り直し

値段を上げると売り手が増えて、買い手が減る
売り手の人数を買い手の人数以上にするための最小の金額が知りたいので、売り手の人数が増えるタイミングか買い手の人数が減るタイミングがボーダーになる可能性がある数値となる
それらの候補より、順に売り手と買い手の人数を出して条件を満たすラインの値を探す

-}

solve :: [Int] -> [Int] -> Int -> Int -> Int
solve seller buyer sn bn =
  let maxTrade = min sn bn
      s = listArray @UArray (1, sn) $ sort seller -- 売りやすい順金額
      b = listArray @UArray (1, bn) $ sortBy (flip compare) buyer -- 買いやすい順金額
      c = sort $ seller ++ map (+ 1) buyer -- ボーダー候補
   in fromJust $ find (check s b) c
  where
    calcSell s n = fst $ binarySearch (\i -> s ! i <= n) (0, succ sn)
    calcBuy b n = fst $ binarySearch (\i -> b ! i >= n) (0, succ bn)
    check s b v =
      let sellerCount = calcSell s v
          buyerCount = calcBuy b v
          !_ = debug "value,(sell,buy)" (v, (sellerCount, buyerCount))
       in sellerCount >= buyerCount

-- 値が有効化どうかを確認する関数と、現在のOK/NG範囲を受け取り、最終的なOK/NG範囲を返却する
binarySearch :: (Int -> Bool) -> (Int, Int) -> (Int, Int)
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
