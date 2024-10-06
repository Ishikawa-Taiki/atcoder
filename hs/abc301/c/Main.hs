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

import qualified Data.Bifunctor as BF
import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.List (group, sort)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import Debug.Trace (trace)

main :: IO ()
main = do
  s <- getLineToString
  t <- getLineToString
  printYesNo $ solve s t

{-
問題概要
a-z及び@マークで構成された、長さが等しい二つの文字列が与えられる
@マークは"atcoder"いずれかの文字に置き換えることが出来、任意の並べ替えも行うことができる
上記製薬のもと、二つの文字列を等しくすることが出来るかどうかを答えよ

戦略
文字列の並べ替えが自由に行えることより、各文字の登場回数が等しく出来るかどうかだけがポイントになる
二つの文字列の各文字の登場回数がどれくらい乖離しているかを基に、等しく出来るかどうかを判断する
@が置き換えられるのは"atcoder"のいずれかの文字だけなので、差分の中にこれ以外の文字が含まれていたら揃えることはできない
乖離を計算した結果、置き換えが必要な文字数が置き換え可能な文字数以内に収まっている場合は等しくすることが可能となる

-}

solve :: String -> String -> Bool
solve s t =
  let sm = countElements s
      tm = countElements t
      base = foldl (createDiffMap sm tm) M.empty ['a' .. 'z']
      (tNeedChars, tNeedFreeCount) = calcNeedsCharAndFreeCount (0 <) base
      (sNeedChars, sNeedFreeCount) = calcNeedsCharAndFreeCount (0 >) base
      sCheck1 = all (`elem` "atcoder") sNeedChars
      sCheck2 = sNeedFreeCount <= sm `mapDefault` '@'
      tCheck1 = all (`elem` "atcoder") tNeedChars
      tCheck2 = tNeedFreeCount <= tm `mapDefault` '@'
   in (sCheck1 && sCheck2) && (tCheck1 && tCheck2)

-- リストの各要素を数える
countElements :: Ord a => [a] -> M.Map a Int
countElements = M.fromList . map count . group . sort
  where
    count xs = (head xs, length xs)

-- 二つのMapを比較し、含まれているものの差を計算する
-- (m1に含まれているものは正として増加、m2に含まれているものは負として増加していく)
createDiffMap :: M.Map Char Int -> M.Map Char Int -> M.Map Char Int -> Char -> M.Map Char Int
createDiffMap m1 m2 m c = M.insert c (mapDiff m1 m2 c) m
  where
    mapDiff :: M.Map Char Int -> M.Map Char Int -> Char -> Int
    mapDiff m1 m2 c =
      let m1v = m1 `mapDefault` c
          m2v = m2 `mapDefault` c
       in m1v - m2v

-- Map中にキーが含まれていればそれの値を、なければ0としてフォールバックした値を返す
mapDefault :: M.Map Char Int -> Char -> Int
mapDefault m c = fromMaybe 0 $ m M.!? c

-- 差分が格納されたMap値を畳み込み、どんな文字が含まれているかのリストと合計の絶対値がいくつかを返す
calcNeedsCharAndFreeCount :: (Int -> Bool) -> M.Map Char Int -> ([Char], Int)
calcNeedsCharAndFreeCount f = BF.second abs . M.foldlWithKey (\(chars, count) k v -> (k : chars, count + v)) ([], 0) . M.filter f

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
