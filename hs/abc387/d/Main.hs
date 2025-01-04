{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Redundant flip" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas -Wno-incomplete-patterns -Wno-unused-imports -Wno-unused-top-binds -Wno-name-shadowing -Wno-unused-matches #-}

module Main (main) where

import Control.Monad (forM_, replicateM, unless, when)
import Control.Monad.Fix (fix)
import Data.Array.Unboxed (Array, IArray (bounds), Ix (range), UArray, accumArray, listArray, (!), (//))
import Data.Bifunctor (bimap, first, second)
import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char (digitToInt, intToDigit, isLower, isUpper, toLower, toUpper)
import Data.List
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, fromMaybe)
import Data.Monoid (Sum (..))
import Data.STRef (modifySTRef, newSTRef, readSTRef, writeSTRef)
import qualified Data.Set as S
import Data.Tuple (swap)
import Debug.Trace (trace)

main :: IO ()
main = do
  (h, w) <- getLineToIntTuple2
  xs <- getContentsToStringList
  print $ solve xs h w

{-
問題概要：
スタートとゴールがひとつずつ含まれた、壁と通路のグリッドが与えられる。
縦横交互に移動するという制約のもとでスタートからゴールまでの最短の経路を出力せよ。
到達できない場合は-1を出力せよ。

戦略：
縦横切り替えながらのbfsで解きたい
初手で縦横両方実施して、早い方でゴールしたい
（やり方誤ってて時間までにうまく書けそうにないので、タイムアップ時点のコードを一旦提出）

-}

solve :: [String] -> Int -> Int -> Int
solve xs h w = result
  where
    grid = listArray @UArray ((1, 1), (h, w)) $ concat xs
    start = head [p | p <- range ((1, 1), (h, w)), isStart p]
    calc = catMaybes [bfs S.empty (True, start) 0, bfs S.empty (False, start) 0]
    result = bool (-1) (minimum calc) (not $ null calc)
    isStart :: (Int, Int) -> Bool
    isStart p = grid ! p == 'S'
    isGoal :: (Int, Int) -> Bool
    isGoal p = grid ! p == 'G'
    bfs :: S.Set (Int, Int) -> (Bool, (Int, Int)) -> Int -> Maybe Int
    bfs visited before@(isVertical, pos) count
      | isGoal pos = Just count
      | null validCandidates = Nothing
      | otherwise =
        let finishedList = catMaybes [bfs (cs `S.insert` visited) (not isVertical, cs) (succ count) | cs <- validCandidates]
         in bool Nothing (Just $ minimum finishedList) (not $ null finishedList)
      where
        validCandidates = filter (`S.notMember` visited) (candidates (not isVertical) pos)
        candidates :: Bool -> (Int, Int) -> [(Int, Int)]
        candidates nextVertical (y, x)
          | nextVertical =
            [ p
              | p@(i, j) <- [(y + 1, x), (y - 1, x)],
                1 <= i && i <= h,
                1 <= j && j <= w,
                p `notElem` visited,
                grid ! p == '.' || grid ! p == 'G'
            ]
          | otherwise =
            [ p
              | p@(i, j) <- [(y, x + 1), (y, x - 1)],
                1 <= i && i <= h,
                1 <= j && j <= w,
                p `notElem` visited,
                grid ! p == '.' || grid ! p == 'G'
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
foldlDebugProxy :: (Show a, Show b) => (a -> b -> a) -> a -> b -> a
foldlDebugProxy f p1 p2 =
  let r = f p1 p2
      !_ = debug "before, param, result" (p1, p2, r)
   in r

#ifndef ATCODER

debugProxy :: (Show a) => String -> a -> a
debugProxy tag value =
  let !_ = debug tag value
   in value

debug :: (Show a) => String -> a -> ()
debug key value = trace (key ++ " : " ++ show value) ()

#else

debugProxy :: (Show a) => String -> a -> a
debugProxy _ = id

debug :: (Show a) => String -> a -> ()
debug _ _ = ()

#endif
