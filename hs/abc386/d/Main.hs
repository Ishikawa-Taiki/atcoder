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
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid (Sum (..))
import Data.STRef (modifySTRef, newSTRef, readSTRef, writeSTRef)
import qualified Data.Set as S
import Data.Tuple (swap)
import Debug.Trace (trace)

main :: IO ()
main = do
  (n, m) <- getLineToIntTuple2
  xs <- replicateM m $ do
    (c, r, t) <- listToTuple3 . words <$> getLineToString
    return (head t, read @Int c, read @Int r)
  printYesNo $ solve xs n m

{-
問題概要：
NxNのグリッドをあるルールのもとで白黒に塗り分けたい。
いくつかのマスが何色で与えられるが、そのルールに塗り分けられるかどうかを求めよ。
ルールは以下の通り。
・すべての行についてあるマスから左は黒、右は白で塗られている。
・すべての列についてあるマスから上は黒、下は白で塗られている。

戦略：
Nはとても大きいので、全探索は無理。
塗られているマスを基準に畳み込みながら、行と列の条件を満たしているかを順にチェックする。

行/列ごとに一番右/下の黒と一番左/上の白を覚えておく、ような見方をすれば塗られているマスだけ見ればできそう。
※もっというと、各行/列で 一番右/下の黒と一番左/上の白 だけ調べればよさそうな気もする???

-}

solve :: [(Char, Int, Int)] -> Int -> Int -> Bool
solve xs n m = result
  where
    (black, white) = partition ((== 'B') . fst3) xs
    disposeColorColumn (_, c, r) = (c, r)
    disposeColorRow (_, c, r) = (r, c)
    !bc = debugProxy "bc" $ M.fromListWith max $ disposeColorColumn <$> black
    !wc = debugProxy "wc" $ M.fromListWith min $ disposeColorColumn <$> white
    !br = debugProxy "br" $ M.fromListWith max $ disposeColorRow <$> black
    !wr = debugProxy "wr" $ M.fromListWith min $ disposeColorRow <$> white
    result = undefined

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
