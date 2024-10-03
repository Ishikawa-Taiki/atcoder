{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
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
import Data.Maybe (fromJust)
import Debug.Trace (trace)

main :: IO ()
main = do
  (h, w) <- getLineToIntTuple2
  xs <- getContentsToStringList
  printMatrix $ solve xs h w

-- 良い感じの処理が思いつかず大変な実装になりそうだったので、以下参考にさせていただいた
-- https://atcoder.jp/contests/abc302/submissions/52408247
solve :: [String] -> Int -> Int -> [[Int]]
solve xs h w =
  let matrix = listArray @UArray ((1, 1), (h, w)) $ concat xs
   in fmap tuple2ToList $
        concatMap (search matrix (h, w)) $ do
          i <- [1 .. h]
          j <- [1 .. w]
          return (i, j)

type I2 = (Int, Int)

search :: UArray I2 Char -> I2 -> I2 -> [I2]
search matrix (h, w) (i, j)
  | j + 4 <= w && check matrix (coord (i, j) (0, 1)) = coord (i, j) (0, 1) --右方向
  | j -4 >= 1 && check matrix (coord (i, j) (0, -1)) = coord (i, j) (0, -1) --左方向
  | i + 4 <= h && check matrix (coord (i, j) (1, 0)) = coord (i, j) (1, 0) --下方向
  | i -4 >= 1 && check matrix (coord (i, j) (-1, 0)) = coord (i, j) (-1, 0) --上方向
  | j + 4 <= w && i + 4 <= h && check matrix (coord (i, j) (1, 1)) = coord (i, j) (1, 1) --左上/右下方向
  | j -4 >= 1 && i -4 >= 1 && check matrix (coord (i, j) (-1, -1)) = coord (i, j) (-1, -1) --右下/左上方向
  | j + 4 <= w && i -4 >= 1 && check matrix (coord (i, j) (-1, 1)) = coord (i, j) (-1, 1) --左下/右上方向
  | j -4 >= 1 && i + 4 <= h && check matrix (coord (i, j) (1, -1)) = coord (i, j) (1, -1) --右上/左下方向
  | otherwise = []

check :: UArray I2 Char -> [I2] -> Bool
check matrix ps = map (matrix !) ps == "snuke"

coord :: I2 -> I2 -> [I2]
coord (x, y) (dx, dy) = take 5 $ zip [x, x + dx ..] [y, y + dy ..]

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
