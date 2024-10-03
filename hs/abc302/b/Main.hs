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
import Data.Maybe (catMaybes, fromJust)
import Debug.Trace (trace)

main :: IO ()
main = do
  (h, w) <- getLineToIntTuple2
  xs <- getContentsToStringList
  printMatrix $ solve xs h w

solve :: [String] -> Int -> Int -> [[Int]]
solve xs h w =
  let matrix = listArray @UArray ((1, 1), (h, w)) $ concat xs
   in fmap tuple2ToList $
        concatMap (search matrix (h, w)) $ do
          i <- [1 .. h]
          j <- [1 .. w]
          return (i, j)

type I2 = (Int, Int)

type F2 = (Int -> Int, Int -> Int)

-- 指定された座標から8方向分の文字列が"snuke"であるかを確認し、該当するならインデックスタプルのリストを返す
search :: UArray I2 Char -> I2 -> I2 -> [I2]
search matrix (h, w) (i, j) =
  let toR = bool [] (createPositionList (id, succ) (i, j)) $ j + 4 <= w
      toL = bool [] (createPositionList (id, pred) (i, j)) $ j -4 >= 1
      toD = bool [] (createPositionList (succ, id) (i, j)) $ i + 4 <= h
      toU = bool [] (createPositionList (pred, id) (i, j)) $ i -4 >= 1
      toRD = bool [] (createPositionList (succ, succ) (i, j)) $ j + 4 <= w && i + 4 <= h
      toLU = bool [] (createPositionList (pred, pred) (i, j)) $ j -4 >= 1 && i -4 >= 1
      toLD = bool [] (createPositionList (pred, succ) (i, j)) $ j + 4 <= w && i -4 >= 1
      toRU = bool [] (createPositionList (succ, pred) (i, j)) $ j -4 >= 1 && i + 4 <= h
      candidate = [toR, toL, toD, toU, toRD, toLU, toLD, toRU]
   in concat $ do
        positionList <- candidate
        return $ bool [] positionList $ "snuke" == matrix `toString` positionList

-- インデックスタプルのリストを基に配列を文字列化する
toString :: UArray I2 Char -> [I2] -> String
toString matrix = map (matrix !)

-- 座標の方向と座標を受け取り、5文字分の配列インデックスタプルのリストを返す
createPositionList :: F2 -> I2 -> [I2]
createPositionList (f, g) (x, y) = take 5 $ zip [x, f x ..] [y, g y ..]

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
