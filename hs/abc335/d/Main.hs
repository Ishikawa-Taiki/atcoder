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

import Data.Bifunctor (Bifunctor (bimap))
import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Debug.Trace (trace)

main :: IO ()
main = do
  n <- getLineToInt
  (\mtx -> putStr . unlines $ unwords <$> mtx) $ solve n

solve :: Int -> [[String]]
solve n = result
  where
    m = createPosMap n
    result = chunksOfList n [bool (show n) "T" (n == 0) | i <- [1 .. n], j <- [1 .. n], let n = m M.! (i, j)]

-- リストをn個ずつの要素数のリストに分解する
chunksOfList :: Int -> [a] -> [[a]]
chunksOfList n [] = []
chunksOfList n xs = as : chunksOfList n bs
  where
    (as, bs) = splitAt n xs

createPosMap :: (Integral a1, Num a2, Enum a2, Show a2, Show a1) => a1 -> M.Map (a1, a1) a2
createPosMap n = posMap
  where
    nDiv2 = n `div` 2
    centerPos = succ n `div` 2
    posList = bimap (+ centerPos) (+ centerPos) <$> mkGrid nDiv2
    posMap = M.fromList $ zip posList [0 ..]
    !_ = debug "list" posList
    !_ = debug "map" posMap

-- 中心からの距離を渡して中心からの距離のタプルリストを返す
-- 計算順：前のリスト、真左から下、左下から右下、右下から右上、右上から左上
mkGrid 0 = [(0, 0)]
mkGrid n =
  let before = mkGrid (pred n)
      leftDown = [(i, negate n) | i <- [negate (pred n) .. pred n]]
      bottom = [(n, j) | j <- [negate n .. n]]
      rightUp = reverse [(i, n) | i <- [negate (pred n) .. pred n]]
      top = reverse [(negate n, j) | j <- [negate n .. n]]
      result = before ++ leftDown ++ bottom ++ rightUp ++ top
   in -- !_ = debug "before" before
      -- !_ = debug "leftDown" leftDown
      -- !_ = debug "bottom" bottom
      -- !_ = debug "rightUp" rightUp
      -- !_ = debug "top" top
      -- !_ = debug "result" result
      result

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
