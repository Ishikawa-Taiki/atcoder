{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Redundant flip" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas -Wno-incomplete-patterns -Wno-unused-imports -Wno-unused-top-binds -Wno-name-shadowing -Wno-unused-matches #-}

-- © 2024 Ishikawa-Taiki
module Main (main) where

import Data.Bifunctor (Bifunctor (first, second))
import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Debug.Trace (trace)

main :: IO ()
main = do
  (h, w, n) <- getLineToIntTuple3
  putStr . unlines $ solve h w n

solve :: Int -> Int -> Int -> [String]
solve h w n = makeStrings h w $ thd3 . foldl acc ((1, 1), U, S.empty) $ [1 .. n]
  where
    acc :: R -> Int -> R
    acc all@(pos, dir, blacks) _ =
      let isBlack = pos `S.member` blacks
          nextDirOp = bool lotateClock lotateUnClock isBlack
          nextDir = nextDirOp dir
          nextBlacksOp = bool S.insert S.delete isBlack
          nextBlack = pos `nextBlacksOp` blacks
          nextPos = move (h, w) nextDir pos
       in (nextPos, nextDir, nextBlack)

makeStrings :: Int -> Int -> S.Set Position -> [String]
makeStrings h w blacks = chunksOfList w [c | y <- [1 .. h], x <- [1 .. w], let c = bool '.' '#' $ (y, x) `S.member` blacks]

type R = (Position, Direction, S.Set Position)

data Direction = U | R | D | L
  deriving (Eq, Show)

type Position = (Int, Int)

type Size = (Int, Int)

move :: Size -> Direction -> Position -> Position
move (h, w) d p@(y, x)
  | d == U && y == 1 = (h, x)
  | d == D && y == h = (1, x)
  | d == L && x == 1 = (y, w)
  | d == R && x == w = (y, 1)
  | otherwise = f d p
  where
    f U = first pred
    f D = first succ
    f L = second pred
    f R = second succ

lotateClock :: Direction -> Direction
lotateClock U = R
lotateClock R = D
lotateClock D = L
lotateClock L = U

lotateUnClock :: Direction -> Direction
lotateUnClock U = L
lotateUnClock L = D
lotateUnClock D = R
lotateUnClock R = U

-- リストをn個ずつの要素数のリストに分解する
chunksOfList :: Int -> [a] -> [[a]]
chunksOfList n [] = []
chunksOfList n xs = as : chunksOfList n bs
  where
    (as, bs) = splitAt n xs

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

-- IO 出力系
printYesNo :: Bool -> IO ()
printYesNo = putStrLn . boolToYesNo

printArrayWithSpace :: (Show a) => [a] -> IO ()
printArrayWithSpace = putStrLn . unwords . fmap show

printArrayWithLn :: (Show a) => [a] -> IO ()
printArrayWithLn = putStr . unlines . fmap show

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
