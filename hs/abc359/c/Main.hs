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
import Data.Maybe (fromJust)
import Debug.Trace (trace)

main :: IO ()
main = do
  (sx : sy : _) <- getLineToIntegerArray
  (tx : ty : _) <- getLineToIntegerArray
  print $ solve (tx, ty) ((sx, sy), 0)

solve :: (Integer, Integer) -> ((Integer, Integer), Integer) -> Integer
solve to@(toX, toY) (from@(fromX, fromY), cost) =
  let (eqX, eqY) = checkGoal to from
      afterX = if fromX < toX then fromX + 1 else fromX - 1
      afterY = if fromY < toY then fromY + 1 else fromY - 1
   in if eqX && eqY
        then cost
        else
          if not eqX
            then
              if checkHMoveCost to from
                then solve to ((afterX, fromY), cost) --　コストはかからず横移動(お得なので優先)
                else
                  if not eqY
                    then solve to ((fromX, afterY), cost + 1) --　コストをかけて縦移動(お得になるかもしれないので次に優先)
                    else solve to ((afterX, fromY), cost + 1) --　コストをかけて横移動(あとは横移動のみ、常にコストがかかる)
            else solve to ((fromX, afterY), cost + 1) --　コストをかけて縦移動(あとは縦移動のみ、常にコストがかかる)

-- ゴールか？(x,yの違い)
checkGoal :: (Integer, Integer) -> (Integer, Integer) -> (Bool, Bool)
checkGoal to@(toX, toY) from@(fromX, fromY) = (toX == fromX, toY == fromY)

-- 横移動のコストがゼロか？(横の移動が必要な時を前提で計算)
checkHMoveCost :: (Integer, Integer) -> (Integer, Integer) -> Bool
checkHMoveCost to@(toX, _) from@(fromX, fromY) =
  if even fromY
    then fromX `div` 2 == toX `div` 2
    else (fromX + 1) `div` 2 == (toX + 1) `div` 2

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
