{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

-- © 2024 Ishikawa-Taiki
module Main (main) where

import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char (isSpace)
import Data.List (elemIndex)
import qualified Data.List as L
import Data.Maybe (fromJust)
import Data.Set (fromList, toList)
import Debug.Trace (trace)
import GHC.Float (int2Float)
import Maybes (listToMaybe)

main :: IO ()
main = do
  n <- getLineToInt
  printArrayWithSpace $ solve [] n

solve :: [Int] -> Int -> [Int]
solve list 1 = list
solve list current =
  let firstPrime = listToMaybe . take 1 . tail $ enumerateDivisor current
   in case firstPrime of
        Nothing -> [current]
        Just p -> p : solve list (current `div` p)

{- Library -}
-- データ変換共通
bsToInt :: ByteString -> Int
bsToInt = fst . fromJust . BS.readInt

-- IO 出力系
printArrayWithSpace :: (Show a) => [a] -> IO ()
printArrayWithSpace = putStrLn . unwords . fmap show

-- IO 入力系
getLineToInt :: IO Int
getLineToInt = bsToInt <$> BS.getLine

-- 便利関数系
-- 約数列挙
enumerateDivisor :: Int -> [Int]
enumerateDivisor n = do
  let max = ceiling . sqrt $ int2Float n
  toList . fromList $ concat [[x, y] | x <- [1 .. max], n `mod` x == 0, let y = n `div` x]
