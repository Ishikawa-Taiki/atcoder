{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- © 2024 Ishikawa-Taiki
module Main (main) where

import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char (isSpace)
import qualified Data.List as L
import Data.Maybe (fromJust)
import Debug.Trace (trace, traceShow)
import GHC.Float (int2Double, int2Float)

main :: IO ()
main = do
  x <- getLineToInt
  printYesNo $ solve x

solve :: Int -> Bool
solve n
  | n <= 2 = True
  | otherwise =
    let max = ceiling . sqrt $ int2Float n
     in null [i | i <- [2, 3 .. max], n `mod` i == 0]

{- Library -}
-- データ変換共通
boolToYesNo :: Bool -> String
boolToYesNo = bool "No" "Yes"

bsToInt :: ByteString -> Int
bsToInt = fst . fromJust . BS.readInt

-- IO 出力系
printYesNo :: Bool -> IO ()
printYesNo = putStrLn . boolToYesNo

-- IO 入力系
getLineToInt :: IO Int
getLineToInt = bsToInt <$> BS.getLine
