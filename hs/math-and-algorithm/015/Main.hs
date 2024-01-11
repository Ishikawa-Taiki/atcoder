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
import qualified Data.List as L
import Data.Maybe (fromJust)
import Data.Set (fromList, toList)
import Debug.Trace (trace)
import GHC.Float (int2Float)

main :: IO ()
main = do
  (a, b) <- getLineToIntTuple2
  print $ solve a b

solve :: Int -> Int -> Int
solve = gcd

{- Library -}
-- データ変換共通
arrayToTuple2 :: [a] -> (a, a)
arrayToTuple2 (a : b : _) = (a, b)

bsToInt :: ByteString -> Int
bsToInt = fst . fromJust . BS.readInt

bsToIntList :: ByteString -> [Int]
bsToIntList = fmap bsToInt . BS.words

bsToIntTuple2 :: ByteString -> (Int, Int)
bsToIntTuple2 = arrayToTuple2 . bsToIntList

bsToIntTuples2 :: ByteString -> [(Int, Int)]
bsToIntTuples2 = fmap (arrayToTuple2 . bsToIntList) . BS.lines

-- IO 入力系
getLineToIntTuple2 :: IO (Int, Int)
getLineToIntTuple2 = bsToIntTuple2 <$> BS.getLine

-- デバッグ用
#ifndef ATCODER
debug :: (Show a) => String -> a -> ()
debug key value = trace (key ++ " : " ++ show value) ()
#else
debug :: (Show a) => String -> a -> ()
debug _ _ = ()
#endif
