#!/usr/bin/env runghc
-- © 2024 Ishikawa-Taiki
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main) where
import Data.List
import GHC.Float

main :: IO ()
main = do
  x <- read <$> getLine :: IO Int
  -- print $ solve [] [2,3..10^13] x
  print $ solve x

-- 011の関数を再利用して改造
-- solve :: [Int] -> [Int] -> Int -> Bool
-- solve _ _ 2 = False
-- solve _ [] _ = False
-- solve primes (x:xs) target
--   | x > target = False
--   | otherwise = solve (x:primes) [v | v<-xs, odd v, v `mod` x /= 0] target

-- 参考作業中
-- https://qiita.com/asksaito/items/76b71602dd956b79dbf7
solve :: Int -> Bool
solve x = True
-- solve x
--   | x == 2 = True
--   | even x = False
--   | otherwise = 
--     let sqrtv = sqrtDouble $ int2Double x
--       result = find (\x -> sqrtv `mod` x /= 0) [3,5..sqrtv]
--     in False
