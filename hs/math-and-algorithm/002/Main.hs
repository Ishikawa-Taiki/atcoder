#!/usr/bin/env runghc
-- © 2024 Ishikawa-Taiki

module Main (main) where

main :: IO ()
main = do
  xs <- map read . words <$> getLine :: IO [Int]
  print $ solve xs

solve :: [Int] -> Int
-- 自前
-- solve [] = 0
-- solve (x:xs) = x + solve xs
-- 書き直し1
-- solve xs = foldr (+) 0 xs
-- 書き直し2
-- solve = foldr (+) 0
-- 書き直し3
solve = sum
