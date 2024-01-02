#!/usr/bin/env runghc
# © 2024 Ishikawa-Taiki

module Main (main) where

main :: IO ()
main = do
  xs <- map read . words <$> getLine :: IO [Int]
  print $ solve xs

solve :: [Int] -> Int
solve xs = undefined
