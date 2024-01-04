#!/usr/bin/env runghc
-- © 2024 Ishikawa-Taiki

module Main (main) where

main :: IO ()
main = do
  x <- read <$> getLine :: IO Int
  print $ solve x

solve :: Int -> Int
solve x = 2 * x + 3
