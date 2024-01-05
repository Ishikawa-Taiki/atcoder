#!/usr/bin/env runghc
-- © 2024 Ishikawa-Taiki

module Main (main) where
import Data.Char (digitToInt)

main :: IO ()
main = do
  x <- read <$> getLine :: IO Int
  print $ solve x

solve :: Int -> Int
solve x = length $ filter (=='1') $ show x
