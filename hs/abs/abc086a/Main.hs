#!/usr/bin/env runghc

module Main (main) where

main :: IO ()
main = do
  [b, c] <- map read . words <$> getLine
  putStrLn $ intToString $ b * c

intToString :: Integer -> String
intToString b
    | even b = "Even"
    | odd b  = "Odd"
