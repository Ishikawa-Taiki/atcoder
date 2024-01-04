#!/usr/bin/env runghc
-- © 2024 Ishikawa-Taiki
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main) where

main :: IO ()
main = do
  xs <- map read . words <$> getLine :: IO [Int]
  print $ solve xs

solve :: [Int] -> Int
solve (n:s:xs) = length $ [(r,b) | r<-[1..n], b<-[1..n], r+b<=s]
