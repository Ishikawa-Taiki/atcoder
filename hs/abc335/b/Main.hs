#!/usr/bin/env runghc
-- © 2024 Ishikawa-Taiki

module Main (main) where

main :: IO ()
main = do
  x <- read <$> getLine :: IO Int
  putStr $ solve x

solve :: Int -> String
solve n = foldl (\x y->x++"\n"++y) "" $ map toString [(x,y,z) | x <- [0..n], y <- [0..n], z <- [0..n], x+y+z <= n]

toString:: (Int,Int,Int) -> String
toString (x,y,z) = show x ++ " " ++ show y ++ " " ++ show z
