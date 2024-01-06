#!/usr/bin/env runghc
-- © 2024 Ishikawa-Taiki

module Main (main) where

main :: IO ()
main = do
  s <- getLine :: IO String
  putStrLn $ solve s

solve :: String -> String
solve s = init s ++ "4"
