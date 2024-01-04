#!/usr/bin/env runghc
-- © 2024 Ishikawa-Taiki
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main) where
import Data.List

main :: IO ()
main = do
  x <- read <$> getLine :: IO Int
  putStrLn $ foldr (\x y -> x ++ " " ++ y) "" $ show <$> solve [] [2,3..x]

-- エラトステネスの篩 を実装してみる
-- ttps://ja.wikipedia.org/wiki/エラトステネスの篩
-- 確定した素数リストと未確定リストを受け取り、素数リストを返す
solve :: [Int] -> [Int] -> [Int]
solve primes [] = primes
solve primes (x:xs) = sort $ solve (x:primes) [v | v<-xs, v `mod` x /= 0]
