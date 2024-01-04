#!/usr/bin/env runghc
-- © 2024 Ishikawa-Taiki
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main) where

main :: IO ()
main = do
  n:s:xs <- map read . words <$> getLine :: IO [Int]
  a <- map read . words <$> getLine :: IO [Int]
  putStr $ toString $ solve 0 s a

-- 現状の合計数と目標とする値から、候補となるカードが存在するかを返す
solve :: Int -> Int -> [Int] -> Bool
solve _ _ [] = False
solve current sum (item:list)
 | current + item == sum = True
 -- 合計が超えちゃうならそのカードは使わないで存在するか確認
 | current + item > sum = solve current sum list
 -- 超えないならそのカードを使うパターンと使わないパターンを両方確認
 | current + item < sum = solve (current+item) sum list || solve current sum list

toString :: Bool -> String
toString True = "Yes"
toString False = "No"
