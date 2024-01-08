#!/usr/bin/env runghc
-- © 2024 Ishikawa-Taiki
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main) where
import Data.Array
import qualified Data.Bifunctor

main :: IO ()
main = do
  [n, q] <- map read . words <$> getLine :: IO [Int]
  -- 複数行、複数列の入力の受け取り
  querys <- map words . lines <$> getContents
  print $ solve n q querys

solve :: Int -> Int -> [[String]] -> [String]
solve n q querys =
  let history = reverse [(v,0) | v <- [1..n]] -- 計算量削減のため、最新の操作は後ろに追加していく
  in fmap show $ fst $ foldl execCommand ([], history) querys

execCommand :: ([String], [(Int, Int)]) -> [String] -> ([String], [(Int, Int)])
execCommand current (c1:c2:_) = createCommand c1 c2 current

-- コマンドと最新状況を受け取って、実行結果を返す
createCommand :: String -> String -> ([String], [(Int, Int)]) -> ([String], [(Int, Int)])
createCommand "1" "U" current = Data.Bifunctor.second u current
createCommand "1" "D" current = Data.Bifunctor.second d current
createCommand "1" "R" current = Data.Bifunctor.second r current
createCommand "1" "L" current = Data.Bifunctor.second l current
createCommand "2" c2  current = (fst current ++ [toString (read c2::Int) (snd current)], snd current)

move :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
move (x, y) h = h ++ [(fst (last h) + x, snd (last h) + y)]
u = move (0, 1)
d = move (0, -1)
l = move (-1, 0)
r = move (1, 0)

toString :: Int -> [(Int, Int)] -> String
toString n h = show $ reverse h !! (n-1)
