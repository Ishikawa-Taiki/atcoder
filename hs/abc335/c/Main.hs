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
  let result = solve n q querys
  -- 複数行の出力
  putStr $ unlines result

solve :: Int -> Int -> [[String]] -> [String]
solve n q querys =
  let history = reverse [(v,0) | v <- [1..n]] -- 計算量削減のため、最新の操作は後ろに追加していく
  in fst $ foldl execCommand ([], history) querys

execCommand :: ([String], [(Int, Int)]) -> [String] -> ([String], [(Int, Int)])
execCommand current (c1:c2:_) = createCommand c1 c2 current

-- コマンドと最新状況を受け取って、実行結果を返す
createCommand :: String -> String -> ([String], [(Int, Int)]) -> ([String], [(Int, Int)])
createCommand "1" "U" current = Data.Bifunctor.second (move (0, 1)) current
createCommand "1" "D" current = Data.Bifunctor.second (move (0, -1)) current
createCommand "1" "R" current = Data.Bifunctor.second (move (1, 0)) current
createCommand "1" "L" current = Data.Bifunctor.second (move (-1, 0)) current
createCommand "2" c2  current = Data.Bifunctor.first (++ [toString (read c2::Int) (snd current)]) current

-- x y の操作量と履歴を受け取って、履歴に操作結果を追加する
move :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
move (x, y) h = h ++ [Data.Bifunctor.bimap (+x) (+y) (last h)]

-- 指定パーツ位置と履歴を受け取って、該当パーツ位置の文字列表現を返却する
toString :: Int -> [(Int, Int)] -> String
toString n h =
  let part = reverse h !! (n-1)
  in show (fst part) ++ " " ++ show (snd part)
