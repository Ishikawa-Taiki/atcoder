#!/usr/bin/env runghc
-- © 2024 Ishikawa-Taiki

module Main (main) where

main :: IO ()
main = do
  [n, q] <- map read . words <$> getLine :: IO [Int]
  -- ns <- fmap (fmap read . words) . lines <$> getContents :: IO [String]
  -- print $ foldl (\x y->x++"\n"++y) "" ns
  -- 複数行、複数列の入力の受け取り
  ns <- map words . lines <$> getContents
  let commandList = map (\(x:y:xs)->Command x y) ns
  -- print $ solve commandList
  print $ firstDragon 5

solve :: [Command] -> [String]
solve commandList =
  let first = False
  in [""]

data Command = Command {
    x :: String,
    y :: String }
    deriving (Show)

data Vector2D a = Vector2D a a deriving (Show)
vinit :: (Num a) => a -> a -> Vector2D a
vinit = Vector2D
vup :: (Num a) => Vector2D a -> a -> Vector2D a
vup (Vector2D x y) n = Vector2D x (y+n)

type Dragon = [Vector2D Int]
firstDragon :: Int -> Dragon
firstDragon n = fmap (`vinit` 0) [1..n]
