-- © 2024 Ishikawa-Taiki
module Main (main) where

main :: IO ()
main = do
  print $ arg1Test1 . arg1Test1 $ 5

arg1Test1 :: Int -> Int
arg1Test1 x = x + 1

arg1Test2 :: Int -> Int
arg1Test2 x = x + 2
