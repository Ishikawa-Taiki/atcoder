-- © 2024 Ishikawa-Taiki
{-# LANGUAGE BlockArguments #-}

module Main (main) where

import Control.Monad.Fix (fix)

main :: IO ()
main = do
  printIOLoop -- loop : 0 loop : 1 loop : 2 loop : 3 loop : 4
  print sumPureLoop -- 5050

-- flip fix を用いた、IOを伴うループのサンプル
printIOLoop :: IO ()
printIOLoop = flip fix 0 \loop n ->
  if n == 5
    then pure ()
    else do
      putStr $ "loop : " ++ show n ++ " "
      loop (n + 1)

-- flip fix を用いた、純粋関数のループのサンプル
sumPureLoop :: Int
sumPureLoop = flip fix (0, 0) \loop (accum, n) ->
  if n == 100
    then accum + 100
    else loop (accum + n, n + 1)
