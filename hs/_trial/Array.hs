-- © 2024 Ishikawa-Taiki
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad (replicateM)
import Data.Array (Array)
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.Ix

main :: IO ()
main = do
  testIArray

-- https://zenn.dev/toyboot4e/books/seriously-haskell/viewer/2-3-2-iarray
testIArray :: IO ()
testIArray = do
  let h = 4
      w = 4
      xs = [[3, 1, 4, 1], [5, 9, 2, 6], [5, 3, 5, 8], [9, 7, 9, 3]] :: [[Int]]
      mat = listArray @UArray ((0, 0), (h - 1, w - 1)) $ concat xs

  -- 1. 配列の添字範囲
  print $ bounds mat
  -- 2. 配列中の値の一覧 (@listArray@ の第 2 引数を表示するとも言えます)
  print $ elems mat
  -- 3. 配列中の (添字, 値) の一覧
  print $ assocs mat
  -- 4. bounds および assocs の組み合わせ
  print mat
