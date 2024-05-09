-- © 2024 Ishikawa-Taiki
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad (forM_, replicateM)
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

  putStrLn "------------------------------------"

  -- 1. 配列の添字範囲
  print $ bounds mat
  -- ((0,0),(3,3))

  -- 2. 配列中の値の一覧 (@listArray@ の第 2 引数を表示するとも言えます)
  print $ elems mat
  -- [3,1,4,1,5,9,2,6,5,3,5,8,9,7,9,3]
  {-
  [
  	3, 1, 4, 1,
  	5, 9, 2, 6,
  	5, 3, 5, 8,
  	9, 7, 9, 3
  ]
  -}

  -- 3. 配列中の (添字, 値) の一覧
  print $ assocs mat
  -- [((0,0),3),((0,1),1),((0,2),4),((0,3),1),((1,0),5),((1,1),9),((1,2),2),((1,3),6),((2,0),5),((2,1),3),((2,2),5),((2,3),8),((3,0),9),((3,1),7),((3,2),9),((3,3),3)]

  -- 4. bounds および assocs の組み合わせ
  print mat
  -- array ((0,0),(3,3)) [((0,0),3),((0,1),1),((0,2),4),((0,3),1),((1,0),5),((1,1),9),((1,2),2),((1,3),6),((2,0),5),((2,1),3),((2,2),5),((2,3),8),((3,0),9),((3,1),7),((3,2),9),((3,3),3)]

  putStrLn "------------------------------------"

  -- 各行の和を求める
  let rowSums = listArray @UArray (0, h - 1) $ [sum [mat ! (y, x) | x <- [0 .. w - 1]] | y <- [0 .. h - 1]]
  print rowSums
  -- array (0,3) [(0,9),(1,22),(2,21),(3,28)]

  -- 各列の和を求める
  let colSums = listArray @UArray (0, w - 1) $ [sum [mat ! (y, x) | y <- [0 .. h - 1]] | x <- [0 .. w - 1]]
  print colSums
  -- array (0,3) [(0,22),(1,20),(2,20),(3,18)]

  -- 各点における cross sum を求める
  let res = [rowSums ! y + colSums ! x - mat ! (y, x) | (y, x) <- range (bounds mat)]
  print res
  -- [28,28,25,26,39,33,40,34,38,38,36,31,41,41,39,43]

  putStrLn "------------------------------------"

--   -- @res@ を @w@ 個ずつ取り出す = 1 行ずつ取り出す
--   forM_ (chunksOf w res) $ \row -> do
--     -- 各行を表示する
--     putStrLn $ unwords (map show row)
