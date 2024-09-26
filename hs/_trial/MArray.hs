{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}

import Control.Monad -- `forM_` など
import Control.Monad.ST -- `ST` モナド
import Data.Array (Array)
import Data.Array.IArray
import Data.Array.IO -- `IOArray`, `IOUArray` (`MArray` のインスタンス)
import Data.Array.MArray -- 可変配列の API (型クラス `MArray`)
import Data.Array.ST -- `STArray`, `STUArray` (`MArray` のインスタンス)
import Data.Array.Unboxed (UArray)
import Debug.Trace (trace)

-- 参考にさせていただく
-- https://zenn.dev/toyboot4e/books/seriously-haskell/viewer/2-3-3-marray

main :: IO ()
main = do
  -- let xs = [1 .. 5]
  -- print $ testMArray (length xs) xs
  -- let xs2 = [1.0 :: Double .. 5.0]
  -- print $ testMArray2 (length xs2) xs2
  -- print $ testMArray3 (length xs) xs
  print bubbleSort

-- 累積和の計算
testMArray :: Int -> [Int] -> UArray Int Int
testMArray n xs = runSTUArray $ do
  arr <- newArray (0, n) 0

  forM_ (zip [0 .. n - 1] xs) $ \all@(!i, !dx) -> do
    let !_ = trace (show all) ()
    acc <- readArray arr i
    let !_ = trace (show acc) ()
    writeArray arr (i + 1) $! acc + dx

  return arr

{-
↑実行結果メモ
\*Main> :main
array
(0,1)
0
(1,2)
1
(2,3)
3
(3,4)
6
(4,5)
10
(0,5) [(0,0),(1,1),(2,3),(3,6),(4,10),(5,15)]
-}

-- 累積和の計算をNum eに拡張
testMArray2 :: (Num e, forall s. MArray (STUArray s) e (ST s)) => Int -> [e] -> UArray Int e
testMArray2 n xs = runSTUArray $ do
  arr <- newArray (0, n) 0

  forM_ (zip [0 .. n - 1] xs) $ \(!i, !dx) -> do
    x <- readArray arr i
    writeArray arr (i + 1) $! x + dx

  return arr

{-
↑実行結果メモ
array (0, 5) [(0, 0.0), (1, 1.0), (2, 3.0), (3, 6.0), (4, 10.0), (5, 15.0)]
-}

-- 累積和を計算した後に、結局和だけを返す関数
testMArray3 :: (Num e, forall s. MArray (STUArray s) e (ST s)) => Int -> [e] -> e
testMArray3 n xs = runST $ do
  arr <- asSTU $ newArray (0, n) 0

  forM_ (zip [0 .. n - 1] xs) $ \(!i, !dx) -> do
    x <- readArray arr i
    writeArray arr (i + 1) $! x + dx

  readArray arr n
  where
    asSTU :: ST s (STUArray s i e) -> ST s (STUArray s i e)
    asSTU = id -- 型推論上newArray がどの配列インスタンスを使用すれば良いかわからなくなるため必要

{-
↑実行結果メモ
15
-}

bubbleSort :: [Int]
bubbleSort =
  let base = [2, 5, 1, 4, 3]
   in elems $ f (length base) base
  where
    f :: Int -> [Int] -> UArray Int Int
    f n xs = runSTUArray $ do
      arr <- newListArray (1, n) xs :: ST s (STUArray s Int Int)

      forM_ [(i, j) | i <- [1 .. n - 1], j <- [1 .. n - i]] $ \(_, j) -> do
        x <- readArray arr j
        y <- readArray arr (j + 1)
        writeArray arr j $ min x y
        writeArray arr (j + 1) $ max x y

      return arr
