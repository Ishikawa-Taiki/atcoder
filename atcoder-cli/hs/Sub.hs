{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Redundant flip" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas -Wno-incomplete-patterns -Wno-unused-imports -Wno-unused-top-binds -Wno-name-shadowing -Wno-unused-matches #-}

-- © 2024 Ishikawa-Taiki

import Data.Fixed (Fixed, HasResolution (resolution), showFixed)
import Data.Set (fromList, toList)
import GHC.Float (int2Float)

{- Library -}
-- 便利関数系
-- 素数判定
isPrime :: Int -> Bool
isPrime n
  | n <= 2 = True
  | otherwise =
    let max = ceiling . sqrt $ int2Float n
     in null [i | i <- [2, 3 .. max], n `mod` i == 0]

-- 約数列挙
enumerateDivisor :: Int -> [Int]
enumerateDivisor n = do
  let max = ceiling . sqrt $ int2Float n
  toList . fromList $ concat [[x, y] | x <- [1 .. max], n `mod` x == 0, let y = n `div` x]

-- nCr は 組み合わせ (combination)　の計算
nCr :: Int -> Int -> Int
nCr n r =
  let numerator = product $ take r [n, n -1 ..]
      denominator = product $ take r [1 ..]
   in numerator `div` denominator

-- 精度の高い少数型定義
-- https://hackage.haskell.org/package/base-4.14.3.0/docs/Data-Fixed.html#t:Pico
data E100 = E100

instance HasResolution E100 where
  resolution _ =
    10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000

type TypeE100 = Fixed E100

-- 回文かどうかをチェックする
checkPalindrome :: String -> Bool
checkPalindrome target =
  let targetLength = length target
      headString = take (targetLength `div` 2) target
      tailString = drop (ceiling (fromIntegral targetLength / 2)) target
   in headString == reverse tailString

-- 二次元平面で回転を行った時の座標値を取得する(近似値)
-- x軸が右向き/y軸が上向きの xy 座標平面において、反時計回りに theta 度回転させたxy座標を得る
rotate :: Int -> (Double, Double) -> (Double, Double)
rotate theta (srcX, srcY) =
  let sinTheta = sin (fromIntegral theta * (pi / 180.0))
      cosTheta = cos (fromIntegral theta * (pi / 180.0))
      dstX = (srcX * cosTheta) - (srcY * sinTheta)
      dstY = (srcX * sinTheta) + (srcY * cosTheta)
   in (dstX, dstY)
