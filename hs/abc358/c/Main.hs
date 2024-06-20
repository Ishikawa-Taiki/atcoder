{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Redundant flip" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas -Wno-incomplete-patterns -Wno-unused-imports -Wno-unused-top-binds -Wno-name-shadowing -Wno-unused-matches #-}

-- © 2024 Ishikawa-Taiki
module Main (main) where

import Data.Array.Unboxed (IArray (bounds), Ix (range), UArray, listArray, (!), (//))
import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.List (find, subsequences, transpose)
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Debug.Trace (trace)

main :: IO ()
main = do
  (n, m) <- getLineToIntTuple2
  xs <- getContentsToStringArray
  print $ solve xs n m

solve :: [String] -> Int -> Int -> Int
solve xs n m =
  let base = debugProxy $ tail . subsequences $ xs
      orCheckList = debugProxy $ transpose <$> base
      resultPetterns = debugProxy $ find (all (elem 'o')) orCheckList
   in fromJust $ length . head <$> resultPetterns

{-
実行時のサンプルログ

[INFO] online - judge - tools 11.5 . 1 (+ online - judge - api - client 10.10 . 1)
[INFO] 3 cases found

[INFO] sample -1
[DebugProxy] : [["oooxx"], ["xooox"], ["oooxx", "xooox"], ["xxooo"], ["oooxx", "xxooo"], ["xooox", "xxooo"], ["oooxx", "xooox", "xxooo"]]
[DebugProxy] : [["o", "o", "o", "x", "x"], ["x", "o", "o", "o", "x"], ["ox", "oo", "oo", "xo", "xx"], ["x", "x", "o", "o", "o"], ["ox", "ox", "oo", "xo", "xo"], ["xx", "ox", "oo", "oo", "xo"], ["oxx", "oox", "ooo", "xoo", "xxo"]]
[DebugProxy] : Just ["ox", "ox", "oo", "xo", "xo"]
[INFO] time : 0.554279 sec
[INFO] memory : 118.928000 MB
[SUCCESS] AC

[INFO] sample -2
[DebugProxy] : [["oo"], ["ox"], ["oo", "ox"], ["xo"], ["oo", "xo"], ["ox", "xo"], ["oo", "ox", "xo"]]
[DebugProxy] : [["o", "o"], ["o", "x"], ["oo", "ox"], ["x", "o"], ["ox", "oo"], ["ox", "xo"], ["oox", "oxo"]]
[DebugProxy] : Just ["o", "o"]
[INFO] time : 0.505824 sec
[INFO] memory : 118.944000 MB
[SUCCESS] AC

[INFO] sample -3
[DebugProxy] : [["xxoxxo"], ["xxoxxx"], ["xxoxxo", "xxoxxx"], ["xoxxxx"], ["xxoxxo", "xoxxxx"], ["xxoxxx", "xoxxxx"], ["xxoxxo", "xxoxxx", "xoxxxx"], ["xxxoxx"], ["xxoxxo", "xxxoxx"], ["xxoxxx", "xxxoxx"], ["xxoxxo", "xxoxxx", "xxxoxx"], ["xoxxxx", "xxxoxx"], ["xxoxxo", "xoxxxx", "xxxoxx"], ["xxoxxx", "xoxxxx", "xxxoxx"], ["xxoxxo", "xxoxxx", "xoxxxx", "xxxoxx"], ["xxoooo"], ["xxoxxo", "xxoooo"], ["xxoxxx", "xxoooo"], ["xxoxxo", "xxoxxx", "xxoooo"], ["xoxxxx", "xxoooo"], ["xxoxxo", "xoxxxx", "xxoooo"], ["xxoxxx", "xoxxxx", "xxoooo"], ["xxoxxo", "xxoxxx", "xoxxxx", "xxoooo"], ["xxxoxx", "xxoooo"], ["xxoxxo", "xxxoxx", "xxoooo"], ["xxoxxx", "xxxoxx", "xxoooo"], ["xxoxxo", "xxoxxx", "xxxoxx", "xxoooo"], ["xoxxxx", "xxxoxx", "xxoooo"], ["xxoxxo", "xoxxxx", "xxxoxx", "xxoooo"], ["xxoxxx", "xoxxxx", "xxxoxx", "xxoooo"], ["xxoxxo", "xxoxxx", "xoxxxx", "xxxoxx", "xxoooo"], ["xxxxox"], ["xxoxxo", "xxxxox"], ["xxoxxx", "xxxxox"], ["xxoxxo", "xxoxxx", "xxxxox"], ["xoxxxx", "xxxxox"], ["xxoxxo", "xoxxxx", "xxxxox"], ["xxoxxx", "xoxxxx", "xxxxox"], ["xxoxxo", "xxoxxx", "xoxxxx", "xxxxox"], ["xxxoxx", "xxxxox"], ["xxoxxo", "xxxoxx", "xxxxox"], ["xxoxxx", "xxxoxx", "xxxxox"], ["xxoxxo", "xxoxxx", "xxxoxx", "xxxxox"], ["xoxxxx", "xxxoxx", "xxxxox"], ["xxoxxo", "xoxxxx", "xxxoxx", "xxxxox"], ["xxoxxx", "xoxxxx", "xxxoxx", "xxxxox"], ["xxoxxo", "xxoxxx", "xoxxxx", "xxxoxx", "xxxxox"], ["xxoooo", "xxxxox"], ["xxoxxo", "xxoooo", "xxxxox"], ["xxoxxx", "xxoooo", "xxxxox"], ["xxoxxo", "xxoxxx", "xxoooo", "xxxxox"], ["xoxxxx", "xxoooo", "xxxxox"], ["xxoxxo", "xoxxxx", "xxoooo", "xxxxox"], ["xxoxxx", "xoxxxx", "xxoooo", "xxxxox"], ["xxoxxo", "xxoxxx", "xoxxxx", "xxoooo", "xxxxox"], ["xxxoxx", "xxoooo", "xxxxox"], ["xxoxxo", "xxxoxx", "xxoooo", "xxxxox"], ["xxoxxx", "xxxoxx", "xxoooo", "xxxxox"], ["xxoxxo", "xxoxxx", "xxxoxx", "xxoooo", "xxxxox"], ["xoxxxx", "xxxoxx", "xxoooo", "xxxxox"], ["xxoxxo", "xoxxxx", "xxxoxx", "xxoooo", "xxxxox"], ["xxoxxx", "xoxxxx", "xxxoxx", "xxoooo", "xxxxox"], ["xxoxxo", "xxoxxx", "xoxxxx", "xxxoxx", "xxoooo", "xxxxox"], ["xoxxox"], ["xxoxxo", "xoxxox"], ["xxoxxx", "xoxxox"], ["xxoxxo", "xxoxxx", "xoxxox"], ["xoxxxx", "xoxxox"], ["xxoxxo", "xoxxxx", "xoxxox"], ["xxoxxx", "xoxxxx", "xoxxox"], ["xxoxxo", "xxoxxx", "xoxxxx", "xoxxox"], ["xxxoxx", "xoxxox"], ["xxoxxo", "xxxoxx", "xoxxox"], ["xxoxxx", "xxxoxx", "xoxxox"], ["xxoxxo", "xxoxxx", "xxxoxx", "xoxxox"], ["xoxxxx", "xxxoxx", "xoxxox"], ["xxoxxo", "xoxxxx", "xxxoxx", "xoxxox"], ["xxoxxx", "xoxxxx", "xxxoxx", "xoxxox"], ["xxoxxo", "xxoxxx", "xoxxxx", "xxxoxx", "xoxxox"], ["xxoooo", "xoxxox"], ["xxoxxo", "xxoooo", "xoxxox"], ["xxoxxx", "xxoooo", "xoxxox"], ["xxoxxo", "xxoxxx", "xxoooo", "xoxxox"], ["xoxxxx", "xxoooo", "xoxxox"], ["xxoxxo", "xoxxxx", "xxoooo", "xoxxox"], ["xxoxxx", "xoxxxx", "xxoooo", "xoxxox"], ["xxoxxo", "xxoxxx", "xoxxxx", "xxoooo", "xoxxox"], ["xxxoxx", "xxoooo", "xoxxox"], ["xxoxxo", "xxxoxx", "xxoooo", "xoxxox"], ["xxoxxx", "xxxoxx", "xxoooo", "xoxxox"], ["xxoxxo", "xxoxxx", "xxxoxx", "xxoooo", "xoxxox"], ["xoxxxx", "xxxoxx", "xxoooo", "xoxxox"], ["xxoxxo", "xoxxxx", "xxxoxx", "xxoooo", "xoxxox"], ["xxoxxx", "xoxxxx", "xxxoxx", "xxoooo", "xoxxox"], ["xxoxxo", "xxoxxx", "xoxxxx", "xxxoxx", "xxoooo", "xoxxox"], ["xxxxox", "xoxxox"], ["xxoxxo", "xxxxox", "xoxxox"], ["xxoxxx", "xxxxox", "xoxxox"], ["xxoxxo", "xxoxxx", "xxxxox", "xoxxox"], ["xoxxxx", "xxxxox", "xoxxox"], ["xxoxxo", "xoxxxx", "xxxxox", "xoxxox"], ["xxoxxx", "xoxxxx", "xxxxox", "xoxxox"], ["xxoxxo", "xxoxxx", "xoxxxx", "xxxxox", "xoxxox"], ["xxxoxx", "xxxxox", "xoxxox"], ["xxoxxo", "xxxoxx", "xxxxox", "xoxxox"], ["xxoxxx", "xxxoxx", "xxxxox", "xoxxox"], ["xxoxxo", "xxoxxx", "xxxoxx", "xxxxox", "xoxxox"], ["xoxxxx", "xxxoxx", "xxxxox", "xoxxox"], ["xxoxxo", "xoxxxx", "xxxoxx", "xxxxox", "xoxxox"], ["xxoxxx", "xoxxxx", "xxxoxx", "xxxxox", "xoxxox"], ["xxoxxo", "xxoxxx", "xoxxxx", "xxxoxx", "xxxxox", "xoxxox"], ["xxoooo", "xxxxox", "xoxxox"], ["xxoxxo", "xxoooo", "xxxxox", "xoxxox"], ["xxoxxx", "xxoooo", "xxxxox", "xoxxox"], ["xxoxxo", "xxoxxx", "xxoooo", "xxxxox", "xoxxox"], ["xoxxxx", "xxoooo", "xxxxox", "xoxxox"], ["xxoxxo", "xoxxxx", "xxoooo", "xxxxox", "xoxxox"], ["xxoxxx", "xoxxxx", "xxoooo", "xxxxox", "xoxxox"], ["xxoxxo", "xxoxxx", "xoxxxx", "xxoooo", "xxxxox", "xoxxox"], ["xxxoxx", "xxoooo", "xxxxox", "xoxxox"], ["xxoxxo", "xxxoxx", "xxoooo", "xxxxox", "xoxxox"], ["xxoxxx", "xxxoxx", "xxoooo", "xxxxox", "xoxxox"], ["xxoxxo", "xxoxxx", "xxxoxx", "xxoooo", "xxxxox", "xoxxox"], ["xoxxxx", "xxxoxx", "xxoooo", "xxxxox", "xoxxox"], ["xxoxxo", "xoxxxx", "xxxoxx", "xxoooo", "xxxxox", "xoxxox"], ["xxoxxx", "xoxxxx", "xxxoxx", "xxoooo", "xxxxox", "xoxxox"], ["xxoxxo", "xxoxxx", "xoxxxx", "xxxoxx", "xxoooo", "xxxxox", "xoxxox"], ["oxoxxo"], ["xxoxxo", "oxoxxo"], ["xxoxxx", "oxoxxo"], ["xxoxxo", "xxoxxx", "oxoxxo"], ["xoxxxx", "oxoxxo"], ["xxoxxo", "xoxxxx", "oxoxxo"], ["xxoxxx", "xoxxxx", "oxoxxo"], ["xxoxxo", "xxoxxx", "xoxxxx", "oxoxxo"], ["xxxoxx", "oxoxxo"], ["xxoxxo", "xxxoxx", "oxoxxo"], ["xxoxxx", "xxxoxx", "oxoxxo"], ["xxoxxo", "xxoxxx", "xxxoxx", "oxoxxo"], ["xoxxxx", "xxxoxx", "oxoxxo"], ["xxoxxo", "xoxxxx", "xxxoxx", "oxoxxo"], ["xxoxxx", "xoxxxx", "xxxoxx", "oxoxxo"], ["xxoxxo", "xxoxxx", "xoxxxx", "xxxoxx", "oxoxxo"], ["xxoooo", "oxoxxo"], ["xxoxxo", "xxoooo", "oxoxxo"], ["xxoxxx", "xxoooo", "oxoxxo"], ["xxoxxo", "xxoxxx", "xxoooo", "oxoxxo"], ["xoxxxx", "xxoooo", "oxoxxo"], ["xxoxxo", "xoxxxx", "xxoooo", "oxoxxo"], ["xxoxxx", "xoxxxx", "xxoooo", "oxoxxo"], ["xxoxxo", "xxoxxx", "xoxxxx", "xxoooo", "oxoxxo"], ["xxxoxx", "xxoooo", "oxoxxo"], ["xxoxxo", "xxxoxx", "xxoooo", "oxoxxo"], ["xxoxxx", "xxxoxx", "xxoooo", "oxoxxo"], ["xxoxxo", "xxoxxx", "xxxoxx", "xxoooo", "oxoxxo"], ["xoxxxx", "xxxoxx", "xxoooo", "oxoxxo"], ["xxoxxo", "xoxxxx", "xxxoxx", "xxoooo", "oxoxxo"], ["xxoxxx", "xoxxxx", "xxxoxx", "xxoooo", "oxoxxo"], ["xxoxxo", "xxoxxx", "xoxxxx", "xxxoxx", "xxoooo", "oxoxxo"], ["xxxxox", "oxoxxo"], ["xxoxxo", "xxxxox", "oxoxxo"], ["xxoxxx", "xxxxox", "oxoxxo"], ["xxoxxo", "xxoxxx", "xxxxox", "oxoxxo"], ["xoxxxx", "xxxxox", "oxoxxo"], ["xxoxxo", "xoxxxx", "xxxxox", "oxoxxo"], ["xxoxxx", "xoxxxx", "xxxxox", "oxoxxo"], ["xxoxxo", "xxoxxx", "xoxxxx", "xxxxox", "oxoxxo"], ["xxxoxx", "xxxxox", "oxoxxo"], ["xxoxxo", "xxxoxx", "xxxxox", "oxoxxo"], ["xxoxxx", "xxxoxx", "xxxxox", "oxoxxo"], ["xxoxxo", "xxoxxx", "xxxoxx", "xxxxox", "oxoxxo"], ["xoxxxx", "xxxoxx", "xxxxox", "oxoxxo"], ["xxoxxo", "xoxxxx", "xxxoxx", "xxxxox", "oxoxxo"], ["xxoxxx", "xoxxxx", "xxxoxx", "xxxxox", "oxoxxo"], ["xxoxxo", "xxoxxx", "xoxxxx", "xxxoxx", "xxxxox", "oxoxxo"], ["xxoooo", "xxxxox", "oxoxxo"], ["xxoxxo", "xxoooo", "xxxxox", "oxoxxo"], ["xxoxxx", "xxoooo", "xxxxox", "oxoxxo"], ["xxoxxo", "xxoxxx", "xxoooo", "xxxxox", "oxoxxo"], ["xoxxxx", "xxoooo", "xxxxox", "oxoxxo"], ["xxoxxo", "xoxxxx", "xxoooo", "xxxxox", "oxoxxo"], ["xxoxxx", "xoxxxx", "xxoooo", "xxxxox", "oxoxxo"], ["xxoxxo", "xxoxxx", "xoxxxx", "xxoooo", "xxxxox", "oxoxxo"], ["xxxoxx", "xxoooo", "xxxxox", "oxoxxo"], ["xxoxxo", "xxxoxx", "xxoooo", "xxxxox", "oxoxxo"], ["xxoxxx", "xxxoxx", "xxoooo", "xxxxox", "oxoxxo"], ["xxoxxo", "xxoxxx", "xxxoxx", "xxoooo", "xxxxox", "oxoxxo"], ["xoxxxx", "xxxoxx", "xxoooo", "xxxxox", "oxoxxo"], ["xxoxxo", "xoxxxx", "xxxoxx", "xxoooo", "xxxxox", "oxoxxo"], ["xxoxxx", "xoxxxx", "xxxoxx", "xxoooo", "xxxxox", "oxoxxo"], ["xxoxxo", "xxoxxx", "xoxxxx", "xxxoxx", "xxoooo", "xxxxox", "oxoxxo"], ["xoxxox", "oxoxxo"], ["xxoxxo", "xoxxox", "oxoxxo"], ["xxoxxx", "xoxxox", "oxoxxo"], ["xxoxxo", "xxoxxx", "xoxxox", "oxoxxo"], ["xoxxxx", "xoxxox", "oxoxxo"], ["xxoxxo", "xoxxxx", "xoxxox", "oxoxxo"], ["xxoxxx", "xoxxxx", "xoxxox", "oxoxxo"], ["xxoxxo", "xxoxxx", "xoxxxx", "xoxxox", "oxoxxo"], ["xxxoxx", "xoxxox", "oxoxxo"], ["xxoxxo", "xxxoxx", "xoxxox", "oxoxxo"], ["xxoxxx", "xxxoxx", "xoxxox", "oxoxxo"], ["xxoxxo", "xxoxxx", "xxxoxx", "xoxxox", "oxoxxo"], ["xoxxxx", "xxxoxx", "xoxxox", "oxoxxo"], ["xxoxxo", "xoxxxx", "xxxoxx", "xoxxox", "oxoxxo"], ["xxoxxx", "xoxxxx", "xxxoxx", "xoxxox", "oxoxxo"], ["xxoxxo", "xxoxxx", "xoxxxx", "xxxoxx", "xoxxox", "oxoxxo"], ["xxoooo", "xoxxox", "oxoxxo"], ["xxoxxo", "xxoooo", "xoxxox", "oxoxxo"], ["xxoxxx", "xxoooo", "xoxxox", "oxoxxo"], ["xxoxxo", "xxoxxx", "xxoooo", "xoxxox", "oxoxxo"], ["xoxxxx", "xxoooo", "xoxxox", "oxoxxo"], ["xxoxxo", "xoxxxx", "xxoooo", "xoxxox", "oxoxxo"], ["xxoxxx", "xoxxxx", "xxoooo", "xoxxox", "oxoxxo"], ["xxoxxo", "xxoxxx", "xoxxxx", "xxoooo", "xoxxox", "oxoxxo"], ["xxxoxx", "xxoooo", "xoxxox", "oxoxxo"], ["xxoxxo", "xxxoxx", "xxoooo", "xoxxox", "oxoxxo"], ["xxoxxx", "xxxoxx", "xxoooo", "xoxxox", "oxoxxo"], ["xxoxxo", "xxoxxx", "xxxoxx", "xxoooo", "xoxxox", "oxoxxo"], ["xoxxxx", "xxxoxx", "xxoooo", "xoxxox", "oxoxxo"], ["xxoxxo", "xoxxxx", "xxxoxx", "xxoooo", "xoxxox", "oxoxxo"], ["xxoxxx", "xoxxxx", "xxxoxx", "xxoooo", "xoxxox", "oxoxxo"], ["xxoxxo", "xxoxxx", "xoxxxx", "xxxoxx", "xxoooo", "xoxxox", "oxoxxo"], ["xxxxox", "xoxxox", "oxoxxo"], ["xxoxxo", "xxxxox", "xoxxox", "oxoxxo"], ["xxoxxx", "xxxxox", "xoxxox", "oxoxxo"], ["xxoxxo", "xxoxxx", "xxxxox", "xoxxox", "oxoxxo"], ["xoxxxx", "xxxxox", "xoxxox", "oxoxxo"], ["xxoxxo", "xoxxxx", "xxxxox", "xoxxox", "oxoxxo"], ["xxoxxx", "xoxxxx", "xxxxox", "xoxxox", "oxoxxo"], ["xxoxxo", "xxoxxx", "xoxxxx", "xxxxox", "xoxxox", "oxoxxo"], ["xxxoxx", "xxxxox", "xoxxox", "oxoxxo"], ["xxoxxo", "xxxoxx", "xxxxox", "xoxxox", "oxoxxo"], ["xxoxxx", "xxxoxx", "xxxxox", "xoxxox", "oxoxxo"], ["xxoxxo", "xxoxxx", "xxxoxx", "xxxxox", "xoxxox", "oxoxxo"], ["xoxxxx", "xxxoxx", "xxxxox", "xoxxox", "oxoxxo"], ["xxoxxo", "xoxxxx", "xxxoxx", "xxxxox", "xoxxox", "oxoxxo"], ["xxoxxx", "xoxxxx", "xxxoxx", "xxxxox", "xoxxox", "oxoxxo"], ["xxoxxo", "xxoxxx", "xoxxxx", "xxxoxx", "xxxxox", "xoxxox", "oxoxxo"], ["xxoooo", "xxxxox", "xoxxox", "oxoxxo"], ["xxoxxo", "xxoooo", "xxxxox", "xoxxox", "oxoxxo"], ["xxoxxx", "xxoooo", "xxxxox", "xoxxox", "oxoxxo"], ["xxoxxo", "xxoxxx", "xxoooo", "xxxxox", "xoxxox", "oxoxxo"], ["xoxxxx", "xxoooo", "xxxxox", "xoxxox", "oxoxxo"], ["xxoxxo", "xoxxxx", "xxoooo", "xxxxox", "xoxxox", "oxoxxo"], ["xxoxxx", "xoxxxx", "xxoooo", "xxxxox", "xoxxox", "oxoxxo"], ["xxoxxo", "xxoxxx", "xoxxxx", "xxoooo", "xxxxox", "xoxxox", "oxoxxo"], ["xxxoxx", "xxoooo", "xxxxox", "xoxxox", "oxoxxo"], ["xxoxxo", "xxxoxx", "xxoooo", "xxxxox", "xoxxox", "oxoxxo"], ["xxoxxx", "xxxoxx", "xxoooo", "xxxxox", "xoxxox", "oxoxxo"], ["xxoxxo", "xxoxxx", "xxxoxx", "xxoooo", "xxxxox", "xoxxox", "oxoxxo"], ["xoxxxx", "xxxoxx", "xxoooo", "xxxxox", "xoxxox", "oxoxxo"], ["xxoxxo", "xoxxxx", "xxxoxx", "xxoooo", "xxxxox", "xoxxox", "oxoxxo"], ["xxoxxx", "xoxxxx", "xxxoxx", "xxoooo", "xxxxox", "xoxxox", "oxoxxo"], ["xxoxxo", "xxoxxx", "xoxxxx", "xxxoxx", "xxoooo", "xxxxox", "xoxxox", "oxoxxo"]]
[DebugProxy] : [["x", "x", "o", "x", "x", "o"], ["x", "x", "o", "x", "x", "x"], ["xx", "xx", "oo", "xx", "xx", "ox"], ["x", "o", "x", "x", "x", "x"], ["xx", "xo", "ox", "xx", "xx", "ox"], ["xx", "xo", "ox", "xx", "xx", "xx"], ["xxx", "xxo", "oox", "xxx", "xxx", "oxx"], ["x", "x", "x", "o", "x", "x"], ["xx", "xx", "ox", "xo", "xx", "ox"], ["xx", "xx", "ox", "xo", "xx", "xx"], ["xxx", "xxx", "oox", "xxo", "xxx", "oxx"], ["xx", "ox", "xx", "xo", "xx", "xx"], ["xxx", "xox", "oxx", "xxo", "xxx", "oxx"], ["xxx", "xox", "oxx", "xxo", "xxx", "xxx"], ["xxxx", "xxox", "ooxx", "xxxo", "xxxx", "oxxx"], ["x", "x", "o", "o", "o", "o"], ["xx", "xx", "oo", "xo", "xo", "oo"], ["xx", "xx", "oo", "xo", "xo", "xo"], ["xxx", "xxx", "ooo", "xxo", "xxo", "oxo"], ["xx", "ox", "xo", "xo", "xo", "xo"], ["xxx", "xox", "oxo", "xxo", "xxo", "oxo"], ["xxx", "xox", "oxo", "xxo", "xxo", "xxo"], ["xxxx", "xxox", "ooxo", "xxxo", "xxxo", "oxxo"], ["xx", "xx", "xo", "oo", "xo", "xo"], ["xxx", "xxx", "oxo", "xoo", "xxo", "oxo"], ["xxx", "xxx", "oxo", "xoo", "xxo", "xxo"], ["xxxx", "xxxx", "ooxo", "xxoo", "xxxo", "oxxo"], ["xxx", "oxx", "xxo", "xoo", "xxo", "xxo"], ["xxxx", "xoxx", "oxxo", "xxoo", "xxxo", "oxxo"], ["xxxx", "xoxx", "oxxo", "xxoo", "xxxo", "xxxo"], ["xxxxx", "xxoxx", "ooxxo", "xxxoo", "xxxxo", "oxxxo"], ["x", "x", "x", "x", "o", "x"], ["xx", "xx", "ox", "xx", "xo", "ox"], ["xx", "xx", "ox", "xx", "xo", "xx"], ["xxx", "xxx", "oox", "xxx", "xxo", "oxx"], ["xx", "ox", "xx", "xx", "xo", "xx"], ["xxx", "xox", "oxx", "xxx", "xxo", "oxx"], ["xxx", "xox", "oxx", "xxx", "xxo", "xxx"], ["xxxx", "xxox", "ooxx", "xxxx", "xxxo", "oxxx"], ["xx", "xx", "xx", "ox", "xo", "xx"], ["xxx", "xxx", "oxx", "xox", "xxo", "oxx"], ["xxx", "xxx", "oxx", "xox", "xxo", "xxx"], ["xxxx", "xxxx", "ooxx", "xxox", "xxxo", "oxxx"], ["xxx", "oxx", "xxx", "xox", "xxo", "xxx"], ["xxxx", "xoxx", "oxxx", "xxox", "xxxo", "oxxx"], ["xxxx", "xoxx", "oxxx", "xxox", "xxxo", "xxxx"], ["xxxxx", "xxoxx", "ooxxx", "xxxox", "xxxxo", "oxxxx"], ["xx", "xx", "ox", "ox", "oo", "ox"], ["xxx", "xxx", "oox", "xox", "xoo", "oox"], ["xxx", "xxx", "oox", "xox", "xoo", "xox"], ["xxxx", "xxxx", "ooox", "xxox", "xxoo", "oxox"], ["xxx", "oxx", "xox", "xox", "xoo", "xox"], ["xxxx", "xoxx", "oxox", "xxox", "xxoo", "oxox"], ["xxxx", "xoxx", "oxox", "xxox", "xxoo", "xxox"], ["xxxxx", "xxoxx", "ooxox", "xxxox", "xxxoo", "oxxox"], ["xxx", "xxx", "xox", "oox", "xoo", "xox"], ["xxxx", "xxxx", "oxox", "xoox", "xxoo", "oxox"], ["xxxx", "xxxx", "oxox", "xoox", "xxoo", "xxox"], ["xxxxx", "xxxxx", "ooxox", "xxoox", "xxxoo", "oxxox"], ["xxxx", "oxxx", "xxox", "xoox", "xxoo", "xxox"], ["xxxxx", "xoxxx", "oxxox", "xxoox", "xxxoo", "oxxox"], ["xxxxx", "xoxxx", "oxxox", "xxoox", "xxxoo", "xxxox"], ["xxxxxx", "xxoxxx", "ooxxox", "xxxoox", "xxxxoo", "oxxxox"], ["x", "o", "x", "x", "o", "x"], ["xx", "xo", "ox", "xx", "xo", "ox"], ["xx", "xo", "ox", "xx", "xo", "xx"], ["xxx", "xxo", "oox", "xxx", "xxo", "oxx"], ["xx", "oo", "xx", "xx", "xo", "xx"], ["xxx", "xoo", "oxx", "xxx", "xxo", "oxx"], ["xxx", "xoo", "oxx", "xxx", "xxo", "xxx"], ["xxxx", "xxoo", "ooxx", "xxxx", "xxxo", "oxxx"], ["xx", "xo", "xx", "ox", "xo", "xx"], ["xxx", "xxo", "oxx", "xox", "xxo", "oxx"], ["xxx", "xxo", "oxx", "xox", "xxo", "xxx"], ["xxxx", "xxxo", "ooxx", "xxox", "xxxo", "oxxx"], ["xxx", "oxo", "xxx", "xox", "xxo", "xxx"], ["xxxx", "xoxo", "oxxx", "xxox", "xxxo", "oxxx"], ["xxxx", "xoxo", "oxxx", "xxox", "xxxo", "xxxx"], ["xxxxx", "xxoxo", "ooxxx", "xxxox", "xxxxo", "oxxxx"], ["xx", "xo", "ox", "ox", "oo", "ox"], ["xxx", "xxo", "oox", "xox", "xoo", "oox"], ["xxx", "xxo", "oox", "xox", "xoo", "xox"], ["xxxx", "xxxo", "ooox", "xxox", "xxoo", "oxox"], ["xxx", "oxo", "xox", "xox", "xoo", "xox"], ["xxxx", "xoxo", "oxox", "xxox", "xxoo", "oxox"], ["xxxx", "xoxo", "oxox", "xxox", "xxoo", "xxox"], ["xxxxx", "xxoxo", "ooxox", "xxxox", "xxxoo", "oxxox"], ["xxx", "xxo", "xox", "oox", "xoo", "xox"], ["xxxx", "xxxo", "oxox", "xoox", "xxoo", "oxox"], ["xxxx", "xxxo", "oxox", "xoox", "xxoo", "xxox"], ["xxxxx", "xxxxo", "ooxox", "xxoox", "xxxoo", "oxxox"], ["xxxx", "oxxo", "xxox", "xoox", "xxoo", "xxox"], ["xxxxx", "xoxxo", "oxxox", "xxoox", "xxxoo", "oxxox"], ["xxxxx", "xoxxo", "oxxox", "xxoox", "xxxoo", "xxxox"], ["xxxxxx", "xxoxxo", "ooxxox", "xxxoox", "xxxxoo", "oxxxox"], ["xx", "xo", "xx", "xx", "oo", "xx"], ["xxx", "xxo", "oxx", "xxx", "xoo", "oxx"], ["xxx", "xxo", "oxx", "xxx", "xoo", "xxx"], ["xxxx", "xxxo", "ooxx", "xxxx", "xxoo", "oxxx"], ["xxx", "oxo", "xxx", "xxx", "xoo", "xxx"], ["xxxx", "xoxo", "oxxx", "xxxx", "xxoo", "oxxx"], ["xxxx", "xoxo", "oxxx", "xxxx", "xxoo", "xxxx"], ["xxxxx", "xxoxo", "ooxxx", "xxxxx", "xxxoo", "oxxxx"], ["xxx", "xxo", "xxx", "oxx", "xoo", "xxx"], ["xxxx", "xxxo", "oxxx", "xoxx", "xxoo", "oxxx"], ["xxxx", "xxxo", "oxxx", "xoxx", "xxoo", "xxxx"], ["xxxxx", "xxxxo", "ooxxx", "xxoxx", "xxxoo", "oxxxx"], ["xxxx", "oxxo", "xxxx", "xoxx", "xxoo", "xxxx"], ["xxxxx", "xoxxo", "oxxxx", "xxoxx", "xxxoo", "oxxxx"], ["xxxxx", "xoxxo", "oxxxx", "xxoxx", "xxxoo", "xxxxx"], ["xxxxxx", "xxoxxo", "ooxxxx", "xxxoxx", "xxxxoo", "oxxxxx"], ["xxx", "xxo", "oxx", "oxx", "ooo", "oxx"], ["xxxx", "xxxo", "ooxx", "xoxx", "xooo", "ooxx"], ["xxxx", "xxxo", "ooxx", "xoxx", "xooo", "xoxx"], ["xxxxx", "xxxxo", "oooxx", "xxoxx", "xxooo", "oxoxx"], ["xxxx", "oxxo", "xoxx", "xoxx", "xooo", "xoxx"], ["xxxxx", "xoxxo", "oxoxx", "xxoxx", "xxooo", "oxoxx"], ["xxxxx", "xoxxo", "oxoxx", "xxoxx", "xxooo", "xxoxx"], ["xxxxxx", "xxoxxo", "ooxoxx", "xxxoxx", "xxxooo", "oxxoxx"], ["xxxx", "xxxo", "xoxx", "ooxx", "xooo", "xoxx"], ["xxxxx", "xxxxo", "oxoxx", "xooxx", "xxooo", "oxoxx"], ["xxxxx", "xxxxo", "oxoxx", "xooxx", "xxooo", "xxoxx"], ["xxxxxx", "xxxxxo", "ooxoxx", "xxooxx", "xxxooo", "oxxoxx"], ["xxxxx", "oxxxo", "xxoxx", "xooxx", "xxooo", "xxoxx"], ["xxxxxx", "xoxxxo", "oxxoxx", "xxooxx", "xxxooo", "oxxoxx"], ["xxxxxx", "xoxxxo", "oxxoxx", "xxooxx", "xxxooo", "xxxoxx"], ["xxxxxxx", "xxoxxxo", "ooxxoxx", "xxxooxx", "xxxxooo", "oxxxoxx"], ["o", "x", "o", "x", "x", "o"], ["xo", "xx", "oo", "xx", "xx", "oo"], ["xo", "xx", "oo", "xx", "xx", "xo"], ["xxo", "xxx", "ooo", "xxx", "xxx", "oxo"], ["xo", "ox", "xo", "xx", "xx", "xo"], ["xxo", "xox", "oxo", "xxx", "xxx", "oxo"], ["xxo", "xox", "oxo", "xxx", "xxx", "xxo"], ["xxxo", "xxox", "ooxo", "xxxx", "xxxx", "oxxo"], ["xo", "xx", "xo", "ox", "xx", "xo"], ["xxo", "xxx", "oxo", "xox", "xxx", "oxo"], ["xxo", "xxx", "oxo", "xox", "xxx", "xxo"], ["xxxo", "xxxx", "ooxo", "xxox", "xxxx", "oxxo"], ["xxo", "oxx", "xxo", "xox", "xxx", "xxo"], ["xxxo", "xoxx", "oxxo", "xxox", "xxxx", "oxxo"], ["xxxo", "xoxx", "oxxo", "xxox", "xxxx", "xxxo"], ["xxxxo", "xxoxx", "ooxxo", "xxxox", "xxxxx", "oxxxo"], ["xo", "xx", "oo", "ox", "ox", "oo"], ["xxo", "xxx", "ooo", "xox", "xox", "ooo"], ["xxo", "xxx", "ooo", "xox", "xox", "xoo"], ["xxxo", "xxxx", "oooo", "xxox", "xxox", "oxoo"], ["xxo", "oxx", "xoo", "xox", "xox", "xoo"], ["xxxo", "xoxx", "oxoo", "xxox", "xxox", "oxoo"], ["xxxo", "xoxx", "oxoo", "xxox", "xxox", "xxoo"], ["xxxxo", "xxoxx", "ooxoo", "xxxox", "xxxox", "oxxoo"], ["xxo", "xxx", "xoo", "oox", "xox", "xoo"], ["xxxo", "xxxx", "oxoo", "xoox", "xxox", "oxoo"], ["xxxo", "xxxx", "oxoo", "xoox", "xxox", "xxoo"], ["xxxxo", "xxxxx", "ooxoo", "xxoox", "xxxox", "oxxoo"], ["xxxo", "oxxx", "xxoo", "xoox", "xxox", "xxoo"], ["xxxxo", "xoxxx", "oxxoo", "xxoox", "xxxox", "oxxoo"], ["xxxxo", "xoxxx", "oxxoo", "xxoox", "xxxox", "xxxoo"], ["xxxxxo", "xxoxxx", "ooxxoo", "xxxoox", "xxxxox", "oxxxoo"], ["xo", "xx", "xo", "xx", "ox", "xo"], ["xxo", "xxx", "oxo", "xxx", "xox", "oxo"], ["xxo", "xxx", "oxo", "xxx", "xox", "xxo"], ["xxxo", "xxxx", "ooxo", "xxxx", "xxox", "oxxo"], ["xxo", "oxx", "xxo", "xxx", "xox", "xxo"], ["xxxo", "xoxx", "oxxo", "xxxx", "xxox", "oxxo"], ["xxxo", "xoxx", "oxxo", "xxxx", "xxox", "xxxo"], ["xxxxo", "xxoxx", "ooxxo", "xxxxx", "xxxox", "oxxxo"], ["xxo", "xxx", "xxo", "oxx", "xox", "xxo"], ["xxxo", "xxxx", "oxxo", "xoxx", "xxox", "oxxo"], ["xxxo", "xxxx", "oxxo", "xoxx", "xxox", "xxxo"], ["xxxxo", "xxxxx", "ooxxo", "xxoxx", "xxxox", "oxxxo"], ["xxxo", "oxxx", "xxxo", "xoxx", "xxox", "xxxo"], ["xxxxo", "xoxxx", "oxxxo", "xxoxx", "xxxox", "oxxxo"], ["xxxxo", "xoxxx", "oxxxo", "xxoxx", "xxxox", "xxxxo"], ["xxxxxo", "xxoxxx", "ooxxxo", "xxxoxx", "xxxxox", "oxxxxo"], ["xxo", "xxx", "oxo", "oxx", "oox", "oxo"], ["xxxo", "xxxx", "ooxo", "xoxx", "xoox", "ooxo"], ["xxxo", "xxxx", "ooxo", "xoxx", "xoox", "xoxo"], ["xxxxo", "xxxxx", "oooxo", "xxoxx", "xxoox", "oxoxo"], ["xxxo", "oxxx", "xoxo", "xoxx", "xoox", "xoxo"], ["xxxxo", "xoxxx", "oxoxo", "xxoxx", "xxoox", "oxoxo"], ["xxxxo", "xoxxx", "oxoxo", "xxoxx", "xxoox", "xxoxo"], ["xxxxxo", "xxoxxx", "ooxoxo", "xxxoxx", "xxxoox", "oxxoxo"], ["xxxo", "xxxx", "xoxo", "ooxx", "xoox", "xoxo"], ["xxxxo", "xxxxx", "oxoxo", "xooxx", "xxoox", "oxoxo"], ["xxxxo", "xxxxx", "oxoxo", "xooxx", "xxoox", "xxoxo"], ["xxxxxo", "xxxxxx", "ooxoxo", "xxooxx", "xxxoox", "oxxoxo"], ["xxxxo", "oxxxx", "xxoxo", "xooxx", "xxoox", "xxoxo"], ["xxxxxo", "xoxxxx", "oxxoxo", "xxooxx", "xxxoox", "oxxoxo"], ["xxxxxo", "xoxxxx", "oxxoxo", "xxooxx", "xxxoox", "xxxoxo"], ["xxxxxxo", "xxoxxxx", "ooxxoxo", "xxxooxx", "xxxxoox", "oxxxoxo"], ["xo", "ox", "xo", "xx", "ox", "xo"], ["xxo", "xox", "oxo", "xxx", "xox", "oxo"], ["xxo", "xox", "oxo", "xxx", "xox", "xxo"], ["xxxo", "xxox", "ooxo", "xxxx", "xxox", "oxxo"], ["xxo", "oox", "xxo", "xxx", "xox", "xxo"], ["xxxo", "xoox", "oxxo", "xxxx", "xxox", "oxxo"], ["xxxo", "xoox", "oxxo", "xxxx", "xxox", "xxxo"], ["xxxxo", "xxoox", "ooxxo", "xxxxx", "xxxox", "oxxxo"], ["xxo", "xox", "xxo", "oxx", "xox", "xxo"], ["xxxo", "xxox", "oxxo", "xoxx", "xxox", "oxxo"], ["xxxo", "xxox", "oxxo", "xoxx", "xxox", "xxxo"], ["xxxxo", "xxxox", "ooxxo", "xxoxx", "xxxox", "oxxxo"], ["xxxo", "oxox", "xxxo", "xoxx", "xxox", "xxxo"], ["xxxxo", "xoxox", "oxxxo", "xxoxx", "xxxox", "oxxxo"], ["xxxxo", "xoxox", "oxxxo", "xxoxx", "xxxox", "xxxxo"], ["xxxxxo", "xxoxox", "ooxxxo", "xxxoxx", "xxxxox", "oxxxxo"], ["xxo", "xox", "oxo", "oxx", "oox", "oxo"], ["xxxo", "xxox", "ooxo", "xoxx", "xoox", "ooxo"], ["xxxo", "xxox", "ooxo", "xoxx", "xoox", "xoxo"], ["xxxxo", "xxxox", "oooxo", "xxoxx", "xxoox", "oxoxo"], ["xxxo", "oxox", "xoxo", "xoxx", "xoox", "xoxo"], ["xxxxo", "xoxox", "oxoxo", "xxoxx", "xxoox", "oxoxo"], ["xxxxo", "xoxox", "oxoxo", "xxoxx", "xxoox", "xxoxo"], ["xxxxxo", "xxoxox", "ooxoxo", "xxxoxx", "xxxoox", "oxxoxo"], ["xxxo", "xxox", "xoxo", "ooxx", "xoox", "xoxo"], ["xxxxo", "xxxox", "oxoxo", "xooxx", "xxoox", "oxoxo"], ["xxxxo", "xxxox", "oxoxo", "xooxx", "xxoox", "xxoxo"], ["xxxxxo", "xxxxox", "ooxoxo", "xxooxx", "xxxoox", "oxxoxo"], ["xxxxo", "oxxox", "xxoxo", "xooxx", "xxoox", "xxoxo"], ["xxxxxo", "xoxxox", "oxxoxo", "xxooxx", "xxxoox", "oxxoxo"], ["xxxxxo", "xoxxox", "oxxoxo", "xxooxx", "xxxoox", "xxxoxo"], ["xxxxxxo", "xxoxxox", "ooxxoxo", "xxxooxx", "xxxxoox", "oxxxoxo"], ["xxo", "xox", "xxo", "xxx", "oox", "xxo"], ["xxxo", "xxox", "oxxo", "xxxx", "xoox", "oxxo"], ["xxxo", "xxox", "oxxo", "xxxx", "xoox", "xxxo"], ["xxxxo", "xxxox", "ooxxo", "xxxxx", "xxoox", "oxxxo"], ["xxxo", "oxox", "xxxo", "xxxx", "xoox", "xxxo"], ["xxxxo", "xoxox", "oxxxo", "xxxxx", "xxoox", "oxxxo"], ["xxxxo", "xoxox", "oxxxo", "xxxxx", "xxoox", "xxxxo"], ["xxxxxo", "xxoxox", "ooxxxo", "xxxxxx", "xxxoox", "oxxxxo"], ["xxxo", "xxox", "xxxo", "oxxx", "xoox", "xxxo"], ["xxxxo", "xxxox", "oxxxo", "xoxxx", "xxoox", "oxxxo"], ["xxxxo", "xxxox", "oxxxo", "xoxxx", "xxoox", "xxxxo"], ["xxxxxo", "xxxxox", "ooxxxo", "xxoxxx", "xxxoox", "oxxxxo"], ["xxxxo", "oxxox", "xxxxo", "xoxxx", "xxoox", "xxxxo"], ["xxxxxo", "xoxxox", "oxxxxo", "xxoxxx", "xxxoox", "oxxxxo"], ["xxxxxo", "xoxxox", "oxxxxo", "xxoxxx", "xxxoox", "xxxxxo"], ["xxxxxxo", "xxoxxox", "ooxxxxo", "xxxoxxx", "xxxxoox", "oxxxxxo"], ["xxxo", "xxox", "oxxo", "oxxx", "ooox", "oxxo"], ["xxxxo", "xxxox", "ooxxo", "xoxxx", "xooox", "ooxxo"], ["xxxxo", "xxxox", "ooxxo", "xoxxx", "xooox", "xoxxo"], ["xxxxxo", "xxxxox", "oooxxo", "xxoxxx", "xxooox", "oxoxxo"], ["xxxxo", "oxxox", "xoxxo", "xoxxx", "xooox", "xoxxo"], ["xxxxxo", "xoxxox", "oxoxxo", "xxoxxx", "xxooox", "oxoxxo"], ["xxxxxo", "xoxxox", "oxoxxo", "xxoxxx", "xxooox", "xxoxxo"], ["xxxxxxo", "xxoxxox", "ooxoxxo", "xxxoxxx", "xxxooox", "oxxoxxo"], ["xxxxo", "xxxox", "xoxxo", "ooxxx", "xooox", "xoxxo"], ["xxxxxo", "xxxxox", "oxoxxo", "xooxxx", "xxooox", "oxoxxo"], ["xxxxxo", "xxxxox", "oxoxxo", "xooxxx", "xxooox", "xxoxxo"], ["xxxxxxo", "xxxxxox", "ooxoxxo", "xxooxxx", "xxxooox", "oxxoxxo"], ["xxxxxo", "oxxxox", "xxoxxo", "xooxxx", "xxooox", "xxoxxo"], ["xxxxxxo", "xoxxxox", "oxxoxxo", "xxooxxx", "xxxooox", "oxxoxxo"], ["xxxxxxo", "xoxxxox", "oxxoxxo", "xxooxxx", "xxxooox", "xxxoxxo"], ["xxxxxxxo", "xxoxxxox", "ooxxoxxo", "xxxooxxx", "xxxxooox", "oxxxoxxo"]]
[DebugProxy] : Just ["xxo", "oxx", "xoo", "xox", "xox", "xoo"]
[INFO] time : 0.541543 sec
[INFO] memory : 118.992000 MB
[SUCCESS] AC

[INFO] slowest : 0.554279 sec (for sample -1)
[INFO] max memory : 118.992000 MB (for sample -3)
[SUCCESS] test success : 3 cases
-}

{- Library -}
-- データ変換共通
boolToYesNo :: Bool -> String
boolToYesNo = bool "No" "Yes"

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

arrayToTuple2 :: [a] -> (a, a)
arrayToTuple2 (a : b : _) = (a, b)

arrayToTuple3 :: [a] -> (a, a, a)
arrayToTuple3 (a : b : c : _) = (a, b, c)

tuple2ToArray :: (a, a) -> [a]
tuple2ToArray (a, b) = [a, b]

tuple3ToArray :: (a, a, a) -> [a]
tuple3ToArray (a, b, c) = [a, b, c]

bsToInt :: ByteString -> Int
bsToInt = fst . fromJust . BS.readInt

bsToIntList :: ByteString -> [Int]
bsToIntList = fmap bsToInt . BS.words

bsToIntTuple2 :: ByteString -> (Int, Int)
bsToIntTuple2 = arrayToTuple2 . bsToIntList

bsToIntTuple3 :: ByteString -> (Int, Int, Int)
bsToIntTuple3 = arrayToTuple3 . bsToIntList

bsToIntMatrix :: ByteString -> [[Int]]
bsToIntMatrix = fmap bsToIntList . BS.lines

bsToIntTuples2 :: ByteString -> [(Int, Int)]
bsToIntTuples2 = fmap (arrayToTuple2 . bsToIntList) . BS.lines

bsToIntTuples3 :: ByteString -> [(Int, Int, Int)]
bsToIntTuples3 = fmap (arrayToTuple3 . bsToIntList) . BS.lines

bsToInteger :: ByteString -> Integer
bsToInteger = fst . fromJust . BS.readInteger

bsToIntegerList :: ByteString -> [Integer]
bsToIntegerList = fmap bsToInteger . BS.words

-- IO 出力系
printYesNo :: Bool -> IO ()
printYesNo = putStrLn . boolToYesNo

printArrayWithSpace :: (Show a) => [a] -> IO ()
printArrayWithSpace = putStrLn . unwords . fmap show

printArrayWithLn :: (Show a) => [a] -> IO ()
printArrayWithLn = putStr . unlines . fmap show

printMatrix :: (Show a) => [[a]] -> IO ()
printMatrix mtx = putStr . unlines $ unwords . fmap show <$> mtx

-- IO 入力系
getLineToString :: IO String
getLineToString = BS.unpack <$> BS.getLine

getLineToInt :: IO Int
getLineToInt = bsToInt <$> BS.getLine

getLineToIntArray :: IO [Int]
getLineToIntArray = bsToIntList <$> BS.getLine

getLineToIntTuple2 :: IO (Int, Int)
getLineToIntTuple2 = bsToIntTuple2 <$> BS.getLine

getLineToIntTuple3 :: IO (Int, Int, Int)
getLineToIntTuple3 = bsToIntTuple3 <$> BS.getLine

getLineToInteger :: IO Integer
getLineToInteger = bsToInteger <$> BS.getLine

getLineToIntegerArray :: IO [Integer]
getLineToIntegerArray = bsToIntegerList <$> BS.getLine

getContentsToStringArray :: IO [String]
getContentsToStringArray = fmap BS.unpack . BS.lines <$> BS.getContents

getContentsToIntMatrix :: IO [[Int]]
getContentsToIntMatrix = bsToIntMatrix <$> BS.getContents

getContentsToIntTuples2 :: IO [(Int, Int)]
getContentsToIntTuples2 = bsToIntTuples2 <$> BS.getContents

getContentsToIntTuples3 :: IO [(Int, Int, Int)]
getContentsToIntTuples3 = bsToIntTuples3 <$> BS.getContents

-- デバッグ用
#ifndef ATCODER

debugProxy :: (Show a) => a -> a
debugProxy value =
  let !_ = debug "[DebugProxy]" value
   in value

debug :: (Show a) => String -> a -> ()
debug key value = trace (key ++ " : " ++ show value) ()

#else

debugProxy :: (Show a) => a -> a
debugProxy = id

debug :: (Show a) => String -> a -> ()
debug _ _ = ()

#endif
