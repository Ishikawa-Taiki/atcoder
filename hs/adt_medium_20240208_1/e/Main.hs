{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Redundant flip" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas -Wno-incomplete-patterns -Wno-unused-imports -Wno-unused-top-binds -Wno-name-shadowing -Wno-unused-matches #-}

-- © 2024 Ishikawa-Taiki
module Main (main) where

import Control.Monad (replicateM)
import Control.Monad.Fix (fix)
import Data.Array.Unboxed (IArray (bounds), Ix (range), UArray, listArray, (!))
import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Debug.Trace (trace)

main :: IO ()
main = do
  (h, w) <- getLineToIntTuple2
  xs <- replicateM h getLineToString
  let grid = listArray @UArray ((1, 1), (h, w)) $ concat xs
  printArrayWithSpace $ solve grid h w

solve :: UArray (Int, Int) Char -> Int -> Int -> [(Int, Int)]
solve grid h w = do
  let crossPoints = flip fix ([], 2) \loop (accum, i) ->
        if i > (h - 1)
          then accum
          else do
            let innerResult = flip fix ([], 2) \loop (inner, j) ->
                  if j > (w - 1)
                    then inner
                    else do
                      let aroundData = debugProxy [grid ! (i - 1, j - 1), grid ! (i + 1, j - 1), grid ! (i - 1, j + 1), grid ! (i + 1, j + 1)]
                      loop
                        ( bool
                            inner
                            (inner ++ [(i, j)])
                            (all (== '#') aroundData),
                          j + 1
                        )
            loop (accum ++ innerResult, i + 1)
  crossPoints

-- バツ印の中心座標を取得する
searchCrossPoints :: UArray (Int, Int) Char -> Int -> Int -> [(Int, Int)]
searchCrossPoints grid h w = flip fix ([], 2) \loop (accum, i) ->
  if i > (h - 1)
    then accum
    else do
      let innerResult = flip fix ([], 2) \loop (inner, j) ->
            if j > (w - 1)
              then inner
              else do
                let aroundData = debugProxy [grid ! (i - 1, j - 1), grid ! (i + 1, j - 1), grid ! (i - 1, j + 1), grid ! (i + 1, j + 1)]
                loop
                  ( bool
                      inner
                      (inner ++ [(i, j)])
                      (all (== '#') aroundData),
                    j + 1
                  )
      loop (accum ++ innerResult, i + 1)

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

-- IO 出力系
printYesNo :: Bool -> IO ()
printYesNo = putStrLn . boolToYesNo

printArrayWithSpace :: (Show a) => [a] -> IO ()
printArrayWithSpace = putStrLn . unwords . fmap show

printArrayWithLn :: (Show a) => [a] -> IO ()
printArrayWithLn = putStr . unlines . fmap show

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
