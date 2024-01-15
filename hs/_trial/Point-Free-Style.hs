-- © 2024 Ishikawa-Taiki
{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Debug.Trace (trace)

main :: IO ()
main = do
  print "---  ---"
  print $ arg1Add1 $ arg1Add1 0 -- 2:1引数関数の順次呼び出し
  print $ arg1Add1 . arg1Add1 $ 0 -- 2:1引数関数を合成して呼び出し
  print $ arg1Add1 $ arg2Mul 2 3 -- 7:2引数関数呼び出し後の1引数関数の呼び出し
  print $ ((arg1Add1 .) . arg2Mul) 2 3 -- 7:2引数関数呼び出し後の1引数関数の呼び出し

-- 1引数関数
arg1Add1 :: Int -> Int
arg1Add1 = (1 +)

-- 2引数関数
arg2Mul :: Int -> Int -> Int
arg2Mul = (*)

-- 動作確認用
debugProxy :: (Show a) => a -> a
debugProxy value =
  let !_ = debug "[DebugProxy]" value
   in value

debug :: (Show a) => String -> a -> ()
debug key value = trace (key ++ " : " ++ show value) ()
