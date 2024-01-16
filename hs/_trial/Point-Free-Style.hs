-- © 2024 Ishikawa-Taiki
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Main (main) where

import Debug.Trace (trace)

main :: IO ()
main = do
  putStrLn "--- 1arg 1arg ---"
  print $ arg1Add1 $ arg1Add1 0 -- 2:1引数関数の順次呼び出し
  print $ arg1Add1 . arg1Add1 $ 0 -- 2:1引数関数を合成して呼び出し
  putStrLn "--- 2arg 1arg ---"
  print $ arg1Add1 $ arg2Mul 2 3 -- 7:2引数関数呼び出し後の1引数関数の呼び出し
  print $ ((arg1Add1 .) . arg2Mul) 2 3 -- 7:2引数関数と1引数関数を合成して呼び出し
  putStrLn "--- Functor ---"
  print $ fmap arg1Add1 $ arg1Just 1 -- Just 2:Functorに包む関数と1引数関数の順次呼び出し
  print $ arg1Add1 <$> arg1Just 1 -- Just 2:Functorに包む関数と1引数関数の順次呼び出し
  print $ arg1Add1 <$> arg1Add1 $ 0 -- 2:1引数関数を合成して呼び出し(関数もFunctor)
  putStrLn "--- Applicative ---"
  print $ (+) <$> arg1Just 1 <*> arg1Just 2 -- Just 3:複数の引数を取る関数に複数の値を適用する

-- 1引数関数
arg1Add1 :: Int -> Int
arg1Add1 = (1 +)

-- 2引数関数
arg2Mul :: Int -> Int -> Int
arg2Mul = (*)

-- 1引数関数(Functor)
arg1Just :: Int -> Maybe Int
arg1Just = Just

-- 動作確認用
debugProxy :: (Show a) => a -> a
debugProxy value =
  let !_ = debug "[DebugProxy]" value
   in value

debug :: (Show a) => String -> a -> ()
debug key value = trace (key ++ " : " ++ show value) ()
