#!/usr/bin/env runghc
-- © 2024 Ishikawa-Taiki

module Main (main) where

main :: IO ()
main = do
    -- 整数の入力
    a <- readLn
    -- 出力
    print (a + 5)
