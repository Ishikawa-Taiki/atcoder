#!/usr/bin/env runghc
# © 2024 Ishikawa-Taiki

{- お客様の中でHaskellを書ける方はいらっしゃいますか？
    と、Haskellの例がなくて困っていたところを @tanakh さんに助けて頂きました。本当にありがとうございました。-}
import Control.Applicative
 
main :: IO ()
main = do
    -- 整数の入力
    a <- readLn
    -- スペース区切り整数の入力
    [b, c] <- map read . words <$> getLine
    -- 文字列の入力
    s <- getLine
    -- 出力
    putStrLn $ show (a + b + c) ++ " " ++ s
