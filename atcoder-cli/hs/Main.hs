#!/usr/bin/env runghc
-- © 2024 Ishikawa-Taiki

module Main (main) where
import Data.Bool (bool)
import qualified Data.List as L
import qualified Data.ByteString.Char8 as BS
import Data.Char (isSpace)
import Data.Maybe (fromJust)

main :: IO ()
main = do
  xs <- map read . words <$> getLine :: IO [Int]
  print $ solve xs

solve :: [Int] -> Int
solve xs = undefined

{- Library -}
-- IO 出力系
printYesNo :: Bool -> IO ()
printYesNo = putStrLn . bool "No" "Yes"

printListWithSpace :: (Show a) => [a] -> IO ()
printListWithSpace = putStrLn . unwords . map show

printListWithLn :: (Show a) => [a] -> IO ()
printListWithLn = putStr . unlines . map show

-- IO 入力系
getInt :: IO Int
getInt = fst . fromJust . BS.readInt <$> BS.getLine
getIntList :: IO [Int]
getIntList = map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
-- getIntMatrix :: IO [[Int]]
-- getIntMatrix = map (fst . fromJust . BS.readInt . BS.words) $ BS.lines <$> BS.getContents
