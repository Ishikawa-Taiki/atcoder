#!/usr/bin/env runghc
-- © 2024 Ishikawa-Taiki
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main) where
import Data.Array

main :: IO ()
main = do
  [n, q] <- map read . words <$> getLine :: IO [Int]
  -- 複数行、複数列の入力の受け取り
  ns <- map words . lines <$> getContents
  let commandList = map (\(c1:c2:xs) -> createCommand c1 c2) ns
  putStr $ foldl (\x y->x++"\n"++y) "" $ solve commandList n q

solve :: [Command] -> Int -> Int -> [String]
solve commandList n q = 
  let first = createDragon n
      command = take q commandList
  in fmap show $ fst $ foldl execCommand ([], first) command

data Command = U | D | R | L | Print Int
  deriving (Show)
createCommand :: String -> String -> Command
createCommand "1" "U" = U
createCommand "1" "D" = D
createCommand "1" "R" = R
createCommand "1" "L" = L
createCommand "2" c2 = Print (read c2::Int)

data Vector2D a = Vector2D a a
vInit :: (Num a) => a -> a -> Vector2D a
vInit = Vector2D
vUp :: (Num a) => Vector2D a -> Vector2D a
vUp (Vector2D x y) = Vector2D x (y+1)
vDown :: (Num a) => Vector2D a -> Vector2D a
vDown (Vector2D x y) = Vector2D x (y-1)
vRight :: (Num a) => Vector2D a -> Vector2D a
vRight (Vector2D x y) = Vector2D (x+1) y
vLeft :: (Num a) => Vector2D a -> Vector2D a
vLeft (Vector2D x y) = Vector2D (x-1) y
instance Show a => Show (Vector2D a) where
    show :: Show a => Vector2D a -> String
    show (Vector2D x y) = show x ++ " " ++ show y

type Dragon = [Vector2D Int]
createDragon :: Int -> Dragon
createDragon n = fmap (`vInit` 0) [1..n]
dragonMove :: Command -> Dragon -> Dragon
dragonMove U d = vUp (head d) : init d
dragonMove D d = vDown (head d) : init d
dragonMove R d = vRight (head d) : init d
dragonMove L d = vLeft (head d) : init d
dragonMove _ d = d

execCommand :: ([Vector2D Int], Dragon) -> Command -> ([Vector2D Int], Dragon)
execCommand (v, d) (Print x) = (v++[d!!(x-1)], d)
execCommand (v, d) o = (v, dragonMove o d)
