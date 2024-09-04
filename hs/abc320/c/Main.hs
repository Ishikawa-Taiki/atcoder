{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Redundant flip" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas -Wno-incomplete-patterns -Wno-unused-imports -Wno-unused-top-binds -Wno-name-shadowing -Wno-unused-matches #-}

-- © 2024 Ishikawa-Taiki
module Main (main) where

import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.List (find, permutations)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, mapMaybe)
import Debug.Trace (trace)

main :: IO ()
main = do
  m <- getLineToInt
  xxs <- getContentsToStringArray
  print $ solve xxs m

{-
問題概要:
スロットの出目0-9の表示順を示す3本のリールのテキスト情報が与えられる
一つずつ順に止めたとき、最短で何秒後にスロットを揃えられるかを求めよ
求まらない時は-1を出力する

戦略:
出目の種類が10通りしかないので、それぞれで揃えようとした時に何秒後になるかを求めたあと、最短のものを導けば良さそう
複数のリールを同時に止めることは出来なさそうなので、止めたあとに次のものを止める想定で進める必要がある
 見つけた場所以降で最初に登場する場所を探すような流れになる
 付随して2周目以降で引き当てる可能性もあるので、リール情報は本数と同じ3周分用意してシミュレーションする
止める順序は6通りあるので、それぞれのパターンも全部試す

-}

solve :: [String] -> Int -> Int
solve xxs m =
  let (as : bs : cs : _) = M.fromListWith (++) . reverse . flip zip (map (: []) [0 ..]) <$> fmap (concat . replicate 3) xxs -- 0-9までの値をキーとし、登場位置をリストとしたMapに保持する
      results = concat (mapMaybe (\x -> slot (as M.!? x) (bs M.!? x) (cs M.!? x)) ['0' .. '9']) -- 0-9それぞれで揃えようとしたら、どの時間で揃えられるかのパターンを求める
   in if null results then -1 else minimum results -- シミュレーションした結果のうち、一番小さい値が最短で揃えられるタイミングとなる

type Index = Maybe [Int]

type HitIndex = [Int]

slot :: Index -> Index -> Index -> Index
slot Nothing _ _ = Nothing
slot _ Nothing _ = Nothing
slot _ _ Nothing = Nothing
slot (Just a) (Just b) (Just c) = Just $ mapMaybe searchSameNumberIndex $ permutations [a, b, c]
  where
    searchSameNumberIndex :: [HitIndex] -> Maybe Int
    searchSameNumberIndex (i : j : k : _) = f j (head i) >>= f k
    f :: HitIndex -> Int -> Maybe Int
    f hits x = find (> x) hits

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
