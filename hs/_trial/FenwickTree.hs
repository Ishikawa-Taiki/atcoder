import Data.Bits
import Data.List (sort)
import qualified Data.Map as M
import Data.Maybe (fromJust)

-- 分かりやすかった記事[AtCoder Library を読んでアルゴリズムを勉強：フェニック木（BIT）](https://qiita.com/sysdev/items/30aa7d5e9ac4ea871bd3)
-- メモ[セグメント木とフェニック木(BIT)の使いどころについて](https://zenn.dev/link/comments/e362b09972ea94)
{-
概要/理解メモ：
・ベース知識になるセグメント木は、大元の要素を2つずつ組み合わせて親レベルで計算結果をキャッシュするようなイメージ
　→情報の参照について、親レベルで見れるところは子まで潜る必要がない！親+子で必要な範囲でだけ辿れば良い。
　→情報の更新について、末端の子を更新した後、その子が情報元になる親だけを更新すれば良い!
・フェニック木は、セグメント木の各段階から右半分を削ったイメージ
　→情報の参照について、区間和二つの差を使うことで任意の区間の和を見ることができる!
　→情報の更新について、親子で同じ値を反映すれば良い!
-}

-- Fenwick Tree 型の定義
data FenwickTree = FenwickTree {size :: Int, tree :: [Int]}
  deriving (Show)

-- 空の Fenwick Tree の初期化
initialize :: Int -> FenwickTree
initialize n = FenwickTree n (replicate (succ n) 0)

-- 値の更新
update :: FenwickTree -> Int -> Int -> FenwickTree
update ft index value = go index (tree ft)
  where
    go idx t
      | idx > size ft = ft {tree = t}
      | otherwise = go (idx + (idx .&. (- idx))) (updateAt idx value t)
    updateAt idx val xs = take idx xs ++ [xs !! idx + val] ++ drop (idx + 1) xs

-- 累積和の取得
query :: FenwickTree -> Int -> Int
query ft index = go index 0
  where
    go idx acc
      | idx <= 0 = acc
      | otherwise = go (idx - (idx .&. (- idx))) (acc + tree ft !! idx)

-- 転倒数の計算
countInversions :: [Int] -> Int
countInversions arr = snd $ foldr count (emptyFT, 0) compressed
  where
    -- 空の Fenwick Tree
    emptyFT = initialize (length arr)
    -- 期待値を算出
    rank = M.fromList $ flip zip [1 ..] $ sort arr
    compressed = (rank M.!) <$> arr

    -- 転倒数を数える関数(フェニック木と転倒数に畳み込む)
    count x (ft, invCount) =
      let invCount' = invCount + query ft (pred x)
          ft' = update ft x 1
       in (ft', invCount')

-- 区間和を求めるサンプル
sampleUpdateAndRangeSum :: String
sampleUpdateAndRangeSum =
  let n = 10 -- 数列のサイズ
      initialFT = initialize n

      -- 初期の数列に値を設定
      updatedFT1 = update initialFT 1 5
      updatedFT2 = update updatedFT1 2 3
      updatedFT3 = update updatedFT2 3 7
      updatedFT4 = update updatedFT3 5 6

      -- 特定の位置の要素を更新
      updatedFT5 = update updatedFT4 3 2
   in -- 指定された範囲の和を取得
      "[sampleUpdateAndRangeSum] src: " ++ show updatedFT5 ++ " Range sum [2, 5]: " ++ show (rangeQuery updatedFT5 2 5) -- [2, 5] の範囲の和
  where
    -- 区間和の取得
    rangeQuery :: FenwickTree -> Int -> Int -> Int
    rangeQuery ft left right = query ft right - query ft (left - 1)

-- 転倒数を求めるサンプル
sampleInversions :: String
sampleInversions = "[sampleInversions] src: " ++ show src ++ " Inversion count: " ++ show invCount
  where
    src = [3, 1, 2, 5, 4] -- サンプルの数列
    invCount = countInversions src

-- メイン関数
main :: IO ()
main = do
  putStrLn sampleUpdateAndRangeSum
  putStrLn sampleInversions
