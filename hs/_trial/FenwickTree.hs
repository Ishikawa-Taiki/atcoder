import Data.Bits
import Data.List (sort)
import qualified Data.Map as Map
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
    -- 座標圧縮
    sorted = sort arr
    rank = Map.fromList $ zip sorted [1 ..]
    compressed = map (fromJust . (`Map.lookup` rank)) arr

    -- 空の Fenwick Tree
    emptyFT = initialize (length arr)

    -- 転倒数を数える関数
    count x (ft, invCount) =
      let invCount' = invCount + query ft (x - 1)
          ft' = update ft x 1
       in (ft', invCount')

-- 転倒数を求めるサンプル
sampleInversions :: String
sampleInversions = " src: " ++ show src ++ " Inversion count: " ++ show invCount
  where
    src = [3, 1, 2, 5, 4] -- サンプルの数列
    invCount = countInversions src

-- メイン関数
main :: IO ()
main = do
  putStrLn sampleInversions
