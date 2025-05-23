{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

import Data.Array.Unboxed (Ix, UArray, accumArray, listArray, (!))
import Data.Map qualified as M
import Data.Tuple (swap)

{- 無効グラフ -}
-- 隣接リスト表現
adjacencyListUndirected :: (Ord a) => [(a, a)] -> M.Map a [a]
adjacencyListUndirected pairs = M.fromListWith (++) $ concat [[(a, [b]), (b, [a])] | (a, b) <- pairs]

-- 隣接行列表現
adjacencyMatrixUndirected :: (Ix b, Num b) => [(b, b)] -> b -> UArray (b, b) Bool
adjacencyMatrixUndirected pairs vCount = accumArray @UArray (||) False ((1, 1), (vCount, vCount)) $ concat [[(pair, True), (swap pair, True)] | pair <- pairs]

{- 有効グラフ -}
-- 隣接リスト表現
adjacencyListDirected :: (Ord a) => [(a, a)] -> M.Map a [a]
adjacencyListDirected pairs = M.fromListWith (++) $ [(a, [b]) | (a, b) <- pairs]
