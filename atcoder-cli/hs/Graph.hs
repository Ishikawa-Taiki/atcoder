{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

import Data.Array.Unboxed (Ix, UArray, accumArray, listArray, (!))
import Data.Graph (Bounds, Graph, Vertex, buildG, indegree, outdegree, reachable)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Tuple (swap)

{- 無効グラフ -}
-- 隣接リスト表現(Data.Graphベース)
adjacencyListUndirected :: Foldable t => Bounds -> t (Vertex, Vertex) -> Graph
adjacencyListUndirected bounds pairs = buildG bounds $ concatMap (\x -> [x, swap x]) pairs

-- 与えられた単純無効グラフがサイクルグラフかどうかを判定する
isCycleGraph :: Bounds -> Graph -> Bool
isCycleGraph (start, end) graph = connected && loop
  where
    connected = S.fromList [start .. end] == S.fromList (reachable graph 1)
    loop = all (\x -> (i ! x == 2) && (o ! x == 2)) [start .. end]
    i = indegree graph
    o = outdegree graph

-- 隣接行列表現
adjacencyMatrixUndirected :: (Ix b, Num b) => [(b, b)] -> b -> UArray (b, b) Bool
adjacencyMatrixUndirected pairs vCount = accumArray @UArray (||) False ((1, 1), (vCount, vCount)) $ concat [[(pair, True), (swap pair, True)] | pair <- pairs]

{- 有効グラフ -}
-- 隣接リスト表現
adjacencyListDirected :: (Ord a) => [(a, a)] -> M.Map a [a]
adjacencyListDirected pairs = M.fromListWith (++) $ [(a, [b]) | (a, b) <- pairs]
