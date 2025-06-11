{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

import Data.Array.Unboxed (Ix, UArray, accumArray, listArray, (!))
import Data.Graph (Bounds, Edge, Graph, Vertex, buildG, dff, indegree, outdegree, reachable, scc)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Tree (flatten)
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

-- 連結成分の数を求める
countComponents :: Graph -> Int
countComponents = length . dff

-- 各連結成分の頂点数を取得する
componentCounts :: Graph -> [Int]
componentCounts = map length . dff

-- 隣接行列表現
adjacencyMatrixUndirected :: (Ix b, Num b) => [(b, b)] -> b -> UArray (b, b) Bool
adjacencyMatrixUndirected pairs vCount = accumArray @UArray (||) False ((1, 1), (vCount, vCount)) $ concat [[(pair, True), (swap pair, True)] | pair <- pairs]

{- 有効グラフ -}
-- 隣接リスト表現(Data.Graphベース)
adjacencyListDirected :: Bounds -> [Edge] -> Graph
adjacencyListDirected = buildG

-- 強連結成分をリスト化し、自己ループを含む閉路を抽出する (DAG を構成する単位)
sccList :: Graph -> [[Vertex]]
sccList = map flatten . scc
