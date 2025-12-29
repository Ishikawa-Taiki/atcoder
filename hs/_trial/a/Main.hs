import Control.Monad (forM_)
import qualified Data.Vector.Unboxed as U
import AtCoder.FenwickTree

main :: IO ()
main = do
  -- フェニック木を作成 (サイズ 5, 初期値 0)
  fw <- newFT 5 (0 :: Int)
  
  -- 0番目に 1 を加算
  addFT fw 0 1
  -- 1番目に 2 を加算
  addFT fw 1 2
  -- 2番目に 3 を加算
  addFT fw 2 3
  -- 3番目に 4 を加算
  addFT fw 3 4
  -- 4番目に 5 を加算
  addFT fw 4 5
  
  -- [0, 5) の区間和 (1+2+3+4+5 = 15) を計算して出力
  s <- sumFT fw 0 5
  print s
  
  -- [2, 4) の区間和 (3+4 = 7) を計算して出力
  s2 <- sumFT fw 2 4
  print s2
