import Data.Vector.Unboxed as U
import Data.Int (Int) -- Int 型を明示的にインポート

main :: IO ()
main = do
  let v = U.fromList [1 :: Int, 2, 3, 4, 5] -- `1 :: Int` で要素の型を明示
  print $ U.sum v
