import Data.Vector.Unboxed as U

main :: IO ()
main = do
  let v = U.fromList [1, 2, 3, 4, 5]
  print $ U.sum v