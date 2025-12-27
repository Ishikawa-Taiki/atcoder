module Main (main) where

import AtCoder.FenwickTree
import Data.Vector.Unboxed (fromList, (!))
import qualified Data.Vector.Unboxed as U

main :: IO ()
main = do
  -- Create a Fenwick tree from a list [1, 2, 3, 4, 5]
  let initialList = [1, 2, 3, 4, 5] :: [Int]
  let ft = build (U.fromList initialList) :: FenwickTree Int

  putStrLn $ "Initial FenwickTree: " ++ show ft

  -- Query sum from index 0 to 2 (1-indexed for FenwickTree)
  -- For [1, 2, 3, 4, 5], sum(1..3) = 1 + 2 + 3 = 6
  let sum0to2 = query ft 3
  putStrLn $ "Sum from index 0 to 2 (1-indexed query 3): " ++ show sum0to2

  -- Update value at index 1 (0-indexed list, 1-indexed FenwickTree) by adding 10
  -- So, initialList[1] (which is 2) becomes 12.
  -- List becomes [1, 12, 3, 4, 5]
  let updatedFt = add ft 2 10 -- add 10 to 1-indexed position 2 (value at original index 1)

  putStrLn $ "FenwickTree after adding 10 at index 1: " ++ show updatedFt

  -- Query sum from index 0 to 2 again (1-indexed query 3)
  -- For [1, 12, 3, 4, 5], sum(1..3) = 1 + 12 + 3 = 16
  let newSum0to2 = query updatedFt 3
  putStrLn $ "New sum from index 0 to 2 (1-indexed query 3): " ++ show newSum0to2

  -- Verify with direct vector
  let v = U.fromList initialList
  putStrLn $ "Original Vector: " ++ show v
  let v' = v U.// [(1, v ! 1 + 10)] -- Update at 0-indexed position 1
  putStrLn $ "Updated Vector: " ++ show v'
  let expectedSum = U.sum (U.take 3 v')
  putStrLn $ "Expected sum from updated vector: " ++ show expectedSum