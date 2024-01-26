-- © 2024 Ishikawa-Taiki
module Main (main) where

import Control.Monad.Trans.State (State, runState, state)

-- Stateモナドのテストだが、do記法やモナド関連の演算子も一緒に確認
main :: IO ()
main = do
  print doCount -- 6
  print testCount -- 6
  print bindCount -- 6
  print stateAndValue -- ([1,2,3,1,2,3,6,7,8],[(2,2,2),(3,3,3),(1,1,1),(1,1,1),(5,5,5)])
  where
    (_, doCount) = runState testDo 0
    (_, testCount) = runState test' 0
    (_, bindCount) = runState testBind 0
    stateAndValue = runState testDo2 [(2, 2, 2), (3, 3, 3)]

type Counter = Int

-- do記法での値の取り出しは値を捨てているのと同じとのこと
testDo :: State Counter ()
testDo = do
  countUp -- _ <- countUp と等価
  countUp
  countUp
  countDouble

-- オペレータの名前がわからなかったが、do記法と等価とのこと
-- actionから値を取り出して捨て、次のactionに移るイメージのオペレータ
test' :: State Counter ()
test' = countUp >> countUp >> countUp >> countDouble

-- bindは a -> m b に m a を入れる為に一旦モナドから値を取り出すイメージ
testBind :: State Counter ()
testBind = countUp >>= (\_ -> countUp) >>= (\_ -> countUp) >>= (\_ -> countDouble)

countUp :: State Counter ()
countUp = state $ \c -> ((), c + 1)

countDouble :: State Counter ()
countDouble = state $ \c -> ((), c * 2)

-- 値と状態の両方を使う＆引数を追加するサンプル
type RecordData = [(Int, Int, Int)]

testDo2 :: State RecordData [Int]
testDo2 = do
  a <- do2add
  b <- do2add
  c <- do2add2 5
  -- return [1, 2, 3]
  return (a ++ b ++ c)

do2add :: State RecordData [Int]
do2add = state $ \c -> ([1, 2, 3], c ++ [(1, 1, 1)])

do2add2 :: Int -> State RecordData [Int]
do2add2 n = state $ \c -> ([n + 1, n + 2, n + 3], c ++ [(n, n, n)])
