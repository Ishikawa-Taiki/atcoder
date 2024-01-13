-- © 2024 Ishikawa-Taiki
module Main (main) where

import Control.Monad.Trans.State (State, runState, state)

-- Stateモナドのテストだが、do記法やモナド関連の演算子も一緒に確認
main :: IO ()
main = do
  print doCount -- 6
  print testCount -- 6
  print bindCount -- 6
  where
    (_, doCount) = runState testDo 0
    (_, testCount) = runState test' 0
    (_, bindCount) = runState testBind 0

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
