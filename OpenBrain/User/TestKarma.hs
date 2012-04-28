module OpenBrain.User.TestKarma (test) where

import Test.QuickCheck as T
import qualified OpenBrain.User.Karma as K

test = do
  putStrLn "Testing User.Karma"
  quickCheck subtraction
  quickCheck fromto
  quickCheck hasK
  where
    subtraction :: (Int, Int) -> Bool
    subtraction (a, b) = do
      let x = K.toKarma a
      let y = K.toKarma b
      x - y >= 0
    fromto :: Int -> Bool
    fromto x = (max 0 x) == K.fromKarma (K.toKarma x)
    hasK :: (Int, Int) -> Bool
    hasK (x, y) = do
      let a = max 0 x
      let b = max 0 y
      (a <= b) == (K.hasKarma a $ K.toKarma b)

main = test
