module User.TestHash (test) where

import Test.QuickCheck as T
import qualified User.Hash as H

test = do
  putStrLn "Testing User.Hash"
  quickCheck go
    where
      go :: String -> Bool
      go s = let h = H.hash s in h == (H.fromString $ H.toString h)

main = test
