module TestAll where

import qualified User.TestHash as UTH
import qualified User.TestKarma as UTK

main = do
  UTH.test
  UTK.test
