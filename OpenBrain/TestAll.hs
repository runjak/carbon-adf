module OpenBrain.TestAll where

import qualified OpenBrain.User.TestHash as UTH
import qualified OpenBrain.User.TestKarma as UTK

main = do
  UTH.test
  UTK.test
