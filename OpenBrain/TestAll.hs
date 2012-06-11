module OpenBrain.TestAll where

import qualified OpenBrain.Data.TestHash as UTH
import qualified OpenBrain.Data.TestKarma as UTK

main = do
  UTH.test
  UTK.test
