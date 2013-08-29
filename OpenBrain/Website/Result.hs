{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Result where

import OpenBrain.Website.Common
import qualified OpenBrain.Website.Session as Session

pageResults :: OBW Response
pageResults = countAndPageBy ResultCount $ \l o -> liftM responseJSON' $ PageResults l o

readResult :: ResultId -> OBW Response
readResult rid = do
  rs <- liftB $ GetResults =<< DisForResult rid
  respOk . responseJSON' $ filter ((==) rid . resultId) rs
