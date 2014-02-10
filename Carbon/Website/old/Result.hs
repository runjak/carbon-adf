{-# LANGUAGE OverloadedStrings #-}
module Carbon.Website.Result where

import Carbon.Website.Common
import qualified Carbon.Website.Session as Session

pageResults :: OBW Response
pageResults = countAndPageBy ResultCount $ \l o -> liftM responseJSON' $ PageResults l o

readResult :: ResultId -> OBW Response
readResult rid = do
  rs <- liftB $ GetResults =<< DisForResult rid
  respOk . responseJSON' $ filter ((==) rid . resultId) rs
