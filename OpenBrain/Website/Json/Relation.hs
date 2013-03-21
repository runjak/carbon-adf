{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Json.Relation (serve) where

import Data.Maybe
import Happstack.Server as S
import System.Time (CalendarTime)

import OpenBrain.Website.Common
import OpenBrain.Website.Session (chkSession)
import qualified OpenBrain.Website.Parameters as Parameters

{-
  Listing relations:
  relation/_
  relation?informationId=_
-}
serve :: OBW Response
serve = msum [getR, getRForI]

getR :: OBW Response
getR = path $ \id -> do
  let rid = fromId id
  r <- liftOBB $ GetRelation rid
  case r of
    Nothing -> jsonFail $ "RelationId " ++ show rid ++ " not found."
    (Just r') -> ok $ jsonResponse r'

getRForI :: OBW Response
getRForI = do
  iid <- Parameters.getInformationId
  rs  <- liftOBB $ GetRelations iid RelationSource Nothing False
  ok $ jsonResponse rs
