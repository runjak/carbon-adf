{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Json.Information (serve) where

import Data.Maybe
import Happstack.Server as S
import System.Time (CalendarTime)

import OpenBrain.Website.Common
import OpenBrain.Website.Session (chkSession)
import qualified OpenBrain.Website.Parameters      as Parameters

{-
  Listing informations:
  information
  information?limit=_&offset=_
  information?after=_&limit=_&offset=_
  information?user=_&limit=_&offset=_
  information?items=[..]
  Displaying single information:
  information?display=_
-}
serve :: OBW Response
serve = msum [serveSingle, serveUser, serveAfter, serveItems, serveList]

{-
  information
  information?limit=_&offset=_
-}
serveList :: OBW Response
serveList = do
  limit   <- Parameters.getLimit
  offset  <- Parameters.getOffset
  count   <- liftOBB GetInformationCount
  is      <- liftOBB $ GetInformations limit offset
  ok $ jsonResponse is

{- information?after=_&limit=_&offset=_ -}
serveAfter :: OBW Response
serveAfter = do
  after   <- Parameters.getAfter
  limit   <- Parameters.getLimit
  count   <- liftOBB $ GetInformationCountAfter after
  offset  <- Parameters.getOffset
  is      <- liftOBB $ GetInformationsAfter after limit offset
  ok $ jsonResponse is

{- information?user=_&limit=_&offset=_ -}
serveUser :: OBW Response
serveUser = do
  uid    <- Parameters.getUser
  limit  <- Parameters.getLimit
  offset <- Parameters.getOffset
  count  <- liftOBB $ GetInformationCountBy uid
  is     <- liftOBB $ GetInformationBy uid limit offset
  ok $ jsonResponse is

{- information?items=[..] -}
serveItems :: OBW Response
serveItems = do
  iids <- Parameters.getItems
  is   <- noMaybes $ mapM (\i -> liftOBB $ GetInformation i) iids
  ok $ jsonResponse is

{- information/_ -}
serveSingle :: OBW Response
serveSingle = Parameters.withId $ \id -> do
  let iid = fromId id
  handleFail "Can't find requested information." $ do
    i <- noMaybe . liftOBB $ GetInformation iid
    ok $ jsonResponse i
