{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Html.Information (serve) where

import Data.Maybe
import Happstack.Server as S
import System.Time (CalendarTime)

import OpenBrain.Website.Common
import OpenBrain.Website.Session (chkSession)
import qualified OpenBrain.Website.Parameters      as Parameters

{-
  Listing informations:
  /information.html
  /information.html?limit=_&offset=_
  /information.html?after=_&limit=_&offset=_
  /information.html?user=_&limit=_&offset=_
  /information.html?items=[..]
  Displaying single information:
  /information.html?display=_
-}
serve :: OBW Response
serve = msum [serveSingle, serveUser, serveAfter, serveItems, serveList]

{-
  /information.html
  /information.html?limit=_&offset=_
-}
serveList :: OBW Response
serveList = do
  limit   <- Parameters.getLimit
  offset  <- Parameters.getOffset
  count   <- liftOBB GetInformationCount
  is      <- liftOBB $ GetInformations limit offset
  undefined

{- /information.html?after=_&limit=_&offset=_ -}
serveAfter :: OBW Response
serveAfter = do
  after   <- Parameters.getAfter
  limit   <- Parameters.getLimit
  count   <- liftOBB $ GetInformationCountAfter after
  offset  <- Parameters.getOffset
  is      <- liftOBB $ GetInformationsAfter after limit offset
  undefined

{- /information.html?user=_&limit=_&offset=_ -}
serveUser :: OBW Response
serveUser = do
  uid     <- Parameters.getUser
  limit   <- Parameters.getLimit
  offset  <- Parameters.getOffset
  count   <- liftOBB $ GetInformationCountBy uid
  is      <- liftOBB $ GetInformationBy uid limit offset
  undefined

{- /information.html?items=[..] -}
serveItems :: OBW Response
serveItems = do
  iids    <- Parameters.getItems
  is      <- noMaybes $ mapM (\i -> liftOBB $ GetInformation i) iids
  undefined

{- /information.html?display=_ -}
serveSingle :: OBW Response
serveSingle = do
  iid <- liftM fromId Parameters.getDisplay
  handleFail "Can't find requested information." $ do
    i   <- noMaybe . liftOBB $ GetInformation iid
    undefined
