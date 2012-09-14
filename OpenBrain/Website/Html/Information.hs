{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Html.Information (serve) where

import Happstack.Server as S
import System.Time (CalendarTime)
import Text.Blaze ((!))
import qualified System.Time as Time
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import OpenBrain.Backend.Types as Types
import OpenBrain.Data.Id
import OpenBrain.Data.Information (Information)
import OpenBrain.Website.Common
import OpenBrain.Website.Monad
import qualified OpenBrain.Backend.Monad as OBB
import qualified OpenBrain.Data.Information as Information
import qualified OpenBrain.Website.Html.Decorator as Decorator

{- Displaying informations: -}
viewSingle :: Information -> OBW H.Html
viewSingle i = return "Some dummy code - replace"

viewMany :: Count -> Limit -> Offset -> [Information] -> OBW H.Html
viewMany count limit offset is = return "More dummy code!"

{- Fetching parameters: -}
getLimit :: OBW Limit
getLimit = msum [lookRead "limit", return 30]

getOffset :: OBW Offset
getOffset = msum [lookRead "offset", return 0]

instance FromReqURI CalendarTime where
  fromReqURI s = case reads s of
    [(ct, _)] -> Just ct
    _ -> Nothing

{- May fail -}
getAfter :: OBW CalendarTime
getAfter = lookRead "after"

{- May fail -}
getUser :: OBW UserId
getUser = liftM fromId $ lookRead "user"

{- May fail -}
getDisplay :: OBW InformationId
getDisplay = liftM fromId $ lookRead "display"

{-
  Listing informations:
  /information.html
  /information.html?limit=_&offset=_
  /information.html?after=_&limit=_&offset=_
  /information.html?user=_&limit=_&offset=_
  Displaying single information:
  /information.html?display=_
-}
serve :: OBW Response
serve = msum [serveSingle, serveUser, serveAfter, serveList]

{-
  /information.html
  /information.html?limit=_&offset=_
-}
serveList :: OBW Response
serveList = do
  limit   <- getLimit
  offset  <- getOffset
  count   <- liftOBB $ OBB.getInformationCount
  is      <- liftOBB $ OBB.getInformations limit offset
  ok . toResponse =<< viewMany count limit offset is

{- /information.html?after=_&limit=_&offset=_ -}
serveAfter :: OBW Response
serveAfter = do
  after   <- getAfter
  limit   <- getLimit
  count   <- undefined -- FIXME
  offset  <- getOffset
  is      <- liftOBB $ OBB.getInformationsAfter after limit offset
  ok . toResponse =<< viewMany count limit offset is

{- /information.html?user=_&limit=_&offset=_ -}
serveUser :: OBW Response
serveUser = do
  uid     <- getUser
  limit   <- getLimit
  offset  <- getOffset
  count   <- liftOBB $ OBB.getInformationCountBy uid
  is      <- liftOBB $ OBB.getInformationBy uid limit offset
  ok . toResponse =<< viewMany count limit offset is

{- /information.html?display=_ -}
serveSingle :: OBW Response
serveSingle = do
  iid <- getDisplay
  i   <- liftOBB $ OBB.getInformation iid
  ok . toResponse =<< viewSingle i
