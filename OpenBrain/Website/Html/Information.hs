{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Html.Information (serve) where

import Data.Maybe
import Happstack.Server as S
import System.Time (CalendarTime)
import Text.Blaze ((!))
import qualified System.Time as Time
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import OpenBrain.Backend.Types as Types hiding (CreateInformation(..))
import OpenBrain.Common
import OpenBrain.Data.Id
import OpenBrain.Data.Information (Information)
import OpenBrain.Website.Common
import OpenBrain.Website.Monad
import qualified OpenBrain.Backend.Monad as OBB
import qualified OpenBrain.Data.Information as Information
import qualified OpenBrain.Data.User as User
import qualified OpenBrain.Website.Html.Decorator as Decorator

{- Displaying informations: -}
viewSingle :: Information -> OBW H.Html
viewSingle i = (=<<) Decorator.page $ do
  -- Gathering information necessary to display:
  let deleted     = (isJust $ Information.deletion i) ? ([A.class_ "deleted"],[])
      attributes  = [A.class_ "Information"] ++ deleted
  -- Constructing the Html:
  return $ foldl (!) H.div attributes $ do
    title i
    when (not . null $ Information.description i) $ do
      description i >> H.hr
    case (Information.media i) of
      (Information.Content c) -> H.div ! A.class_ "InformationContent" $ H.toHtml c
      _ -> return () -- FIXME handle collections and discussions.
    H.hr >> "Foobar" -- FIXME display defendants and attackers
    H.hr >> footnotes i

viewMany :: Count -> Limit -> Offset -> [Information] -> OBW H.Html
viewMany count limit offset is = (=<<) Decorator.page $ do
  return $ do
    "A List of informations:" >> list is

list :: [Information] -> H.Html
list is = H.ul ! A.class_ "InformationList" $ mapM_ (H.li . preview) is

preview :: Information -> H.Html
preview i = H.div ! A.class_ "InformationPreview" $ do
  let displayId = show . unwrap . toId . Information.informationId
  H.a ! A.href (toHref "information.html" ["display=" ++ displayId i]) $ do
    title i
  description i >> H.hr >> footnotes i

title :: Information -> H.Html
title i = H.h1 ! A.class_ "InformagtionTitle" $ H.toHtml $ Information.title i

description :: Information -> H.Html
description i = H.div ! A.class_ "InformationDescription" $ H.toHtml $ Information.description i

footnotes :: Information -> H.Html
footnotes i = H.dl $ do
  H.dt "Created:"
  H.dd . H.toHtml $ Information.creation i
  when (isJust $ Information.deletion i) $ do
    H.dt "Deleted:"
    H.dd . H.toHtml . fromJust $ Information.deletion i
  H.dt "Author:"
  H.dd . H.toHtml . User.username $ Information.author i

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
  count   <- liftOBB $ OBB.getInformationCountAfter after
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
  handleFail "Can't find requested information." $ do
    i   <- liftOBB $ OBB.getInformation iid
    ok . toResponse =<< viewSingle i
