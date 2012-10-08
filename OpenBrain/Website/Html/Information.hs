{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Html.Information (serve, viewSingle) where

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
import OpenBrain.Data.Relation
import OpenBrain.Website.Common
import OpenBrain.Website.Monad
import qualified OpenBrain.Backend.Monad as OBB
import qualified OpenBrain.Data.Information as Information
import qualified OpenBrain.Data.User as User
import qualified OpenBrain.Website.Html.Decorator as Decorator
import qualified OpenBrain.Website.Html.Images as Images

{- Displaying informations: -}
viewSingle :: Information -> OBW H.Html
viewSingle i = do
  -- Gathering information necessary to display:
  content <- mkContent i
  rels    <- relations i
  -- Generating the attributes for the H.div:
  let deleted     = (isJust $ Information.deletion i) ? ([A.class_ "deleted"],[])
      attributes  = [A.class_ "Information"] ++ deleted
  -- Constructing the Html:
  return $ foldl (!) H.div attributes $ do
    title i
    when (not . null $ Information.description i) $ do
      description i >> H.hr
    tColumns [content, rels]
    H.hr >> footnotes True i
  where
    mkContent :: Information -> OBW H.Html
    mkContent i
      | Information.isContent (Information.media i) = do
          let content = H.toHtml . Information.getContent $ Information.media i
          return $ H.div ! A.class_ "InformationContent" $ content
      | otherwise = return $ "Displaying Collections and Discussions not implemented." -- FIXME handle collections and discussions.

viewMany :: Count -> Limit -> Offset -> [Information] -> OBW H.Html
viewMany count limit offset is = do
  return $ do
    "A List of informations:" >> list False is

type Selectable = Bool
list :: Selectable -> [Information] -> H.Html
list selectable is = let item = selectable ? (H.li ! A.class_ "selectable", H.li)
                     in  H.ul ! A.class_ "InformationList" $ mapM_ (item . preview) is

preview :: Information -> H.Html
preview i = H.div ! A.class_ "InformationPreview" $ do
  let displayId = show . unwrap . toId . Information.informationId
  H.a ! A.href (toHref "information.html" ["display=" ++ displayId i]) $ do
    title i
  description i >> H.hr >> footnotes False i

title :: Information -> H.Html
title i = let iAttr = H.dataAttribute "InformationId" (H.toValue . show $ Information.informationId i)
          in  H.h1 ! iAttr ! A.class_ "InformationTitle" $ H.toHtml $ Information.title i

description :: Information -> H.Html
description i = H.div ! A.class_ "InformationDescription" $ H.toHtml $ Information.description i

relations :: Information -> OBW H.Html
relations i = do
  let iid = Information.informationId i
  parents'    <- liftOBB $ OBB.getRelations iid RelationTarget (Just Parent)  True
  children'   <- liftOBB $ OBB.getRelations iid RelationSource (Just Parent)  True
  attackers'  <- liftOBB $ OBB.getRelations iid RelationTarget (Just Attack)  False
  supporters' <- liftOBB $ OBB.getRelations iid RelationTarget (Just Defense) False
  victims'    <- liftOBB $ OBB.getRelations iid RelationSource (Just Attack)  False
  protegee'   <- liftOBB $ OBB.getRelations iid RelationSource (Just Defense) False
  let lookup = mapM $ liftOBB . OBB.getInformation
  parents     <- lookup $ map source parents'
  children    <- lookup $ map target children'
  attackers   <- lookup $ map source attackers'
  supporters  <- lookup $ map source supporters'
  victims     <- lookup $ map target victims'
  protegee    <- lookup $ map target protegee'
  return $ H.div ! A.id "InformationRelations" $ do
    unless (null parents) $ do
      H.div ! A.id "InformationParents" $ do
        H.h2 "Parents:"
        mkList "Parent" $ zip parents parents'
    unless (null children) $ do
      H.div ! A.id "InformationChildren" $ do
        H.h2 "Children:"
        mkList "Child" $ zip children children'
    unless (null attackers) $ do
      H.div ! A.id "InformationAttackers" $ do
        H.h2 "Attackers:"
        mkList "Attacker" $ zip attackers attackers'
    unless (null supporters) $ do
      H.div ! A.id "InformationSupporters" $ do
        H.h2 "Supporters:"
        mkList "Supporter" $ zip supporters supporters'
    unless (null victims) $ do
      H.div ! A.id "InformationVictims" $ do
        H.h2 "Victims:"
        mkList "Victim" $ zip victims victims'
    unless (null protegee) $ do
      H.div ! A.id "InformationProtegee" $ do
        H.h2 "Protegee:"
        mkList "Protege" $ zip protegee protegee'
  where
    mkList :: H.AttributeValue -> [(Information, Relation)] -> H.Html
    mkList liClass xs = H.ul ! A.class_ "RelationList" $ forM_ xs $ \(x, xRel) -> H.li ! A.class_ liClass $ do
      let rid     = H.toValue . show $ relationId xRel
          xHref   = H.toValue . ("/information.html?display="++). show . unwrap . toId $ Information.informationId x
          xTitle  = H.toValue $ Information.description x
      H.dl ! A.class_ "RelationDescription" ! H.dataAttribute "relationId" rid $ do
        H.dt "Name"
        H.dd $ H.a ! A.href xHref ! A.title xTitle $ H.toHtml $ Information.title x
        unless (null $ comment xRel) $ do
          H.dt "Comment"
          H.dd . H.toHtml $ comment xRel
        H.dt "Created"
        H.dd . H.toHtml $ creation xRel
        when (isJust $ deletion xRel) $ do
          H.dt "Deleted"
          H.dd . H.toHtml . fromJust $ deletion xRel

type ShowEditLink = Bool
footnotes :: ShowEditLink -> Information -> H.Html
footnotes sel i = H.dl ! A.class_ "InformationFootnotes" $ do
  H.dt "Created"
  H.dd . H.toHtml $ Information.creation i
  when (isJust $ Information.deletion i) $ do
    H.dt "Deleted"
    H.dd . H.toHtml . fromJust $ Information.deletion i
  H.dt "Author"
  H.dd . H.toHtml . User.username $ Information.author i
  when sel $ do
    H.dt "Edit"
    let href = H.toValue $ "/edit/" ++ (show . unwrap . toId $ Information.informationId i)
    H.dd $ H.a ! A.href href $ Images.edit' "Edit this Information" "Edit this Information"
    H.dt "Collect"
    H.dd ! A.id "InformationBookmark" $ Images.bookmark' "Collect this Information." "Collect this Information."

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
getItems :: OBW [InformationId]
getItems = liftM (map (fromId . wrap) . read) $ look "items"

{- May fail -}
getDisplay :: OBW InformationId
getDisplay = liftM fromId $ lookRead "display"

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
  limit   <- getLimit
  offset  <- getOffset
  count   <- liftOBB $ OBB.getInformationCount
  is      <- liftOBB $ OBB.getInformations limit offset
  ok . toResponse =<< Decorator.page =<< viewMany count limit offset is

{- /information.html?after=_&limit=_&offset=_ -}
serveAfter :: OBW Response
serveAfter = do
  after   <- getAfter
  limit   <- getLimit
  count   <- liftOBB $ OBB.getInformationCountAfter after
  offset  <- getOffset
  is      <- liftOBB $ OBB.getInformationsAfter after limit offset
  ok . toResponse =<< Decorator.page =<< viewMany count limit offset is

{- /information.html?user=_&limit=_&offset=_ -}
serveUser :: OBW Response
serveUser = do
  uid     <- getUser
  limit   <- getLimit
  offset  <- getOffset
  count   <- liftOBB $ OBB.getInformationCountBy uid
  is      <- liftOBB $ OBB.getInformationBy uid limit offset
  ok . toResponse =<< Decorator.page =<< viewMany count limit offset is

{- /information.html?items=[..] -}
serveItems :: OBW Response
serveItems = do
  iids <- getItems
  is <- liftOBB $ mapM OBB.getInformation iids
  let relationEdit  = H.div ! A.id "RelationEditor" $ do
        let rCaption = "Remove selected Informations from list."
        H.div ! A.id "InformationRemoveSelected" $ Images.remove' rCaption rCaption
      content       = H.div ! A.class_ "Information" $ tColumns [list True is, relationEdit]
  ok . toResponse =<< Decorator.page content

{- /information.html?display=_ -}
serveSingle :: OBW Response
serveSingle = do
  iid <- getDisplay
  handleFail "Can't find requested information." $ do
    i   <- liftOBB $ OBB.getInformation iid
    ok . toResponse =<< Decorator.page =<< viewSingle i
