{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Html.Information (serve, viewSingle) where

import Data.Maybe
import Happstack.Server as S
import System.Time (CalendarTime)
import Text.Hastache
import Text.Hastache.Context

import OpenBrain.Backend.Types as Types hiding (CreateInformation(..))
import OpenBrain.Common
import OpenBrain.Data.Id
import OpenBrain.Data.Information (Information)
import OpenBrain.Data.Relation
import OpenBrain.Website.Common
import OpenBrain.Website.Monad
import OpenBrain.Website.Session (chkSession)
import OpenBrain.Website.Template
import qualified OpenBrain.Backend.Monad as OBB
import qualified OpenBrain.Data.Information as Information
import qualified OpenBrain.Data.User as User
import qualified OpenBrain.Website.Parameters as Parameters
import qualified OpenBrain.Website.Html.Datepicker as Datepicker
import qualified OpenBrain.Website.Html.Decorator as Decorator
import qualified OpenBrain.Website.Html.Relation as Relation

title :: Information -> OBW HTML
title i = do
  let context "iid" = MuVariable . show $ Information.informationId i
      context "InformationTitle" = MuVariable $ Information.title i
  liftIO $ tmpl "InformationTitle.html" context

description :: Information -> OBW HTML
description i = do
  let context "InformationDescription" = MuVariable $ Information.description i
  liftIO $ tmpl "InformationDescription.html" context

type ShowControls = Bool
type LoggedIn     = Bool
footnotes :: LoggedIn -> ShowControls -> Information -> OBW HTML
footnotes loggedIn sControls i = do
  let context "InformationCreated"  = MuVariable . show $ Information.creation i
      context "IsDeleted"           = MuBool . isJust $ Information.deletion i
      context "InformationDeletion" = MuVariable . show . fromJust $ Information.deletion i
      context "Author"              = MuVariable . User.username $ Information.author i
      context "DisplayControls"     = MuBool sControls
      context "EditLink"            = MuVariable . ("/edit/"++) . show . unwrap
                                    . toId $ Information.informationId i
      context "LoggedIn"            = MuBool loggedIn
  liftIO $ tmpl "InformationFootnotes.html" context

preview :: Information -> OBW HTML
preview  i = do
  t <- title i
  d <- description i
  f <- footnotes False False i
  let context "InformationLink"        = MuVariable . ("information.html?display="++)
                                       . show . unwrap . toId $ Information.informationId i
      context "InformationTitle"       = htmlToMu t
      context "InformationDescription" = htmlToMu d
      context "InformationFootnotes"   = htmlToMu f
  liftIO $ tmpl "InformationPreview.html" context

type Selectable = Bool
list :: Selectable -> [Information] -> OBW HTML
list selectable is = do
  hs <- mapM preview is
  let context "items" = MuList $ map (mkStrContext . itemContext) hs
  liftIO $ tmpl "InformationList.html" context
  where
    itemContext _ "selectable" = MuBool selectable
    itemContext h "item"       = htmlToMu h

viewMany :: Count -> Limit -> Offset -> [Information] -> OBW HTML
viewMany c l o is = do
  l <- list False is
  return $ htmlConcat ["A List of informations:", l]

relationEditor :: OBW HTML
relationEditor = do
  datepicker <- liftIO $ Datepicker.datepicker
  let context "Datepicker" = htmlToMu datepicker
  liftIO $ tmpl "RelationEditor.html" context

informationContent :: Information -> OBW HTML
informationContent i = do
  let context "Content" = MuVariable . Information.getContent $ Information.media i
  liftIO $ tmpl "InformationContent.html" context

{- Displaying informations: -}
viewSingle :: Information -> OBW HTML
viewSingle i = do
  loggedIn <- msum [liftM (const True) chkSession, return False]
  t        <- title i
  d        <- description i
  content  <- informationContent i
  rels     <- Relation.relations i
  cols     <- liftIO $ tColumns [content, rels]
  f        <- footnotes loggedIn True i
  let context "Title"          = htmlToMu t
      context "HasDescription" = MuBool . isJust $ Information.deletion i
      context "Description"    = htmlToMu d
      context "Columns"        = htmlToMu cols
      context "Footnotes"      = htmlToMu f
  liftIO $ tmpl "InformationSingleView.html" context

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
  count   <- liftOBB OBB.getInformationCount
  is      <- liftOBB $ OBB.getInformations limit offset
  ok . toResponse =<< Decorator.page =<< viewMany count limit offset is

{- /information.html?after=_&limit=_&offset=_ -}
serveAfter :: OBW Response
serveAfter = do
  after   <- Parameters.getAfter
  limit   <- Parameters.getLimit
  count   <- liftOBB $ OBB.getInformationCountAfter after
  offset  <- Parameters.getOffset
  is      <- liftOBB $ OBB.getInformationsAfter after limit offset
  ok . toResponse =<< Decorator.page =<< viewMany count limit offset is

{- /information.html?user=_&limit=_&offset=_ -}
serveUser :: OBW Response
serveUser = do
  uid     <- Parameters.getUser
  limit   <- Parameters.getLimit
  offset  <- Parameters.getOffset
  count   <- liftOBB $ OBB.getInformationCountBy uid
  is      <- liftOBB $ OBB.getInformationBy uid limit offset
  ok . toResponse =<< Decorator.page =<< viewMany count limit offset is

{- /information.html?items=[..] -}
serveItems :: OBW Response
serveItems = do
  iids    <- Parameters.getItems
  is      <- liftOBB $ mapM OBB.getInformation iids
  relEdit <- relationEditor
  l       <- list True is
  cols    <- liftIO $ tColumns [l, relEdit]
  let context "Content" = htmlToMu cols
  content <- liftIO $ tmpl "InformationItems.html" context
  ok . toResponse =<< Decorator.page content

{- /information.html?display=_ -}
serveSingle :: OBW Response
serveSingle = do
  iid <- liftM fromId Parameters.getDisplay
  handleFail "Can't find requested information." $ do
    i   <- liftOBB $ OBB.getInformation iid
    ok . toResponse =<< Decorator.page =<< viewSingle i
