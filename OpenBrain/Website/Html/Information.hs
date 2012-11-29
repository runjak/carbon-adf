{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Html.Information (serve, viewSingle) where

import Data.Maybe
import Happstack.Server as S
import System.Time (CalendarTime)

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

informationDisplayLink :: Information -> String
informationDisplayLink = ("/information.html?display="++) . show
                       . unwrap . toId . Information.informationId


title :: Information -> OBW HTML
title i = do
  removeable <- msum [Parameters.getItems >> return True, return False]
  let context "iid" = MuVariable . show $ Information.informationId i
      context "InformationTitle" = MuVariable $ Information.title i
      context "Removeable" = MuBool removeable
  liftIO $ tmpl "InformationTitle.html" context

description :: Information -> OBW HTML
description i = do
  let context "InformationDescription" = MuVariable $ Information.description i
  liftIO $ tmpl "InformationDescription.html" context

type ShowControls = Bool
type LoggedIn     = Bool
footnotes :: LoggedIn -> ShowControls -> Information -> OBW HTML
footnotes loggedIn sControls i = do
  let context "InformationCreated"  = MuVariable $ Information.creation i
      context "IsDeleted"           = MuBool . isJust $ Information.deletion i
      context "InformationDeletion" = MuVariable . fromJust $ Information.deletion i
      context "Author"              = MuVariable . User.username $ Information.author i
      context "DisplayControls"     = MuBool sControls
      context "EditLink"            = MuVariable . ("/edit/"++) . show . unwrap
                                    . toId $ Information.informationId i
      context "LoggedIn"            = MuBool loggedIn
  liftIO $ tmpl "InformationFootnotes.html" context

preview :: Information -> OBW HTML
preview i = do
  t <- title i
  d <- description i
  f <- footnotes False False i
  let context "InformationLink"        = MuVariable . ("information.html?display="++)
                                       . show . unwrap . toId $ Information.informationId i
      context "InformationTitle"       = htmlToMu t
      context "InformationDescription" = htmlToMu d
      context "InformationFootnotes"   = htmlToMu f
  liftIO $ tmpl "InformationPreview.html" context

list :: [Information] -> OBW HTML
list is = do
  selectable <- msum [Parameters.getItems >> return True, return False]
  hs <- mapM preview is
  let itemContext _ "selectable" = MuBool selectable
      itemContext h "item"       = htmlToMu h
      context "items" = MuList $ map (mkStrContext . itemContext) hs
  liftIO $ tmpl "InformationList.html" context

viewMany :: Count -> Limit -> Offset -> [Information] -> OBW HTML
viewMany c l o is = do
  let lBase = "/information.html?offset=" ++ show o
  content <- list is
  ps      <- pages l o c lBase
  return $ htmlConcat [content, ps]

relationEditor :: OBW HTML
relationEditor = do
  datepicker <- liftIO $ Datepicker.datepicker
  let context "Datepicker" = htmlToMu datepicker
  liftIO $ tmpl "RelationEditor.html" context

informationMain :: Information -> OBW HTML
informationMain i = let isC = Information.isContent $ Information.media i
                    in  isC ? (informationContent i, informationCollection i)

informationContent :: Information -> OBW HTML
informationContent i = do
  let context "Content" = MuVariable . Information.getContent $ Information.media i
  liftIO $ tmpl "InformationContent.html" context

informationCollection :: Information -> OBW HTML
informationCollection i = do
  let m = Information.media i
  -- Describing the Collection:
  let collectionType =
        case Information.collectionType m of
             Information.Choice                  -> "This is a choice to a discussion." :: String
             Information.Decision                -> "This is a decision from a discussion."
             Information.DiscussionAttackOnly    -> "This is a discussion that allowes only attack relations."
             Information.DiscussionAttackDefense -> "This is a discussion with attack and defense relations."
  args <- liftOBB . mapM OBB.getInformation $ Information.arguments m
  -- Displaying the Arguments:
  let argumentContext i "ArgumentLink"  = MuVariable $ informationDisplayLink i
      argumentContext i "ArgumentTitle" = MuVariable $ Information.title i
  -- Handling Discussions:
  let dComplete c "ResultLink"     = MuVariable $ informationDisplayLink c
      dComplete c "ResultTitle"    = MuVariable $ Information.title c
      dChoiceList c "ChoiceId"     = MuVariable . show . Information.informationId $ fst c
      dChoiceList c "ChoiceLink"   = MuVariable . informationDisplayLink $ fst c
      dChoiceList c "ChoiceTitle"  = MuVariable $ Information.title (fst c) ++ " (" ++ (show $ snd c) ++ ")"
      dPList p "ParticipantLink"   = MuVariable . ("/user.html?display="++) . show . unwrap . toId . User.userid $ fst p
      dPList p "ParticipantTitle"  = MuVariable $ User.username (fst p) ++ ((snd p) ? (" (voted)",""))
      dContext d "Deadline"        = MuVariable $ Information.deadline d
      dContext d "IsComplete"      = MuList . map (mkStrContext . dComplete) . maybeToList $ Information.complete d
      dContext d "HasChoices"      = MuBool . not . null $ Information.choices d
      dContext d "ChoiceList"      = MuList . map (mkStrContext . dChoiceList) $ Information.choices d
      dContext d "ParticipantList" = MuList . map (mkStrContext . dPList) $ Information.participants d
  -- Getting it all together:
  let context "CollectionType" = MuVariable collectionType
      context "ArgumentList"   = MuList $ map (mkStrContext . argumentContext) args
      context "IsDiscussion"   = MuList . map (mkStrContext . dContext) . maybeToList $ Information.discussion m
      context _ = MuNothing
  liftIO $ putStrLn $ show i
  liftIO $ tmpl "InformationCollection.html" context

{- Displaying informations: -}
viewSingle :: Information -> OBW HTML
viewSingle i = do
  loggedIn <- msum [liftM (const True) chkSession, return False]
  t        <- title i
  d        <- description i
  ic       <- informationMain i
  rels     <- Relation.relations i
  f        <- footnotes loggedIn True i
  let context "Title"          = htmlToMu t
      context "Deleted"        = MuBool . isJust $ Information.deletion i
      context "HasDescription" = MuBool . not . null $ Information.description i
      context "Description"    = htmlToMu d
      context "Content"        = htmlToMu $ htmlConcat [ic, rels]
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
  l       <- list is
  let context "Content" = htmlToMu $ htmlConcat [l, relEdit]
  content <- liftIO $ tmpl "InformationItems.html" context
  ok . toResponse =<< Decorator.page content

{- /information.html?display=_ -}
serveSingle :: OBW Response
serveSingle = do
  iid <- liftM fromId Parameters.getDisplay
  handleFail "Can't find requested information." $ do
    i   <- liftOBB $ OBB.getInformation iid
    ok . toResponse =<< Decorator.page =<< viewSingle i
