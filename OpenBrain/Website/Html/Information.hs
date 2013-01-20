{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Html.Information (serve, viewSingle) where

import Data.Maybe
import Happstack.Server as S
import System.Time (CalendarTime)

import OpenBrain.Backend
import OpenBrain.Common
import OpenBrain.Website.Common
import OpenBrain.Website.Monad
import OpenBrain.Website.Session (chkSession)
import OpenBrain.Website.Template
import qualified OpenBrain.Website.Parameters      as Parameters
import qualified OpenBrain.Website.Html.Datepicker as Datepicker
import qualified OpenBrain.Website.Html.Decorator  as Decorator
import qualified OpenBrain.Website.Html.Relation   as Relation

informationDisplayLink :: Information -> String
informationDisplayLink = ("/information.html?display="++) . show
                       . unwrap . toId . informationId

renderTitle :: Information -> OBW HTML
renderTitle i = do
  removeable <- msum [Parameters.getItems >> return True, return False]
  let context "iid" = MuVariable . show $ informationId i
      context "InformationTitle" = MuVariable $ title i
      context "Removeable" = MuBool removeable
  liftIO $ tmpl "InformationTitle.html" context

renderDescription :: Information -> OBW HTML
renderDescription i = do
  let context "InformationDescription" = MuVariable $ description i
  liftIO $ tmpl "InformationDescription.html" context

type ShowControls = Bool
type LoggedIn     = Bool
footnotes :: LoggedIn -> ShowControls -> Information -> OBW HTML
footnotes loggedIn sControls i = do
  let context "InformationCreated"  = MuVariable $ creation i
      context "IsDeleted"           = MuBool . isJust $ deletion i
      context "InformationDeletion" = MuVariable . fromJust $ deletion i
      context "Author"              = MuVariable . username $ author i
      context "DisplayControls"     = MuBool sControls
      context "EditLink"            = MuVariable . ("/edit/"++) . show . unwrap
                                    . toId $ informationId i
      context "LoggedIn"            = MuBool loggedIn
  liftIO $ tmpl "InformationFootnotes.html" context

preview :: Information -> OBW HTML
preview i = do
  t <- renderTitle i
  d <- renderDescription i
  f <- footnotes False False i
  let context "InformationLink"        = MuVariable . ("information.html?display="++)
                                       . show . unwrap . toId $ informationId i
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
  datepicker <- liftIO Datepicker.datepicker
  let context "Datepicker" = htmlToMu datepicker
  liftIO $ tmpl "RelationEditor.html" context

informationMain :: Information -> OBW HTML
informationMain i = let isC = isContent $ media i
                    in  isC ? (informationContent i, informationCollection i)

informationContent :: Information -> OBW HTML
informationContent i = do
  let context "Content" = MuVariable . getContent $ media i
  liftIO $ tmpl "InformationContent.html" context

informationCollection :: Information -> OBW HTML
informationCollection i = do
  let m = media i
  -- Describing the Collection:
  let cType =
        case collectionType m of
             Choice                  -> "This is a choice to a discussion." :: String
             Decision                -> "This is a decision from a discussion."
             DiscussionAttackOnly    -> "This is a discussion that allowes only attack relations."
             DiscussionAttackDefense -> "This is a discussion with attack and defense relations."
  args <- noMaybes . mapM (\i -> liftOBB $ GetInformation i) $ arguments m
  -- Displaying the Arguments:
  let argumentContext i "ArgumentLink"  = MuVariable $ informationDisplayLink i
      argumentContext i "ArgumentTitle" = MuVariable $ title i
  -- Handling Discussions:
  let dComplete c "ResultLink"     = MuVariable $ informationDisplayLink c
      dComplete c "ResultTitle"    = MuVariable $ title c
      dChoiceList c "ChoiceId"     = MuVariable . show . informationId $ fst c
      dChoiceList c "ChoiceLink"   = MuVariable . informationDisplayLink $ fst c
      dChoiceList c "ChoiceTitle"  = MuVariable $ title (fst c) ++ " (" ++ show (snd c) ++ ")"
      dPList p "ParticipantLink"   = MuVariable . ("/user.html?display="++) . show . unwrap . toId . userid $ fst p
      dPList p "ParticipantTitle"  = MuVariable $ username (fst p) ++ (snd p ? (" (voted)",""))
      dContext d "Deadline"        = MuVariable $ deadline d
      dContext d "IsComplete"      = MuList . map (mkStrContext . dComplete) . maybeToList $ complete d
      dContext d "HasChoices"      = MuBool . not . null $ choices d
      dContext d "ChoiceList"      = MuList . map (mkStrContext . dChoiceList) $ choices d
      dContext d "ParticipantList" = MuList . map (mkStrContext . dPList) $ participants d
  -- Getting it all together:
  let context "CollectionType" = MuVariable cType
      context "ArgumentList"   = MuList $ map (mkStrContext . argumentContext) args
      context "IsDiscussion"   = MuList . map (mkStrContext . dContext) . maybeToList $ discussion m
      context _ = MuNothing
  liftIO $ tmpl "InformationCollection.html" context

{- Displaying informations: -}
viewSingle :: Information -> OBW HTML
viewSingle i = do
  loggedIn <- msum [liftM (const True) chkSession, return False]
  t        <- renderTitle i
  d        <- renderDescription i
  ic       <- informationMain i
  rels     <- Relation.relations i
  f        <- footnotes loggedIn True i
  let context "Title"          = htmlToMu t
      context "Deleted"        = MuBool . isJust $ deletion i
      context "HasDescription" = MuBool . not . null $ description i
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
  count   <- liftOBB GetInformationCount
  is      <- liftOBB $ GetInformations limit offset
  ok . toResponse =<< Decorator.page =<< viewMany count limit offset is

{- /information.html?after=_&limit=_&offset=_ -}
serveAfter :: OBW Response
serveAfter = do
  after   <- Parameters.getAfter
  limit   <- Parameters.getLimit
  count   <- liftOBB $ GetInformationCountAfter after
  offset  <- Parameters.getOffset
  is      <- liftOBB $ GetInformationsAfter after limit offset
  ok . toResponse =<< Decorator.page =<< viewMany count limit offset is

{- /information.html?user=_&limit=_&offset=_ -}
serveUser :: OBW Response
serveUser = do
  uid     <- Parameters.getUser
  limit   <- Parameters.getLimit
  offset  <- Parameters.getOffset
  count   <- liftOBB $ GetInformationCountBy uid
  is      <- liftOBB $ GetInformationBy uid limit offset
  ok . toResponse =<< Decorator.page =<< viewMany count limit offset is

{- /information.html?items=[..] -}
serveItems :: OBW Response
serveItems = do
  iids    <- Parameters.getItems
  is      <- noMaybes $ mapM (\i -> liftOBB $ GetInformation i) iids
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
    i   <- noMaybe . liftOBB $ GetInformation iid
    ok . toResponse =<< Decorator.page =<< viewSingle i
