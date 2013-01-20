{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Html.Relation (relations) where

import Data.Maybe

import OpenBrain.Website.Common

relations :: Information -> OBW HTML
relations i = do
  let iid = informationId i
  -- Types of Relations:
  parents'    <- liftOBB $ GetRelations iid RelationTarget (Just Parent)  True
  children'   <- liftOBB $ GetRelations iid RelationSource (Just Parent)  True
  attackers'  <- liftOBB $ GetRelations iid RelationTarget (Just Attack)  False
  supporters' <- liftOBB $ GetRelations iid RelationTarget (Just Defense) False
  victims'    <- liftOBB $ GetRelations iid RelationSource (Just Attack)  False
  protegee'   <- liftOBB $ GetRelations iid RelationSource (Just Defense) False
  -- Informations behind the Relations:
  let lookup = noMaybes . mapM (liftOBB . GetInformation)
  parents     <- lookup $ map source parents'
  children    <- lookup $ map target children'
  attackers   <- lookup $ map source attackers'
  supporters  <- lookup $ map source supporters'
  victims     <- lookup $ map target victims'
  protegee    <- lookup $ map target protegee'
  -- Building the content:
  let buildInfos = concat [
          null parents    ? ([], [RLBI "Parents"    "InformationParents"    False "Parent"    $ zip parents parents'])
        , null children   ? ([], [RLBI "Children"   "InformationChildren"   False "Child"     $ zip children children'])
        , null attackers  ? ([], [RLBI "Attackers"  "InformationAttackers"  True  "Attacker"  $ zip attackers attackers'])
        , null supporters ? ([], [RLBI "Supporters" "InformationSupporters" True  "Supporter" $ zip supporters supporters'])
        , null victims    ? ([], [RLBI "Victims"    "InformationVictims"    True  "Victim"    $ zip victims victims'])
        , null protegee   ? ([], [RLBI "Protegee"   "InformationProtegee"   True  "Protege"   $ zip protegee protegee'])
        ]
  contextInfos <- mapM buildToContextInfo buildInfos
  let context "Relation" = MuList $ map (mkStrContext . relationContext) contextInfos
  liftIO $ tmpl "Relations.html" context
  where
    buildToContextInfo :: RelationListBuildInfo -> OBW RelationContextInfo
    buildToContextInfo (RLBI title id deletable rclass items) = do
      h <- relationList deletable rclass items
      return $ RCI title id h

    relationContext (RCI t i h) "RelationType"  = MuVariable i
    relationContext (RCI t i h) "RelationTitle" = MuVariable t
    relationContext (RCI t i h) "RelationList"  = htmlToMu h

data RelationListBuildInfo = RLBI {
    rlbiTitle         :: String
  , rlbiId            :: String
  , rlbiDeletable     :: Bool
  , rlbiRelationClass :: String
  , rlbiItems         :: [(Information, Relation)]  
  }

data RelationContextInfo = RCI {
    rciTitle :: String
  , rciId    :: String
  , rciList  :: HTML
  }

relationList :: Bool -> String -> [(Information, Relation)] -> OBW HTML
relationList deletable relationClass irs = do
  descriptions <- mapM (relationDescription deletable) irs
  let cds = zip (repeat relationClass) descriptions
      context "RelationList" = MuList $ map (mkStrContext . descriptionContext) cds
  liftIO $ tmpl "RelationList.html" context
  where
    descriptionContext (rClass, _) "RelationClass"       = MuVariable rClass
    descriptionContext (_, rDesc)  "RelationDescription" = htmlToMu rDesc

relationDescription :: Bool -> (Information, Relation) -> OBW HTML
relationDescription deletable (i, r) = do
  let context "rid"                    = MuVariable . show $ relationId r
      context "InformationLink"        = MuVariable . ("/information.html?display="++) 
                                       . show . unwrap . toId $ informationId i
      context "InformationDescription" = MuVariable $ description i
      context "InformationTitle"       = MuVariable $ title i
      context "HasRelationComment"     = MuBool . not . null $ comment r
      context "RelationComment"        = MuVariable $ comment r
      context "RelationCreated"        = MuVariable $ creation r 
      context "IsDeleted"              = MuBool . isJust $ deletion r
      context "Deletion"               = MuVariable . fromJust $ deletion r
      context "IsDeletable"            = MuBool deletable
  liftIO $ tmpl "RelationDescription.html" context

