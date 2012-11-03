{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Html.Relation where

import Data.Maybe
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import OpenBrain.Backend.Types as Types hiding (CreateInformation(..))
import OpenBrain.Common
import OpenBrain.Data.Id
import OpenBrain.Data.Information (Information)
import OpenBrain.Data.Relation
import OpenBrain.Website.Common
import OpenBrain.Website.Monad
import qualified OpenBrain.Data.Information as Information
import qualified OpenBrain.Website.Html.Images as Images
import qualified OpenBrain.Backend.Monad as OBB

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
    unless (null parents) $
      H.div ! A.id "InformationParents" $ do
        H.h2 "Parents:"
        mkList False "Parent" $ zip parents parents'
    unless (null children) $
      H.div ! A.id "InformationChildren" $ do
        H.h2 "Children:"
        mkList False "Child" $ zip children children'
    unless (null attackers) $
      H.div ! A.id "InformationAttackers" $ do
        H.h2 "Attackers:"
        mkList True "Attacker" $ zip attackers attackers'
    unless (null supporters) $
      H.div ! A.id "InformationSupporters" $ do
        H.h2 "Supporters:"
        mkList True "Supporter" $ zip supporters supporters'
    unless (null victims) $
      H.div ! A.id "InformationVictims" $ do
        H.h2 "Victims:"
        mkList True "Victim" $ zip victims victims'
    unless (null protegee) $
      H.div ! A.id "InformationProtegee" $ do
        H.h2 "Protegee:"
        mkList True "Protege" $ zip protegee protegee'
  where
    mkList :: Bool -> H.AttributeValue -> [(Information, Relation)] -> H.Html
    mkList deleteable liClass xs = H.ul ! A.class_ "RelationList" $ forM_ xs $ \(x, xRel) -> H.li ! A.class_ liClass $ do
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
        when deleteable $ do
          H.dt "Remove"
          H.dd ! A.class_ "removeRelation" $ Images.remove' "Remove Relation" "Remove Relation"
