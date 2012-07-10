{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Profile () where

import Control.Monad
import Control.Monad.State
import Data.Maybe
import Text.Blaze ((!))
import Text.Blaze.Html (ToMarkup(..))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import OpenBrain.Data.Profile (Profile)
import qualified OpenBrain.Data.Profile as P
import OpenBrain.Common
import OpenBrain.Website.Common
import OpenBrain.Website.Monad

instance ToMarkup Profile where
  toMarkup p = (H.div ! A.class_ "userProfile") $ do
    let n = fromMaybe P.emptyName $ P.name p
    (H.div ! A.class_ "userName") $ do
      H.div ! A.class_ "prefix"     $ H.toHtml $ P.prefix n
      H.div ! A.class_ "foreName"   $ H.toHtml $ P.foreName n
      H.div ! A.class_ "middleName" $ H.toHtml $ P.middleName n
      H.div ! A.class_ "familyName" $ H.toHtml $ P.familyName n
      H.div ! A.class_ "suffix"     $ H.toHtml $ P.suffix n
    let avatar = fromMaybe "" $ P.avatar p
    H.img ! A.class_ "avatar" ! A.src (H.toValue avatar)
    let locations = null (P.locations p) ? ([P.emptyLocation], P.locations p)
    H.ul ! A.class_ "userLocations" $ forM_ locations $
      \l -> H.li . (H.dl ! A.class_ "userLocation") $ do
          H.dt "Street"   >> (H.dd . H.toHtml $ P.street l)
          H.dt "City"     >> (H.dd . H.toHtml $ P.city l)
          H.dt "State"    >> (H.dd . H.toHtml $ P.state l)
          H.dt "Land"     >> (H.dd . H.toHtml $ P.land l)
          H.dt "ZipCode"  >> (H.dd . H.toHtml $ P.zipCode l)
          H.dt "Note"     >> (H.dd . H.toHtml $ P.note l)
    let websites = null (P.websites p) ? ([P.emptySnippet], P.websites p)
    H.dl ! A.class_ "userWebsites" $ forM_ websites $
      \w -> do
        H.dt $ H.a ! (A.href . H.toValue $ P.target w) $ H.toHtml $ P.title w
        H.dd . H.toHtml $ P.description w
    let emails = null (P.emails p) ? ([P.emptySnippet], P.emails p)
    H.dl ! A.class_ "userEmails" $ forM_ emails $
      \e -> do
        H.dt $ do
          (H.div ! A.class_ "title")  $ H.toHtml $ P.title e
          unless (null $ P.title e) ":"
          (H.div ! A.class_ "target") $ H.toHtml $ P.target e
        H.dd . H.toHtml $ P.description e
    let ims = null (P.instantMessagers p) ? ([P.emptySnippet], P.instantMessagers p)
    H.dl ! A.class_ "userIms" $ forM_ ims $
      \i -> do
        H.dt $ do
          (H.div ! A.class_ "title")  $ H.toHtml $ P.title i
          unless (null $ P.title i) ":"
          (H.div ! A.class_ "target") $ H.toHtml $ P.target i
        H.dd . H.toHtml $ P.description i

