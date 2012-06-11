{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module OpenBrain.Website.User () where
{-
  Displaying information regarding a single user to a Client.
  Narf - we need some guildelines here to ensure data safety and ++privacy
-}

import OpenBrain.Data.Profile (Profile, AccessRule, Name, Location, ProfileSnippet)
import qualified OpenBrain.Data.Profile as P

import Text.Blaze ((!))
import Text.Blaze.Html (ToMarkup(..))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

instance ToMarkup Profile where
  toMarkup p = (H.div ! A.class_ "userProfile") $ do
    case P.name p of
      Nothing -> ""
      (Just n) -> (H.div ! A.class_ "userName") $ do
        H.div ! A.class_ "prefix"     $ H.toHtml $ P.prefix n
        H.div ! A.class_ "foreName"   $ H.toHtml $ P.foreName n
        H.div ! A.class_ "middleName" $ H.toHtml $ P.middleName n
        H.div ! A.class_ "familyName" $ H.toHtml $ P.familyName n
        H.div ! A.class_ "suffix"     $ H.toHtml $ P.suffix n
    case P.avatar p of
      Nothing -> ""
      (Just a) -> H.img ! A.class_ "avatar" ! (A.src $ H.toValue a)
    case P.locations p of
      [] -> ""
      locations -> H.ul ! A.class_ "userLocations" $ flip mapM_ locations $
        \l -> H.li . (H.dl ! A.class_ "userLocation") $ do
            H.dt "Street"   >> (H.dd . H.toHtml $ P.street l)
            H.dt "City"     >> (H.dd . H.toHtml $ P.city l)
            H.dt "State"    >> (H.dd . H.toHtml $ P.state l)
            H.dt "Land"     >> (H.dd . H.toHtml $ P.land l)
            H.dt "ZipCode"  >> (H.dd . H.toHtml $ P.zipCode l)
            H.dt "Note"     >> (H.dd . H.toHtml $ P.note l)
    case P.websites p of
      [] -> ""
      websites -> H.dl ! A.class_ "userWebsites" $ flip mapM_ websites $
        \w -> do
          H.dt $ H.a ! (A.href . H.toValue $ P.target w) $ H.toHtml $ P.title w
          H.dd . H.toHtml $ P.description w
    case P.emails p of
      [] -> ""
      emails -> H.dl ! A.class_ "userEmails" $ flip mapM_ emails $
        \e -> do
          H.dt . H.toHtml $ P.title e ++ ": " ++ P.target e
          H.dd . H.toHtml $ P.description e
    case P.instantMessagers p of
      [] -> ""
      ims -> H.dl ! A.class_ "userIms" $ flip mapM_ ims $
        \i -> do
          H.dt . H.toHtml $ P.title i ++ ": " ++ P.target i
          H.dd . H.toHtml $ P.description i
