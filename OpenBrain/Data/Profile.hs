{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Data.Profile (
    Profile(..)
  , AccessRule(..)
  , Name(..)
  , Location(..)
  , ProfileSnippet(..)
  , emptyProfile
) where
{-
  Definition of Profile information that can be attached to a UserId.
  Profiles will be displayed on the page as HTML and RDF
  and will be exchanged with the Client via JSON to edit them.
  For RDF encoding I'll look for an existing schema,
  keeping in mind that RDF is not a priority now.
-}

import Control.Applicative
import Control.Monad
import Data.Aeson as A
import Data.Text (pack)

import OpenBrain.Data.Id

data Profile = Profile {
    profileId         :: Id
  , userId            :: Id
  , accessRule        :: AccessRule
  , name              :: Maybe Name
  , avatar            :: Maybe String -- URL to image
  , locations         :: [Location]
  , websites          :: [ProfileSnippet]
  , emails            :: [ProfileSnippet]
  , instantMessagers  :: [ProfileSnippet]
} deriving (Eq, Ord, Read, Show)

data AccessRule = 
    Everyone    -- Every client may view this Information
  | Registered  -- Only registered clients may view this Information
  | None        -- Nobody except self and admins may view this Information
    deriving (Eq, Ord, Read, Show)

data Name = Name {
	  prefix      :: String
	, foreName    :: String
	, middleName  :: String
	, familyName  :: String
  , suffix      :: String
} deriving (Eq, Ord, Read, Show)

data Location = Location {
    street   :: String
  , city     :: String
  , state    :: String
  , land     :: String
  , zipCode  :: String
  , note     :: String
} deriving (Eq, Ord, Read, Show)

data ProfileSnippet = ProfileSnippet {
    title       :: String
  , description :: String
  , target      :: String {- can be an url, email address, instant messaging handle, etc. -}
} deriving (Eq, Ord, Read, Show)

{- Below some functions and Interfaces to work with profiles -}

emptyProfile :: UserId -> Profile
emptyProfile u = Profile u None Nothing Nothing [] [] [] []

instance ToJSON Profile where
  toJSON p = object [
      "userId"            .= userId p
    , "accessRule"        .= accessRule p
    , "name"              .= name p
    , "avatar"            .= avatar p
    , "locations"         .= locations p
    , "websites"          .= websites p
    , "emails"            .= emails p
    , "instantMessagers"  .= instantMessagers p
    ]

instance FromJSON Profile where
  parseJSON (Object v) = Profile <$>
    v .: "userId" <*>
    v .: "accessRule" <*>
    v .: "name" <*>
    v .: "avatar" <*>
    v .: "locations" <*>
    v .: "websites" <*>
    v .: "emails" <*>
    v .: "instantMessagers"
  parseJSON _ = mzero

instance ToJSON AccessRule where
  toJSON = String . pack . show

instance FromJSON AccessRule where
  parseJSON (String "Everyone")   = return Everyone
  parseJSON (String "Registered") = return Registered
  parseJSON (String "None")       = return None
  parseJSON _ = mzero

instance ToJSON Name where
  toJSON n = object [
      "prefix"      .= prefix n
    , "foreName"    .= foreName n
    , "middleName"  .= middleName n
    , "familyName"  .= familyName n
    , "suffix"      .= suffix n
    ]

instance FromJSON Name where
  parseJSON (Object v) = Name <$>
    v .: "prefix" <*>
    v .: "foreName" <*>
    v .: "middleName" <*>
    v .: "familyName" <*>
    v .: "suffix"
  parseJSON _ = mzero

instance ToJSON Location where
  toJSON l = object [
      "street"  .= street l
    , "city"    .= city l
    , "state"   .= state l
    , "land"    .= land l
    , "zipCode" .= zipCode l
    , "note"    .= note l
    ]

instance FromJSON Location where
  parseJSON (Object v) = Location <$>
    v .: "street" <*>
    v .: "city" <*>
    v .: "state" <*>
    v .: "land" <*>
    v .: "zipCode" <*>
    v .: "note"
  parseJSON _ = mzero

instance ToJSON ProfileSnippet where
  toJSON (ProfileSnippet title description target) = object [
      "title"       .= title
    , "description" .= description
    , "target"      .= target
    ]

instance FromJSON ProfileSnippet where
  parseJSON (Object v) = ProfileSnippet <$>
    v .: "title" <*>
    v .: "description" <*>
    v .: "target"
  parseJSON _ = mzero
