{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Data.Json where
{-|
  This module provides instances for [To,From]JSON
  so that modules in OpenBrain.Website.Json can handle data nicely.
|-}
import Control.Applicative as A
import Control.Monad
import Data.Aeson as Aeson
import Happstack.Server as S
import System.Time
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap

import OpenBrain.Data as Data

jsonResponse :: (ToJSON j) => j -> Response
jsonResponse = setHeader "Content-Type" "application/json" . toResponse . encode

instance ToJSON ActionStatus where
  toJSON s = object ["actionSuccess" .= actionSuccess s, "actionMessage" .= actionMessage s]
instance FromJSON ActionStatus where
  parseJSON (Object v) = ActionStatus <$> v .: "actionSuccess" <*> v .: "actionMessage"
  parseJSON _ = mzero

merge :: (ToJSON a, ToJSON b) => a -> b -> Value
merge a b = merge' (toJSON a) (toJSON b)
  where
    merge' :: Value -> Value -> Value
    merge' (Object o1) (Object o2) = Object $ HashMap.union o1 o2
    merge' o1@(Object _) _ = o1
    merge' _ o2@(Object _) = o2
    merge' x _ = x

{-| This will look close to the ISO norm for dates. |-}
instance ToJSON CalendarTime where
  toJSON = toJSON . stringify
    where
      stringify t = let y   = show $ ctYear t
                        m   = show . (+1) . fromEnum $ ctMonth t
                        d   = show $ ctDay t
                        h   = show $ ctHour t
                        min = show $ ctMin t 
                    in y++"-"++m++"-"++d++" "++h++":"++min

{-|
  Notice how this instance doesn't deliver the password hash,
  and therefore FromJSON isn't possible.
|-}
instance ToJSON UserData where
  toJSON u = object [
               "id"           .= userid u
             , "username"     .= username u
             , "karma"        .= fromKarma (karma u)
             , "userCreation" .= userCreation u
             , "lastLogin"    .= lastLogin u
             , "isAdmin"      .= isAdmin u
             , "profile"      .= profile u
             ]

instance ToJSON Information where
  toJSON i = object [
               "author"      .= author i
             , "creation"    .= informationCreation i
             , "deletion"    .= informationDeletion i
             , "description" .= iDescription i
             , "id"          .= informationId i
             , "media"       .= media i
             , "title"       .= iTitle i
             ]

instance ToJSON Media where
  toJSON (Content c) = toJSON c
  toJSON m = object [
               "arguments"      .= arguments m
             , "collectionType" .= collectionType m
             , "discussion"     .= discussion m
             ]

instance ToJSON CollectionType where
  toJSON = toJSON . show

instance ToJSON DiscussionInfo where
  toJSON d = object [
               "choices"      .= choices d
             , "complete"     .= complete d
             , "deadline"     .= deadline d
             , "participants" .= participants d
             ]
