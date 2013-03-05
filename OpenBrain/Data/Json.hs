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

import OpenBrain.Data as Data

jsonResponse :: (ToJSON j) => j -> Response
jsonResponse = setHeader "Content-Type" "application/json" . toResponse . encode

instance ToJSON ActionStatus where
  toJSON s = object ["success" .= actionSuccess s, "message" .= actionMessage s]
instance FromJSON ActionStatus where
  parseJSON (Object v) = ActionStatus <$> v .: "success" <*> v .: "message"
  parseJSON _ = mzero

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
               "userid"       .= userid u
             , "username"     .= username u
             , "karma"        .= fromKarma (karma u)
             , "userCreation" .= userCreation u
             , "lastLogin"    .= lastLogin u
             , "isAdmin"      .= isAdmin u
             , "profile"      .= profile u
             ]
