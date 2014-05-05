{-# LANGUAGE OverloadedStrings #-}
module Carbon.Data.Discussion.Strings where
{-|
  For the Carbon.Data.Discussion module it made sense
  to make sure, that all Strings and such be defined
  in a central place, so I cannot introduce typos by having them
  in different places.
|-}

import Prelude hiding (id)

import Data.String (IsString(..))
import Data.Text (Text)

id           = "id"           :: Text
arguments    = "arguments"    :: Text
deadline     = "deadline"     :: Text
participants = "participants" :: Text
evaluation   = "evaluation"   :: Text

fields = [id, arguments, deadline, participants, evaluation]
