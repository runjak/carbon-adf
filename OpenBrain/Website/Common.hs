{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Common where
{-
  Stuff to be included in various OpenBrain.Website modules.
-}

import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.String (IsString(..))
import Happstack.Server
import System.Time
import Text.Blaze ((!))
import Text.Blaze.Html (ToMarkup(..))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- For fun with OverloadedStrings pragma
instance IsString Response where
  fromString = toResponse

instance ToMarkup CalendarTime where
  toMarkup t =  H.toHtml $
    show (ctHour t) ++ ":" ++ show (ctMin t) ++ " " ++ show (ctDay t) ++ "." ++ show (ctMonth t) ++ " " ++ show (ctYear t)

