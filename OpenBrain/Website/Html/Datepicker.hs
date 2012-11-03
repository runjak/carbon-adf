{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Html.Datepicker where

import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

datepicker :: H.Html
datepicker = H.div ! A.class_ "datepicker" $ do
  H.input ! A.type_ "date" ! A.name "date"
  H.input ! A.type_ "time" ! A.name "time"
