{-# LANGUAGE OverloadedStrings #-}
module Carbon.Website.Description where

import Carbon.Website.Common
import qualified Carbon.Website.Session as Session

pageDescriptions :: OBW Response
pageDescriptions = countAndPageBy DescriptionCount $ \l o -> liftM responseJSON' $ PageDescriptions l o

createDescription :: OBW NewDescriptionId
createDescription = do
  liftIO $ putStrLn "OpenBrain.Website.Description:createDescription"
  (author, headline, desc) <- liftM3 (,,) Session.chkSession getHeadline getDesc
  liftB $ AddDescription author headline desc

readDescription :: DescriptionId -> OBW Response
readDescription = respOk . responseJSON' <=< liftB . GetDescription

deleteDescription :: DescriptionId -> OBW Response
deleteDescription did = Session.chkSession' . const $ do
  liftB  $ DeleteDescription did
  respOk $ responseJSON'' "Deletion successful."

getHeadline :: OBW Headline
getHeadline = liftM sanitize $ lookRead "headline"

getDesc :: OBW String
getDesc = liftM sanitize $ lookRead "description"
