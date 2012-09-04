{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Html.Users (userList, pageSelection, serve) where
{-
  This module deals with displaying a list of users and their profiles.
  This has to deal with privacy issues.
-}

import Control.Monad.State
import Control.Monad.Trans
import Happstack.Server as S
import Prelude hiding (head)
import Text.Blaze ((!))
import Text.Blaze.Html (ToMarkup(..))

import qualified Prelude as P (head)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import OpenBrain.Backend.Types as Types
import OpenBrain.Data.Karma
import OpenBrain.Data.User
import OpenBrain.Website.Common
import OpenBrain.Website.Html.Decorator
import OpenBrain.Website.Monad

import qualified OpenBrain.Backend.Monad as OBB

userList :: Types.Limit -> Types.Offset -> OBW H.Html
userList limit offset = do
  b         <- gets backend
  userdata  <- liftOBB $ OBB.getUsers =<< OBB.getUserList limit offset
  return $ H.ul ! A.class_ "userList" $
    forM_ userdata $
      \ud -> H.li ! A.class_ "user" $ do
        when (isAdmin ud) "Admin: "
        (H.a ! A.href (H.toValue . ("user/"++) $ username ud)) $ do
          H.toHtml $ username ud
          H.toHtml $ " (" ++ show (fromKarma $ karma ud) ++ ")"

pageSelection :: OBW H.Html
pageSelection = do
  limit   <- getLimit
  offset  <- getOffset
  count   <- liftOBB OBB.getUserCount
  let page  = offset `div` limit
  let prevs = take 5 . drop 1 . takeWhile (>= 0) $ iterate (subtract limit) offset
  let nexts = take 5 . drop 1 . takeWhile (<= (count - limit)) $ iterate (+ limit) offset
  return $ H.ul ! A.class_ "pageSelection" $ do
    unless (null prevs) $
      H.li $ H.a ! A.href (toHref "users" ["limit=" ++ show limit, "offset=" ++ show (P.head prevs)]) $ "previous"
    toLink limit . reverse . zip prevs . drop 1 $ iterate (subtract 1) page
    H.li $ H.toHtml page
    toLink limit . zip nexts . drop 1 $ iterate (+ 1) page
    unless (null nexts) $
      H.li $ H.a ! A.href (toHref "users" ["limit=" ++ show limit, "offset=" ++ show (P.head nexts)]) $ "next"
  where
    toLink :: Limit -> [(Offset,Int)] -> H.Html
    toLink limit targets = forM_ targets $ \(o, p) ->
      H.li $ H.a ! A.href (toHref "users" ["limit=" ++ show limit, "offset=" ++ show o]) $ H.toHtml p

serve :: OBW Response
serve = do
  limit       <- getLimit
  offset      <- getOffset
  uList       <- userList limit offset
  pSelection  <- pageSelection
  html <- head (uList >> pSelection)
  lift $ ok $ toResponse html

getLimit :: OBW Limit
getLimit = msum [lookRead "limit", return 30]

getOffset :: OBW Offset
getOffset = msum [lookRead "offset", return 0]
