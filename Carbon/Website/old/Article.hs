{-# LANGUAGE OverloadedStrings #-}
module Carbon.Website.Article where

import Carbon.Website.Common
import qualified Carbon.Website.Description as Description
import qualified Carbon.Website.Session     as Session

pageArticles :: OBW Response
pageArticles = countAndPageBy ArticleCount $ \l o -> liftM responseJSON' $ PageArticles l o

createArticle :: OBW Response
createArticle = plusm createFail $ do
  liftIO $ putStrLn "OpenBrain.Website.Article:createArticle"
  ndid    <- Description.createDescription
  content <- getContent
  aid     <- liftB $ AddArticle ndid content
  readArticle aid
  where
    createFail = respBadRequest $ responseJSON'' "Login required; expected parameters: headline, description, content"

readArticle :: ArticleId -> OBW Response
readArticle = respOk . responseJSON' <=< liftB . GetArticle

updateArticle :: ArticleId -> OBW Response
updateArticle aid = Session.chkSession' $ \author -> plusm updateFail $ do
  liftIO $ putStrLn "OpenBrain.Website.Article:updateArticle"
  actions <- liftM concat $ sequence updates
  guard . not $ null actions
  aid' <- liftB $ do
    aid'' <- Clone aid author
    mapM_ ($ aid'') actions
    deleteArticle aid
    return aid''
  readArticle aid'
  where
    deleteArticle = DeleteDescription <=< liftM (descriptionId . aDescription) . GetArticle
    updateHeadline = plusm (return []) $ do
      h <- Description.getHeadline
      let update = flip SetHeadline h <=< liftM (descriptionId . aDescription) . GetArticle
      return [update]
    updateDescription = plusm (return []) $ do
      d <- Description.getDesc
      let update = flip SetDescription d <=< liftM (descriptionId . aDescription) . GetArticle
      return [update]
    updateContent = plusm (return []) $ do
      c <- getContent
      let update = flip SetContent c
      return [update]
    updates :: [OBW [ArticleId -> BackendDSL ()]]
    updates = [updateHeadline, updateDescription, updateContent]
    updateFail = respBadRequest $ responseJSON'' "Expected parameters are any of: headline, description, content."

deleteArticle :: ArticleId -> OBW Response
deleteArticle aid = Session.chkSession' . const $ do
  liftB $ do
    d <- liftM aDescription $ GetArticle aid
    DeleteDescription $ descriptionId d
  respOk . responseJSON'' $ "Deleted Article:\t" ++ show aid

replaceDummy :: ArticleId -> ArticleId -> OBW Response
replaceDummy dummy replacement = Session.chkSession' . const $ do
  replaced <- liftB $ ReplaceDummy dummy replacement
  let success = respOk $ responseJSON'' "Dummy replaced"
      problem = respNotModified $ responseJSON'' "Could not replace dummy"
  replaced ? (success, problem)

-- | Parameters…
getContent :: OBW String
getContent = liftM sanitize $ lookRead "content"
