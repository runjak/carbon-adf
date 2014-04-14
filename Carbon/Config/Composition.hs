{-# LANGUAGE OverloadedStrings #-}
module Carbon.Config.Composition where

import Data.Text (Text)
import Control.Monad
import qualified Data.Text as Text
import qualified Text.Regex as Regex

data Composition = Composition {
  baseFile     :: FilePath
, replacements :: [(Text, Composition)]
} deriving (Show, Read, Eq)

defaultComposition = Composition {
    baseFile = "files/html/index.html"
  , replacements = [
      ("[[head]]", Composition {
        baseFile = "files/html/head.html"
      , replacements = []})
    , ("[[menu]]", Composition {
        baseFile = "files/html/menu.html"
      , replacements = []})
    , ("[[ArticleView]]", Composition {
        baseFile = "files/html/ArticleView.html"
      , replacements = []})
    , ("[[SingleArticleView]]", Composition {
        baseFile = "files/html/SingleArticleView.html"
      , replacements = []})
    , ("[[CreateArticleView]]", Composition {
        baseFile = "files/html/CreateArticleView.html"
      , replacements = []})
    , ("[[DiscussionView]]", Composition {
        baseFile = "files/html/DiscussionView.html"
      , replacements = []})
    , ("[[SingleDiscussionView]]", Composition {
        baseFile = "files/html/SingleDiscussionView.html"
      , replacements = [
          ("[[SingleDiscussionViewTabs]]", Composition {
            baseFile = "files/html/SingleDiscussionViewTabs.html"
          , replacements = []})
        , ("[[SingleDiscussionViewArticles]]", Composition {
            baseFile = "files/html/SingleDiscussionViewArticles.html"
          , replacements = []})
        , ("[[SingleDiscussionViewCollected]]", Composition {
            baseFile = "files/html/SingleDiscussionViewCollected.html"
          , replacements = []})
        , ("[[SingleDiscussionViewGraph]]", Composition {
            baseFile = "files/html/SingleDiscussionViewGraph.html"
          , replacements = []})
        , ("[[SingleDiscussionViewResults]]", Composition {
            baseFile = "files/html/SingleDiscussionViewResults.html"
          , replacements = []})
        , ("[[SingleDiscussionViewParticipants]]", Composition {
            baseFile = "files/html/SingleDiscussionViewParticipants.html"
          , replacements = []})
        , ("[[SingleDiscussionViewFiles]]", Composition {
            baseFile = "files/html/SingleDiscussionViewFiles.html"
          , replacements = []})
        , ("[[RelationCreationModal]]", Composition {
            baseFile = "files/html/RelationCreationModal.html"
          , replacements = []})
        , ("[[ArticleConditionModal]]", Composition {
            baseFile = "files/html/ArticleConditionModal.html"
          , replacements = []})
        ]})
    , ("[[CreateDiscussionView]]", Composition {
        baseFile = "files/html/CreateDiscussionView.html"
      , replacements = []})
    , ("[[ResultView]]", Composition {
        baseFile = "files/html/ResultView.html"
      , replacements = []})
    , ("[[SingleResultView]]", Composition {
        baseFile = "files/html/SingleResultView.html"
      , replacements = [
          ("[[SingleResultViewTabs]]", Composition {
            baseFile = "files/html/SingleResultViewTabs.html"
          , replacements = []})
        , ("[[SingleResultViewGraph]]", Composition {
            baseFile = "files/html/SingleResultViewGraph.html"
          , replacements = []})
        , ("[[SingleResultViewVote]]", Composition {
            baseFile = "files/html/SingleResultViewVote.html"
          , replacements = []})
        , ("[[SingleResultViewVoters]]", Composition {
            baseFile = "files/html/SingleResultViewVoters.html"
          , replacements = []})
      ]})
    , ("[[UserView]]", Composition {
        baseFile = "files/html/UserView.html"
      , replacements = []})
    , ("[[SingleUserView]]", Composition {
        baseFile = "files/html/SingleUserView.html"
      , replacements = []})
    , ("[[LoginRegisterForm]]", Composition {
        baseFile = "files/html/LoginRegisterForm.html"
      , replacements = []})
    , ("[[scripts]]", Composition {
        baseFile = "files/html/scripts.html"
      , replacements = []})
    ]
  }
