module OpenBrain.Website.Routes where

import OpenBrain.Website.Common
import qualified OpenBrain.Website.Article           as Article
import qualified OpenBrain.Website.CollectionArticle as CArticle
import qualified OpenBrain.Website.Collection        as Collection
import qualified OpenBrain.Website.Description       as Description
import qualified OpenBrain.Website.Discussion        as Discussion
import qualified OpenBrain.Website.Files             as Files
import qualified OpenBrain.Website.Login             as Login
import qualified OpenBrain.Website.Relation          as Relation
import qualified OpenBrain.Website.Result            as Result
import qualified OpenBrain.Website.User              as User

route :: OBW Response
route = msum [
    dir "article" $ msum [
      path $ \aid -> msum [
        dir "replace" . path $ \d -> crudUpdate $ Article.replaceDummy d aid
      , dir "collection" . path $ \cid -> crudUpdate $ msum [
          dir "position"  $ CArticle.updatePosition  cid aid
        , dir "condition" $ CArticle.updateCondition cid aid
        ]
      , crudRead   $ Article.readArticle   aid
      , crudUpdate $ Article.updateArticle aid
      , crudDelete $ Article.deleteArticle aid
      ]
    , crudRead   Article.pageArticles
    , crudCreate Article.createArticle
    ]
  , dir "collection" $ msum [
      path $ \cid -> path $ \aid -> msum [
        crudUpdate $ Collection.addToCollection cid aid
      , crudDelete $ Collection.delFromCollection cid aid
      ]
    , path $ crudRead . Collection.readCollection
    , crudRead Collection.pageCollections
    ]
  , dir "description" $ msum [
      path $ \did -> msum [
        crudRead   $ Description.readDescription   did
      , crudDelete $ Description.deleteDescription did
      ]
    , crudRead Description.pageDescriptions
    ]
  , dir "discussion" $ msum [
      path $ \did -> msum [
        dir "uploadinstance" . crudCreate $ Discussion.uploadInstance did
      , dir "participate" $ msum [
          crudCreate $ Discussion.joinDiscussion did
        , crudDelete $ Discussion.leaveDiscussion did
        ]
      , dir "acs" . crudRead $ Discussion.acs did
      , dir "evaluate" . crudRead $ Discussion.evaluate did
      , crudRead $ Discussion.readDiscussion did
      ]
    , crudCreate Discussion.createDiscussion
    , crudRead   Discussion.pageDiscussions
    ]
  , dir "files" Files.serve
  , dir "login" $ msum [
      crudCreate Login.login
    , crudDelete Login.logout
    ]
  , dir "relation" $ msum [
      path $ crudRead . Relation.readRelation
    , crudRead   Relation.pageRelations
    , crudCreate Relation.createRelation
    ]
  , dir "result" $ msum [
      path $ \rid -> msum [
        dir "vote" . crudCreate $ Result.vote rid
      , crudRead $ Result.readResult rid
      ]
    , crudRead Result.pageResults
    ]
  , dir "user" $ msum [
      path $ \uid -> msum [
        crudRead   $ User.readUser   uid
      , crudUpdate $ User.updateUser uid
      , crudDelete $ User.deleteUser uid
      ]
    , crudRead   User.pageUsers
    , crudCreate User.createUser
    ]
  , liftM responseHTML $ serveFile return "files/index.html"
  ]
