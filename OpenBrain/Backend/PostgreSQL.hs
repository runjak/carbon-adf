{-# LANGUAGE GADTs #-}
module OpenBrain.Backend.PostgreSQL (load, validConfig) where

import Control.Monad
import Data.Maybe
import Database.HDBC.PostgreSQL as PSQL
import qualified Database.HDBC  as HDBC

import OpenBrain.Backend.DSL as DSL
import OpenBrain.Backend.PostgreSQL.Common (Connector, Executor, exec)
import OpenBrain.Config      as Config
import qualified OpenBrain.Backend.PostgreSQL.Article           as Article
import qualified OpenBrain.Backend.PostgreSQL.Collection        as Collection
import qualified OpenBrain.Backend.PostgreSQL.CollectionArticle as CArticle
import qualified OpenBrain.Backend.PostgreSQL.Common            as Common
import qualified OpenBrain.Backend.PostgreSQL.Description       as Description
import qualified OpenBrain.Backend.PostgreSQL.Discussion        as Discussion
import qualified OpenBrain.Backend.PostgreSQL.Paging            as Paging
import qualified OpenBrain.Backend.PostgreSQL.Relation          as Relation
import qualified OpenBrain.Backend.PostgreSQL.Result            as Result
import qualified OpenBrain.Backend.PostgreSQL.User              as User

load :: Config -> IO CBackendProcessor
load = liftM CBackendProcessor . Common.load

validConfig :: Config -> Bool
validConfig c = case backendType c of
                  (PostgreSQLBackend _) -> True
                  _ -> False

instance BackendProcessor Connector where
  process c e = flip process e =<< Common.connect c

instance BackendProcessor Executor where
  process e (Backendλ m ma) = process e . ma =<< process e m
  process e (Nop         r) = return r
  -- | User related:
  process e (AddUser uname hs isA) = exec e $ User.addUser uname hs isA
  process e (DeleteUser  uid heir) = exec e $ User.deleteUser uid heir
  process e  GetNobody             = exec e User.getNobody
  process e (GetUser          uid) = exec e $ User.getUser uid
  process e (HasUser        uname) = exec e $ User.hasUser uname
  process e (Login          uid f) = exec e $ User.login uid f
  process e (Validate    uid skey) = exec e $ User.validate uid skey
  process e (Logout           uid) = exec e $ User.logout uid
  process e (SetAdmin     uid isA) = exec e $ User.setAdmin uid isA
  process e (SetPasswd      uid f) = exec e $ User.setPasswd uid f
  process e (SetProfile  uid mAid) = exec e $ User.setProfile uid mAid
  -- | Description related:
  process e (AddDescription author headline content) = exec e $ Description.addDescription author headline content
  process e (DeleteDescription                  did) = exec e $ Description.deleteDescription did
  process e (GetDescription                     did) = exec e $ Description.getDescription did
  process e (SetHeadline               did headline) = exec e $ Description.setHeadline did headline
  process e (SetDescription                did desc) = exec e $ Description.setDescription did desc
  -- | Article related:
  process e (AddArticle ndid content) = exec e $ Article.addArticle ndid content
  process e (Clone           aid uid) = exec e $ Article.clone aid uid
  process e (GetArticle          aid) = exec e $ Article.getArticle aid
  process e (SetContent  aid content) = exec e $ Article.setContent aid content
  process e (ReplaceDummy  dummy aid) = exec e $ Article.replaceDummy dummy aid
  -- | Relation related:
  process e (AddRelation did ndid source target) = exec e $ Relation.addRelation did ndid source target
  process e (GetRelation                    rid) = exec e $ Relation.getRelation rid
  process e (RelationDiscussion             rid) = exec e $ Relation.relationDiscussion rid
  process e (RemoveRelation                 rid) = exec e $ Relation.removeRelation rid
  -- | Collection related:                         
  process e (AddCollection  ndid as) = exec e $ Collection.addCollection ndid as
  process e (CollectArticles cid as) = exec e $ Collection.collectArticles cid as
  process e (ForgetArticles  cid as) = exec e $ Collection.forgetArticles cid as
  process e (GetCollection      cid) = exec e $ Collection.getCollection cid
  process e (DiscussionIds      cid) = exec e $ Collection.discussionIds cid
  -- | CollectionArticle related:
  process e (UpdatePosition  cid aid xy       ) = exec e $ CArticle.updatePosition cid aid xy
  process e (UpdateAccepted  cid aid mac      ) = exec e $ CArticle.updateAccepted cid aid mac
  process e (UpdateCondition cid aid cust cond) = exec e $ CArticle.updateCondition cid aid cust cond
  -- | Discussion related:
  process e (AddDiscussion ncid uids deadline) = exec e $ Discussion.addDiscussion ncid uids deadline
  process e (GetDiscussion                did) = exec e $ Discussion.getDiscussion did
  process e (SetParticipant         did uid p) = exec e $ Discussion.setParticipant did uid p
  -- | Result related:
  process e (AddResult did rType cids) = exec e $ Result.addResult did rType cids
  process e (GetResults           did) = exec e $ Result.getResults did
  process e (Vote             rid uid) = exec e $ Result.vote rid uid
  process e (RemoveResults        did) = exec e $ Result.removeResults did
  process e (DisForResult         rid) = exec e $ Result.disForResult rid
  -- | Paging: = exec e $ undefined
  process e  ArticleCount          = exec e Paging.articleCount
  process e  CollectionCount       = exec e Paging.collectionCount
  process e  DescriptionCount      = exec e Paging.descriptionCount
  process e  DiscussionCount       = exec e Paging.discussionCount
  process e  RelationCount         = exec e Paging.relationCount
  process e  ResultCount           = exec e Paging.resultCount
  process e  UserCount             = exec e Paging.userCount
  process e (PageArticles     l o) = exec e $ Paging.pageArticles     l o
  process e (PageCollections  l o) = exec e $ Paging.pageCollections  l o
  process e (PageDescriptions l o) = exec e $ Paging.pageDescriptions l o
  process e (PageDiscussions  l o) = exec e $ Paging.pageDiscussions  l o
  process e (PageRelations    l o) = exec e $ Paging.pageRelations    l o
  process e (PageResults      l o) = exec e $ Paging.pageResults      l o
  process e (PageUsers        l o) = exec e $ Paging.pageUsers        l o
  -- | Logging:
  process e (LogString s) = putStrLn $ "BackendDSL.Logstring:\t"++s
