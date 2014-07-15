{-# LANGUAGE RankNTypes #-}
module Carbon.Backend.PostgreSQL.Item where

import Data.Monoid (Monoid(..))
import qualified Data.Either as Either
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import Carbon.Backend.PostgreSQL.Common hiding (clone)
import Carbon.Backend.PostgreSQL.Conversion
import qualified Carbon.Backend.PostgreSQL.AcceptanceCondition as AcceptanceCondition
import qualified Carbon.Backend.PostgreSQL.Article as Article
import qualified Carbon.Backend.PostgreSQL.Description as Description
import qualified Carbon.Backend.PostgreSQL.Discussion as Discussion
import qualified Carbon.Backend.PostgreSQL.Relation as Relation
import qualified Carbon.Backend.PostgreSQL.ResultSet as ResultSet
import qualified Carbon.Backend.PostgreSQL.User as User

addItem :: UserId -> Query (Item Id)
addItem uid conn = do
  i <- addItem' uid conn
  let problem = chkProblem $ "Could not fetch just created item with id " ++ show i
  problem =<< getItem' i conn

addItem' :: UserId -> Query SqlValue
addItem' uid conn = do
  let q = "INSERT INTO items (commitmessage, commitauthor) VALUES ('Creation of this item', ?) RETURNING itemid"
  [[i]] <- quickQuery' conn q [toSql uid]
  return i

clone :: ItemId -> Query (Either Error (Item Id))
clone iid conn = do
  iid' <- clone' (toSql iid) conn
  getItem' iid' conn

{-|
  Due to the internals of cloning,
  there must never be a loop between levels of Relations,
  or otherwise this function will not terminate.
  To prevent this, no Item that has a Relation associated
  shall ever be able to be the target or source of a relation itself.
|-}
clone' :: SqlValue -> Query SqlValue
clone' iid conn = do -- | Fixme think about discussions and mark original as deleted
  -- Fetching original item
  let get = "SELECT descriptionid, articleid, acceptanceconditionid, "
          ++"relationid, discussionid, resultsetid, commitmessage, commitauthor "
          ++"FROM items WHERE itemid = ?"
  [[did, aid, acid, rid, disid, rsid, cm, ca]] <- quickQuery' conn get [iid]
  -- Inserting clone
  let new = "INSERT INTO items (descriptionid, articleid, acceptanceconditionid, "
          ++"relationid, discussionid, resultsetid, commitmessage, commitauthor) "
          ++"VALUES (?, ?, ?, ?, ?, ?, ?, ?) RETURNING itemid"
  [[clone]] <- quickQuery' conn new [did, aid, acid, rid, disid, rsid, cm, ca]
  -- Taking care of the family
  let parent = "INSERT INTO item_family (parent, child) VALUES (?, ?)"
      family = "INSERT INTO item_family (parent, child) SELECT parent, ? FROM item_family where child = ?"
  quickQuery' conn parent [iid, clone]
  quickQuery' conn family [clone, iid]
  -- Taking care of relations with source or target as iid'
  cloneRelations iid clone conn
  -- Marking original as deleted
  let q = "UPDATE items SET deletion = now() WHERE itemid = ?"
  quickQuery' conn q [iid]
  -- Done
  return clone
  where
    cloneRelations :: SqlValue -> SqlValue -> Query ()
    cloneRelations orig clone conn = do
      let wasSource = "SELECT relationid FROM relations WHERE source = ?"
          wasTarget = "SELECT relationid FROM relations WHERE target = ?"
          setSource = "UPDATE relations SET source = ? WHERE relationid = ?"
          setTarget = "UPDATE relations SET target = ? WHERE relationid = ?"
      oldSources <- liftM (map head) $ quickQuery' conn wasSource [orig]
      oldTargets <- liftM (map head) $ quickQuery' conn wasTarget [orig]
      newSources <- mapM (`cloneRelation` conn) oldSources
      newTargets <- mapM (`cloneRelation` conn) oldTargets
      forM_ newSources $ \r -> quickQuery' conn setSource [clone, r]
      forM_ newTargets $ \r -> quickQuery' conn setTarget [clone, r]
    
    cloneRelation :: SqlValue -> Query SqlValue
    cloneRelation rId conn = do
      -- Fetch and clone related item:
      let getI = "SELECT itemid FROM items WHERE relationid = ?"
      [[iid]] <- quickQuery' conn getI [rId]
      iid' <- clone' iid conn
      -- Clone the relation:
      let getR = "SELECT source, target, relationtype FROM relations WHERE relationid = ?"
      [[s, t, rtype]] <- quickQuery' conn getR [rId]
      let addR = "INSERT INTO relations (source, target, relationtype) VALUES (?, ?, ?) RETURNING relationid"
      [[clone]] <- quickQuery' conn addR [s, t, rtype]
      -- Set the relation in the item:
      let setR = "UPDATE items SET relationid = ? WHERE itemid = ?"
      quickQuery' conn setR [clone, iid']
      -- Done
      return clone

getItem :: ItemId -> Query (Either Error (Item Id))
getItem i = getItem' $ toSql i

getItem' :: SqlValue -> Query (Either Error (Item Id))
getItem' i conn = do
  -- Fetching the basic item entry
  let getI = "SELECT descriptionid, articleid, acceptanceconditionid, "
           ++"relationid, discussionid, resultsetid, creation, deletion, "
           ++"commitmessage, commitauthor FROM items WHERE itemid = ?"
  row <- quickQuery' conn getI [i]
  case row of
    [[did, aid, acid, rid, disid, rsid, creation, deletion, cm, cr]] -> do
      description <- Description.getDescription did conn
      article     <- Article.getArticle aid conn
      condition   <- AcceptanceCondition.getCondition acid conn
      relation    <- Relation.getRelation rid conn
      relations   <- getRelations i conn
      discussion  <- Discussion.getDiscussion disid conn
      resultSet   <- ResultSet.getResultSet rsid conn
      (ps, cs)    <- getParentsAndChildren i conn
      return . Right $ Item {
        itemId        = fromSql i
      , description   = description
      , article       = article
      , condition     = condition
      , relation      = relation
      , relations     = relations
      , discussion    = discussion
      , resultSet     = resultSet
      , creation      = fromSql creation
      , deletion      = fromSql deletion
      , parents       = ps
      , children      = cs
      , commitMessage = fromSql cm
      , commitAuthor  = fromSql cr
      }
    _ -> return . Left $ "No entry found for item" ++ show i
  where
    getRelations :: SqlValue -> Query [Item Id]
    getRelations i conn = do
      let getR = "SELECT i.itemid FROM items as i JOIN relations as r USING (relationid) "
               ++"WHERE r.source = ? OR r.target = ?"
      iids <- liftM (map head) $ quickQuery' conn getR [i, i]
      is <- forM iids $ getItem' `flip` conn
      case Either.partitionEithers is of
        ([], is) -> return is
        (fails, _) -> fail $ unlines fails

    getParentsAndChildren :: SqlValue -> Query ([Id],[Id])
    getParentsAndChildren i conn = do
      let getPs = "SELECT parent FROM item_family WHERE child = ? ORDER BY parent DESC"
          getCs = "SELECT child FROM item_family WHERE parent = ? ORDER BY child DESC"
          mkIds = map $ fromSql . head
      ps <- quickQuery' conn getPs [i]
      cs <- quickQuery' conn getCs [i]
      return (mkIds ps, mkIds cs)

idFromHeadline :: String -> Query (Maybe ItemId)
idFromHeadline h conn = do
  let q = "SELECT i.itemid FROM items as i JOIN descriptions as d USING (descriptionid) WHERE d.headline = ?"
  rst <- quickQuery' conn q [toSql h]
  case rst of
    [[i]] -> return . Just $ fromSql i
    _ -> return Nothing

setItem :: Item Id -> Query (Either Error (Item Id))
setItem i conn = do
  let actions = [ setBasic
                , Description.setDescription
                , Article.setArticle
                , AcceptanceCondition.setCondition
                , Relation.setRelation
                , Discussion.setDiscussion setItem
                , ResultSet.setResultSet
                ]
      perform (Right i) a = a conn i
      perform l _         = return l
  foldM perform (Right i) actions
  where
    -- Makes sure we've got an Id, and that the basic attributes of Item are stored.
    setBasic :: IConnection conn => conn -> Item Id -> IO (Either Error (Item Id))
    setBasic conn i
      | mempty == itemId i = do -- Need to add an Id.
        let cAuth = commitAuthor i
        uid <- (cAuth == mempty) ? (User.getNobody conn, return cAuth)
        i' <- addItem uid conn
        setBasic conn $ i' `mappend` i
      | otherwise = do -- Got an Id, saving other fields.
        eI' <- clone (itemId i) conn
        case eI' of
          (Right i') -> do
            let qDel = "UPDATE items SET commitmessage = ?, commitauthor = ?, deletion = now() WHERE itemid = ?"
                qSim = "UPDATE items SET commitmessage = ?, commitauthor = ? WHERE itemid = ?"
                q = Maybe.isJust (deletion i) ? (qDel, qSim)
            quickQuery' conn q [toSql $ commitMessage i, toSql $ commitAuthor i, toSql $ itemId i']
            return . Right $ i <+ itemId i'
          _ -> return eI'

deleteItem :: ItemId -> Query (Maybe Error)
deleteItem iid conn = do
  let q = "UPDATE items SET deletion = now() WHERE itemid = ?"
  [[c]] <- quickQuery' conn q [toSql iid]
  chkChanges $ fromSql c
  where
    chkChanges :: Int -> IO (Maybe Error)
    chkChanges 1 = return Nothing
    chkChanges c = return . Just $ unwords [
        "Had"
      , show c
      , "changes when marking"
      , show iid
      , "as deleted!"
      ]
