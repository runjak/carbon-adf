module OpenBrain.Backend.Logic where

import Control.Arrow ((&&&), second)
import Control.Monad
import Data.Map (Map, (!))
import Data.Set (Set, (\\))
import qualified Data.Map   as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set   as Set

import OpenBrain.Backend.DSL
import OpenBrain.Common
import OpenBrain.Data
import OpenBrain.Data.Id
import OpenBrain.Data.Logic as Logic

{-|
  Generates the  for a CollectionArticle automatically
  as long as it's not a custom one.
  This will be executed whenever a Relation is added or removed.
|-}
autoCondition :: DiscussionId -> ArticleId -> BackendDSL ()
autoCondition did aid = do
  d <- GetDiscussion did
  let ca = head . filter ((aid ==) . articleId . cArticle) . articles $ dCollection d
  unless (customcondition ca) $ do
    let attackers = map source . filter ((aid ==) . target) $ relations d
        condition = null attackers ? (Const True, Neg . or' $ map Var attackers)
    UpdateCondition (collectionId $ dCollection d) aid False condition

{-|
  Generates the content of a diamond input file from a DiscussionId.
|-}
type RenameIds = Bool
diamondInput :: RenameIds -> DiscussionId -> BackendDSL String
diamondInput rIds did = do
  as <- liftM (articles . dCollection) $ GetDiscussion did
  let caToIdName = articleId . cArticle                              :: CollectionArticle -> ArticleId
      caToHeName = headline . aDescription . cArticle                :: CollectionArticle -> Headline
      renameMap  = Map.fromList $ map (caToIdName &&& caToHeName) as :: Map ArticleId Headline
      hAndMConds = map (caToIdName &&& condition) as                 :: [(ArticleId, Exp ArticleId)]
      idsToStrgs = map (fmap $ show . unwrap . toId)                 :: [ACondition ArticleId] -> [ACondition String]
      acs        = map (uncurry AC) hAndMConds                       :: [ACondition ArticleId]
      acs'       = map (fmap (renameMap!)) acs                       :: [ACondition String]
  return . show . instanceFromAcs $ rIds ? (acs', idsToStrgs acs)

{-|
  Saves the parsed output as Results String for a DiscussionId.
  As a first step it also removes older results if existent
|-}
saveResults :: DiscussionId -> Results String -> BackendDSL ()
saveResults did rs  =
  let (Results rs') = fmap (fromId . wrap . read) rs
  in RemoveResults did >> mapM_ (go did) rs'
  where
    go :: DiscussionId -> (ResultType, [DiamondResult ArticleId]) -> BackendDSL ()
    go did (rType, dResults) = mapM_ (AddResult did rType . mkRArticles) dResults

    mkRArticles :: DiamondResult ArticleId -> [(ResultState, ArticleId)]
    mkRArticles dResult =
      let ins   = map ((,) In)   $ inSet   dResult
          udecs = map ((,) Udec) $ udecSet dResult
          outs  = map ((,) Out)  $ outSet  dResult
      in concat [ins, udecs, outs]

{-|
  Fit's a DiscussionId to an Instance.
  This means, that all necessary Articles will be autocreated
  with the given UserId as author, if they don't already exist.
  Afterwards, all Articles will get the supplied acceptance conditions
  if they have any.
  The acceptance conditions will also cause relations to be created where necessary.
  Notice that not listed Articles won't be changed, but will be kept.
  This way the given Instance is included in the Discussion,
  but possible additional stuff aswell.
|-}
fitInstance :: UserId -> DiscussionId -> Instance Headline -> BackendDSL ()
fitInstance uid did i = do
  d        <- addMissingNodes uid did i
  hToIdMap <- mapHeadlineArticleId  d i
  let i'      = fmap (hToIdMap !) i                                   :: Instance ArticleId
      posRels = Set.fromList . concatMap possibleRels $ conditions i' :: Set (ArticleId, ArticleId)
  addMissingRelations posRels hToIdMap d i' uid 
  removeOldRelations  posRels hToIdMap i' d
  saveConditions d i'
  where
    addMissingNodes :: UserId -> DiscussionId -> Instance Headline-> BackendDSL Discussion
    addMissingNodes uid did i = do
      d <- GetDiscussion did
      let headlines = map (headline . aDescription . cArticle) . articles $ dCollection d
          nodes     = names i                         :: Set String
          newNodes  = nodes \\ Set.fromList headlines :: Set String
      CollectArticles (collectionId $ dCollection d) =<< mapM (newDummyA uid) (Set.toList newNodes)
      GetDiscussion did
      where
       newDummyA :: UserId -> Headline -> BackendDSL ArticleId
       newDummyA uid h = (`AddArticle` "") =<< AddDescription uid h ""

    mapHeadlineArticleId :: Discussion -> Instance Headline -> BackendDSL (Map Headline ArticleId)
    mapHeadlineArticleId d i = do
      let nodes   = names i                                 :: Set Headline
          fetchAs = map cArticle . articles . dCollection   :: Discussion -> [Article]
          wanted  = filter $ (`Set.member` nodes) . fst     :: [(Headline, ArticleId)] -> [(Headline, ArticleId)]
          hIdPair = (headline . aDescription) &&& articleId :: Article -> (Headline, ArticleId)
      return . Map.fromList . wanted . map hIdPair $ fetchAs d

    addMissingRelations :: Set (ArticleId, ArticleId) -> Map Headline ArticleId
                        -> Discussion -> Instance ArticleId -> UserId -> BackendDSL ()
    addMissingRelations posRels hToIdMap d i uid = do
      let curRels  = Set.fromList . map shrink $ relations d :: Set (ArticleId, ArticleId)
          newRels  = posRels \\ curRels                      :: Set (ArticleId, ArticleId)
      mapM_ (newRel uid did) $ Set.toList newRels
      where
        newRel :: UserId -> DiscussionId -> (ArticleId, ArticleId) -> BackendDSL RelationId
        newRel uid did (s, t) = do
          desc <- AddDescription uid "Autogenerated Relation"
            "The system has generated this Relation automatically."
          AddRelation did desc s t

    removeOldRelations :: Set (ArticleId, ArticleId) -> Map Headline ArticleId
                       -> Instance ArticleId -> Discussion -> BackendDSL ()
    removeOldRelations posRels hToIdMap i d = do
      let targets  = map snd $ Set.toList posRels :: [ArticleId] -- Changed Articles
          oldRels' = filter (flip elem targets . target) $ relations d
          oldRels  = filter (flip Set.notMember posRels . shrink) oldRels'
      mapM_ (RemoveRelation . relationId) oldRels

    saveConditions :: Discussion -> Instance ArticleId -> BackendDSL ()
    saveConditions d i =
      let updates = map (aHead &&& aCondition) $ conditions i :: [(ArticleId, Exp ArticleId)]
          cid     = collectionId $ dCollection d              :: CollectionId
      in forM_ updates $ \(aid, exp) -> UpdateCondition cid aid True exp

    shrink :: Relation -> (ArticleId, ArticleId)
    shrink r = (source r, target r)

    possibleRels :: ACondition ArticleId -> [(ArticleId, ArticleId)]
    possibleRels ac = map ((,) $ aHead ac) . vars $ aCondition ac

{-|
  Removes a Relation, and updates the targets Exp,
  so that it no longer includes the source.
  On custom Conditions this is done via
  OpenBrain.Data.Logic.Exp:removeVar.
  Otherwise autoCondition is used.
|-}
removeRelation :: RelationId -> BackendDSL ()
removeRelation rid = do
  r <- GetRelation rid
  d <- GetDiscussion =<< RelationDiscussion rid
  RemoveRelation rid
  let cas  = articles $ dCollection d
      caId = articleId . cArticle
      s    = head $ filter ((==) (source r) . caId) cas
      t    = head $ filter ((==) (target r) . caId) cas
      cid  = collectionId $ dCollection d
      aid  = caId t
      cust = customcondition t
      var  = caId s
      exp  = condition t
      exp' = removeVar var exp
  LogString $ unlines [
      "Changing exp:"
    , "from " ++ show (fmap (show . unwrap . toId) exp)
    , "to "   ++ show (fmap (show . unwrap . toId) exp')
    , "by removing var: " ++ (show . unwrap $ toId var)
    ]
  UpdateCondition cid aid cust exp'
