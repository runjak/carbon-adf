module OpenBrain.Backend.Logic where

import Control.Arrow ((&&&), second)
import Control.Monad
import Data.List ((\\), nub)
import Data.Map (Map)
import qualified Data.Map   as Map
import qualified Data.Maybe as Maybe

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
        condition = null attackers ? (Const True, Neg . or' $ map idToExp attackers)
    UpdateCondition (collectionId $ dCollection d) aid False condition

{-|
  Generates the content of a diamond input file from a DiscussionId.
|-}
type RenameIds = Bool
diamondInput :: RenameIds -> DiscussionId -> BackendDSL String
diamondInput rIds did = do
  as <- liftM (articles . dCollection) $ GetDiscussion did
  let caToIdName = show . unwrap . toId . articleId . cArticle
      caToHeName = headline . aDescription . cArticle
      renameMap  = Map.fromList $ map (caToIdName &&& caToHeName) as
      hAndMConds = map (caToIdName &&& condition) as
      acs        = map (uncurry AC) hAndMConds
      acs'       = rename renameMap acs
  return . show . instanceFromAcs $ rIds ? (acs', acs)

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

  Notice that this functions can be optimized by using Data.Set instead of []
  … or even a Set from the unordered containers package

  FIXME technically it would be correct to remove Relations from ac's that will be changed,
  where the relations source is not mentioned in the EXP.
|-}
fitInstance :: UserId -> DiscussionId -> Instance -> BackendDSL ()
fitInstance uid did i = do
  -- Add missing Nodes:
  d <- GetDiscussion did
  let headlines = map (headline . aDescription . cArticle) . articles $ dCollection d
      nodes     = names i            :: [String]
      newNodes  = nodes \\ headlines :: [String]
  CollectArticles (collectionId $ dCollection d) =<< mapM (newDummyA uid) newNodes
  -- Add mising Relations:
  d <- GetDiscussion did -- We need the new Id's
  let fetchAs  = map cArticle . articles . dCollection            :: Discussion -> [Article]
      hIdPair  = (headline . aDescription) &&& articleId          :: Article -> (Headline, ArticleId)
      wanted   = filter $ flip elem nodes . fst                   :: [(Headline, ArticleId)] -> [(Headline, ArticleId)]
      hToIdMap = Map.fromList . wanted . map hIdPair $ fetchAs d  :: Map Headline ArticleId
      curRels  = map shrink $ relations d                         :: [(ArticleId, ArticleId)]
      posRels  = concatMap (possibleRels hToIdMap) $ conditions i :: [(ArticleId, ArticleId)]
      newRels  = nub $ posRels \\ curRels                         :: [(ArticleId, ArticleId)]
  mapM_ (newRel uid did) newRels
  -- Removing old Relations:
  let oldRels' = filter (flip elem (Map.elems hToIdMap) . snd) curRels :: [(ArticleId, ArticleId)]
      oldRels  = oldRels' \\ posRels                                   :: [(ArticleId, ArticleId)]
      remRids  = map relationId . filter (flip elem oldRels . shrink) $ relations d
  mapM_ RemoveRelation remRids
  -- Saving the Conditions:
  let updates  = Maybe.mapMaybe (acToAidExp hToIdMap) $ conditions i
      cid      = collectionId $ dCollection d
  forM_ updates $ \(aid, exp) -> UpdateCondition cid aid True exp
  where
    newDummyA :: UserId -> Headline -> BackendDSL ArticleId
    newDummyA uid h = AddArticle `flip` "" =<< AddDescription uid h ""

    shrink :: Relation -> (ArticleId, ArticleId)
    shrink r = (source r, target r)

    possibleRels :: Map Headline ArticleId -> ACondition -> [(ArticleId, ArticleId)]
    possibleRels m (AC n e) = case Map.lookup n m of
      (Just t) -> map (\s -> (s,t)) . lookupMany m $ names e
      Nothing  -> []

    lookupMany :: Ord k => Map k a -> [k] -> [a]
    lookupMany m = Maybe.mapMaybe $ Map.lookup `flip` m

    newRel :: UserId -> DiscussionId -> (ArticleId, ArticleId) -> BackendDSL RelationId
    newRel uid did st = do
      desc <- AddDescription uid "Autogenerated Relation" "The system has generated this Relation automatically."
      uncurry (AddRelation did desc) st

    acToAidExp :: Map String ArticleId -> ACondition -> Maybe (ArticleId, Exp)
    acToAidExp m (AC n e) =
      let mAid = Map.lookup n m
      in maybe Nothing (\aid -> Just (aid, e)) mAid
