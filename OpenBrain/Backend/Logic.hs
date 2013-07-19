module OpenBrain.Backend.Logic where

import Control.Arrow ((&&&), second)
import Control.Monad
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
        condition = Neg . or' $ map idToExp attackers
    UpdateCondition (collectionId $ dCollection d) aid False condition

{-|
  Generates the content of a diamond input file from a DiscussionId.
|-}
type RenameIds = Bool
diamondInput :: RenameIds -> DiscussionId -> BackendDSL String
diamondInput rename did = do
  as <- liftM (articles . dCollection) $ GetDiscussion did
  let caToIdName = show . unwrap . toId . articleId . cArticle
      caToHeName = headline . aDescription . cArticle
      renameMap  = Map.fromList $ map (caToIdName &&& caToHeName) as
      hAndMConds = filter (Maybe.isJust . snd) $ map (caToIdName &&& condition) as
      acs        = map (uncurry AC . second Maybe.fromJust) hAndMConds
      acs'       = map (renameAc renameMap) acs
  return . unlines . map show $ rename ? (acs', acs)
