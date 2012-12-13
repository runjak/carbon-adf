module OpenBrain.Argumentation.OBB where
{-|
  This module works with the Argumentation to do the following things:
  - Fetch Relations from the Backend
  - Build an AF from these Relations
  - Compute 'interesting' Sets of Arguments
  - Store these sets back to the Backend.
|-}
import Control.Monad
import Data.List (nub)
import Data.Maybe
import qualified Data.Set as Set

import OpenBrain.Argumentation.Semantics as Semantics
import OpenBrain.Backend.Monad (OBB)
import OpenBrain.Backend.Types
import OpenBrain.Data.Id
import OpenBrain.Data.Information
import OpenBrain.Data.Relation as R
import qualified OpenBrain.Argumentation.AttackOnly as AttackOnly
import qualified OpenBrain.Argumentation.Compute    as Compute
import qualified OpenBrain.Backend.Monad            as OBB

{-|
  Given InformationId should belong to a discussion.
  If not, something will fail.
|-}
compute :: InformationId -> OBB ()
compute iid = do
  -- | Checking if iid is a discussion:
  i <- OBB.getInformation iid
  cType <- case media i of -- | the CollectionType describes what kind of discussion this is.
    (Content _) -> failNoDiscussion iid
    media       -> do
      when (isNothing $ discussion media) $ failNoDiscussion iid
      return $ collectionType media
  -- | Fetching the arguments in iid:
  args <- liftM (Set.fromList . map target) $
          OBB.getRelations iid RelationSource (Just R.Collection) False
  -- | Fetching the relations between the arguments:
  let targetInArgs = flip Set.member args . target                     :: R.Relation -> Bool
      okType       = not . flip elem [Parent, R.Collection] . relation :: R.Relation -> Bool
      filterRels   = filter targetInArgs . filter okType               :: [R.Relation] -> [R.Relation]
  rels <- liftM (filterRels . nub . concat) .
          mapM (\i -> OBB.getRelations i RelationSource Nothing False) $
          Set.toList args
  -- | Getting the computation depending on the cType:
  let computation = 
        case cType of
          DiscussionAttackOnly -> AttackOnly.wantedSets
          _ -> error $ "Unexpected cType: " ++ show cType
               ++ " in OpenBrain.Argumentation.OBB:compute"
  -- | Computing the candidate sets that users can vote for:
  let candidates = Compute.compute rels computation
  -- | Storing the computed candidates:
  OBB.setChoices iid candidates
  where
    -- | Error if iid doesn't belong to a discussion:
    failNoDiscussion :: InformationId -> a
    failNoDiscussion iid = error $
         "Given iid doesn't belong to a discussion: "
      ++ show iid
      ++ " in OpenBrain.Argumentation.OBB:compute"

