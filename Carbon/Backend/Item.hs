module Carbon.Backend.Item where
{-|
  This module aims to provide means of conversion
  for (Item String -> Item Id), and, possibly,
  also the other way round.
|-}
import Control.Monad
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import qualified Data.Convertible as Convertible
import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import Carbon.Backend.DSL as DSL
import Carbon.Data as Data

{-|
  We ensure to have Ids for all names in the condition.
  Thereby we get a map to convert (AcceptanceCondition String -> AcceptanceCondition Id).
  The other part is the discussion, where we just make sure to have
  the arguments as (Set Id) rather than (Set (Item String)).
  Therefore we map Discussions from their Items to their Ids,
  thereby ommiting the problem of recursion in this case.
|-}
hToId :: Item String -> BackendDSL (Item Id)
hToId i
  | Maybe.isNothing (condition i) = do -- | Deals with the Discussion, is used by the other case.
    let mD = discussion i
    return i{condition = Nothing, discussion = liftM convertD mD}
  | otherwise = do -- | Deals with the Condition.
    let acNames' = vars . Maybe.fromJust $ condition i :: Set String
        acNames = Set.toList acNames' :: [String]
    mIds <- mapM IdFromHeadline acNames :: BackendDSL [Maybe Id]
    let existing = Map.fromList . Maybe.catMaybes $ zipWith merge acNames mIds :: Map String Id
        missing = Set.difference acNames' . Set.fromList $ Map.keys existing :: Set String
    created <- liftM (Map.fromList . Maybe.catMaybes) . mapM createMissing $ Set.toList missing
    let mapping = Map.union existing created :: Map String Id
        convert = Maybe.fromJust . (Map.lookup `flip` mapping) :: String -> Id
        c = fmap convert . Maybe.fromJust $ condition i :: AcceptanceCondition Id
    -- | Here we cleverly use the other case to help us .)
    i' <- hToId i{condition = Nothing}
    return i'{condition = Just c}
  where
    merge :: String -> Maybe Id -> Maybe (String, Id)
    merge s (Just i) = Just (s,i)
    merge _ Nothing = Nothing

    createMissing :: String -> BackendDSL (Maybe (String, Id))
    createMissing h = do
      eEI <- SetItem $ mempty <+ mempty{headline = h}
      let go i = Just (h, itemId i)
          no = error $ "Problem in Carbon.Backend.Item:hToId:createMissing for " ++ h ++ " with " ++ show i
      return $ either no go eEI

    convertD :: Discussion (Item String) -> Discussion (Item Id)
    convertD d = d{arguments = Right . go $ arguments d}
      where
        go :: Either (Set (Item String)) (Set ItemId) -> Set ItemId
        go (Left s) = Set.filter (/= mempty) $ Set.map itemId s
        go (Right s) = s

idToH :: Item Id -> BackendDSL (Item String)
idToH i
  | Maybe.isNothing (condition i) = -- Taking care of the Discussion, used by the other case
    case discussion i of
      Nothing -> return i{condition = Nothing, discussion = Nothing}
      (Just d) -> do
        d' <- convertD d
        return i{condition = Nothing, discussion = Just d'}
  | otherwise = do -- Taking care of the condition
    let acIds = Set.toList.  vars . Maybe.fromJust $ condition i
    mapping <- liftM (Map.fromList . map mappingFromItem) $ mapM GetItem acIds
    let convert = Maybe.fromJust . (Map.lookup `flip` mapping)
        c = fmap convert . Maybe.fromJust $ condition i
    i' <- idToH i{condition = Nothing}
    return i'{condition = Just c}
  where
    mappingFromItem :: Either Error (Item Id) -> (Id, String)
    mappingFromItem (Left e) = error $ unlines ["Problem in Carbon.Backend.Item:idToH:mappingFromItem:",e]
    mappingFromItem (Right i)
      | Maybe.isJust (description i) = (itemId i, headline . Maybe.fromJust $ description i)
      | otherwise = error $ unlines ["Problem in Carbon.Backend.Item:idToH:mappingFromItem:"
                                    ,"No description for item: "++show i]

    convertD :: Discussion (Item Id) -> BackendDSL (Discussion (Item String))
    convertD d = do
      as <- go $ arguments d
      return d{arguments = as}
      where
        go :: Either (Set (Item Id)) (Set ItemId) -> BackendDSL (Either (Set (Item String)) (Set ItemId))
        go (Left is) = do
          is' <- mapM idToH $ Set.toList is
          return . Left $ Set.fromList is'
        go (Right s) = return $ Right s

{-| A map from the Items Id and it's parents Ids to the Items Id. |-}
fromParentMap :: Item Id -> Map ItemId ItemId
fromParentMap i = Map.fromList . zip (itemId i : parents i) . repeat $ itemId i

{-| fromParentMap for a list of Items. |-}
fromParentMaps :: [Item Id] -> Map ItemId ItemId
fromParentMaps = Map.unions . map fromParentMap

{-|
  Fetches Items that are the arguments of a discussion
  Arguments of a Discussion are an Either of two Sets.
|-}
getArgs :: Either (Set (Item Id)) (Set ItemId) -> BackendDSL [Item Id]
getArgs (Left is) = return $ Set.toList is
getArgs (Right ids) = do
  (errs, is) <- liftM Either.partitionEithers . mapM GetItem $ Set.toList ids
  unless (null errs) $
    let msg = "In Carbon.Backend.Logic:getArgs, could not fetch some Items of " ++ show ids
    in error . unlines $ msg:errs
  return is

{-|
  Maps an operation over all arguments of an item.
  This will be required when dealing with Discussions,
  so that an autoCondition of all Items can be performed.
  For items that are not discussions, this function will perform id.
|-}
mapArgs :: Item Id -> (Item Id -> BackendDSL (Item Id)) -> BackendDSL (Item Id)
mapArgs i f
  | itemIsDiscussion i = do
    let d = Maybe.fromJust $ discussion i
    args <- getArgs $ arguments d
    args' <- mapM f args
    return $ i <+ d <+ args'
  | otherwise = return i
