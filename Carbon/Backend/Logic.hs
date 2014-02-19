module Carbon.Backend.Logic where

import Control.Arrow ((&&&), second)
import Control.Monad
import Data.Function (on)
import Data.List (groupBy, nub, nubBy, partition)
import Data.Map (Map, (!))
import Data.Monoid (Monoid(..))
import Data.Set (Set, (\\))
import qualified Data.Either as Either
import qualified Data.Map   as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set   as Set

import Carbon.Backend.DSL
import Carbon.Common
import Carbon.Data
import qualified Carbon.Backend.Item as BItem

{-|
  Generates the condition for an Item by taking into account all it's relations.
  There's three different cases:
  1: Item doesn't have a Condition -> Simple Condition is generated, and case 2 is performed.
  2: Items Condition has a proofStandard -> We generate the Condition by the proofStandard
  3: Items Condition has a formula but no proofStandard -> This is a custom formula,
     we don't change it. However, all incomming relations become custom if they aren't.
  Note, that the returned Item is not safed via SetItem,
  but relations as in case 3 are.
|-}
autoCondition :: Item Id -> BackendDSL (Item Id)
autoCondition item
  | Maybe.isNothing $ condition item = do -- case 1
    let p = mempty :: ProofStandard
        c = mempty :: AcceptanceCondition Id
    autoCondition $ item <+ c <+ p
  | Maybe.isJust . proofStandard $ getC item = do -- case 2
    let rels = Maybe.mapMaybe relation $ relations item
        incomming = filter ((itemId item ==) . target) rels
        sourceIds = map source incomming
    (sourceErrors, sourceItems) <- liftM Either.partitionEithers $ mapM GetItem sourceIds
    unless (null sourceErrors) $ -- Exception in case of errors
      let problem = "Could not fetch items in Carbon.Backend.Logic:autoCondition":sourceErrors
      in error $ unlines problem
    let getProofStandard = mkGetProofStandard sourceItems
        newExp = mkFormula (getP item) incomming getProofStandard
    return $ item <+ getC item <+ newExp
  | otherwise = do -- case 3 - sets all incomming relations to relationType custom.
    let incomming = filter ((==) (itemId item) . target . getR) $ relations item
        notCustom = filter ((/=) RelationCustom . relationType . getR) incomming
        setCustom = map (\i -> i <+ getR i <+ RelationCustom) notCustom :: [Item Id]
    (errs, _) <- liftM Either.partitionEithers $ mapM SetItem setCustom
    unless (null errs) $
      let problem = "Could not set items in Carbon.Bakend.Logic:autoCondition":errs
      in error $ unlines problem
    return item
  where
    getC = Maybe.fromJust . condition
    getP = Maybe.fromJust . proofStandard . getC
    getR = Maybe.fromJust . relation

    mkGetProofStandard :: [Item Id] -> ItemId -> ProofStandard
    mkGetProofStandard items i =
      let go m item = Map.insert (itemId item) (getP item) m
          sourceProofMap = foldl go Map.empty items
      in sourceProofMap Map.! i

{-
  This is how we calculate the Formula for an Item in case where
  the Item has a ProofStandard, and no custom formula.
  The Formula is calculated by herpaDerpDingDong.
  FIXME check if the  conditions generated in the canDefend block must be checked against
    arguments with larger proofStandards.
    Because this way, it may be possible, that the whole condition is kinde broken.
-}
mkFormula :: ProofStandard -> [Relation] -> (ItemId -> ProofStandard) -> Exp ItemId
mkFormula itemProofStandard incomming getProofStandard =
  {- Of the incomming relations, only these with >= ProofStandard are interesting. -}
  let incomming' = filter ((itemProofStandard <=) . getProofStandard . source) incomming
  {- incomming' is devided into attacks and supports: -}
      rFilter r = filter $ (r ==) . relationType
      attacks = rFilter RelationAttack incomming'
      supports = rFilter RelationSupport incomming'
  {- We view attacks and supports only in sets respective to the same ProofStandard: -}
      asPairs = do -- :: [(Set Relation, Set Relation)]
        let hasP p = filter $ (p ==) . getProofStandard . source
        p <- [PSScintillaOfEvidence ..]
        let as = hasP p attacks
            ss = hasP p supports
        return (Set.fromList as, Set.fromList ss)
  {- We now build up acceptance conditions on a per ProofStandard basis: -}
      conditions = do -- :: [Exp ItemId]
        -- Functions to compose a condition:
        let body = map (Var . source) . Set.toList
            nBody = map Neg . body
        -- Groups we consider:
        (attackSet, supportSet) <- asPairs
        -- It's no use if attacks and supports are empty:
        guard . not $ Set.null attackSet && Set.null supportSet
        -- For each set of attacks we consider it's powerset!
        attackSet' <- powerset' attackSet
        -- There'll have to be attacks!
        guard . not $ Set.null attackSet'
        let canDefend = Set.size attackSet' < Set.size supportSet
        if canDefend then
          (do supportSet' <- powerset' supportSet
              guard $ Set.size supportSet' >= Set.size attackSet'
              let notAttack = Set.difference attackSet attackSet'
              return . and' $ body attackSet' ++ body supportSet' ++ nBody notAttack)
          else return . and' $ nBody attackSet'
  {- We accept if at least one condition yields true: -}
  in null conditions ? (Const True, simplify $ or' conditions)

{-|
  Generates the content of a diamond input file from a DiscussionId.
  The content is contained in both parts of the tuple,
  but the first field uses the ids whereas the second
  has the ids translated for headlines.
|-}
diamondInput :: ItemId -> BackendDSL (String, String)
diamondInput iId = do
  item <- liftM (either no id) $ GetItem iId
  unless (itemIsDiscussion item) $ noDisc iId
  let disc = Maybe.fromJust $ discussion item
  args <- BItem.getArgs $ arguments disc -- These are expected to have conditions and headlines.
  -- Making sure all args got conditions:
  args' <- mapM autoCondition args
  -- Updating the formulas for all Items:
  let formulaMap = BItem.fromParentMaps args'
      updated    = Maybe.mapMaybe (updateFormula formulaMap) args'
  -- Saving changed items:
      keep       = filter (Set.notMember `flip` Set.fromList updated) args'
      args''     = keep ++ updated
  mapM_ ModifyAcceptanceCondition updated
  -- How to rename Ids to Headlines:
  let idAndHeadline = itemId &&& (headline . Maybe.fromJust . description) :: Item Id -> (ItemId, Headline)
      renameMap     = Map.fromList $ map idAndHeadline'' args''            :: Map ItemId Headline
  -- Building the acs:
      idAndFormula  = itemId &&& (formula . Maybe.fromJust . condition)    :: Item Id -> (ItemId, Exp ItemId)
      acs           = map (uncurry AC . idAndFormula) args''               :: [ACondition ItemId]
      acs'          = map (fmap (renameMap !)) acs                         :: [ACondition Headline]
  return $ (show . instanceFromAcs) `both` (map (fmap show) acs, acs')
  where
    no = error . ("Could not fetch Item in Carbon.Backend.Logic:diamondInput\n"++)
    noDisc = error . ("Cannot perform Carbon.Backend.Logic:diamondInput on Item: "++) . show

    both f (a,b) = (f a, f b)

    idAndHeadline' :: Item Id -> Maybe (ItemId, Headline)
    idAndHeadline' i = do
      d <- description i
      return (itemId i, headline d)

    idAndHeadline'' :: Item Id -> (ItemId, Headline)
    idAndHeadline'' i = do
      let mIH = idAndHeadline' i
          h = "No Headline found for " ++ show (itemId i)
      Maybe.fromMaybe (itemId i, h) mIH

    updateFormula :: Map ItemId ItemId -> Item Id -> Maybe (Item Id)
    updateFormula m i = let ac = Maybe.fromJust $ condition i
                            f  = formula ac
                            f' = fmap (m Map.!) $ formula ac
                        in (f == f') ? (Nothing, Just $ i <+ ac <+ f')

{-|
  Performs magic to add Results to an Item.
  This also resets votes to 0.
|-}
addResults :: Item Id -> Results String -> Item Id
addResults item rs =
  -- How we merge:
  let merge x y = nub $ x ++ y
  -- What's currently in the item:
      rSet = getRS item :: ResultSet
      rsPairs = map (items &&& resultType) :: [Result] -> [(Set (ResultState, Id), [ResultType])]
      cResMap = Map.fromList . rsPairs $ results rSet :: Map (Set (ResultState, Id)) [ResultType]
  -- What shall be added:
      (Results rs') = fmap read rs :: Results Id
      nResMap = Map.fromListWith merge $ concatMap forPair rs' :: Map (Set (ResultState, Id)) [ResultType]
  -- What we got:
      resMap = Map.unionWith merge cResMap nResMap
      res = map mkResult $ Map.toList resMap
  in item <+ rSet <+ res
  where
    no = error . ("Could not fetch Item in Carbon.Backend.Logic:saveResults\n"++)

    getRS :: Item Id -> ResultSet
    getRS i
      | Maybe.isNothing $ resultSet i = mempty
      | otherwise = Maybe.fromJust $ resultSet i

    forPair :: (ResultType, [DiamondResult Id]) -> [(Set (ResultState, Id), [ResultType])]
    forPair (rT, drs) = zip (map fromDR drs) $ repeat [rT]

    fromDR :: DiamondResult Id -> Set (ResultState, Id)
    fromDR dr =
      let ins = zip (repeat In) $ inSet dr
          uns = zip (repeat Udec) $ udecSet dr
          ous = zip (repeat Out) $ outSet dr
      in Set.fromList $ ins ++ uns ++ ous

    mkResult :: (Set (ResultState, Id), [ResultType]) -> Result
    mkResult (is, rt) = (mempty <+ is) <+ rt

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
fitInstance :: UserId -> Item Id -> Instance Headline -> BackendDSL (Either Error (Item Id))
fitInstance uid item i = do
  LogString "Carbon.Backend.Logic:fitInstance"
  LogString $ "Instance is:\n" ++ show i
  args <- BItem.getArgs . arguments $ getD item
  -- Adding missing nodes:
  added <- addMissingNodes uid args i
  -- Building a list of all nodes, aswell as a set of current relations:
  let nodes = nubBy ((==) `on` itemId) $ args ++ added :: [Item Id]
      cRels' = Maybe.mapMaybe relation nodes :: [Relation]
      currentRels = Set.fromList $ map (source &&& target) cRels' :: Set (ItemId, ItemId)
  -- Establishing a mapping for Headline -> ItemId
  let hAndId = (headline . Maybe.fromJust . description) &&& itemId :: Item Id -> (Headline, ItemId)
      hToIdMap = Map.fromList $ map hAndId nodes :: Map Headline ItemId
      i' = fmap (hToIdMap !) i :: Instance ItemId
      possibleRels = iRels i' :: Set (ItemId, ItemId)
  -- Adding missing relations
  addRels uid . Set.toList $ possibleRels \\ currentRels
  -- Removing now impossible Relations
  delRels nodes . Set.toList $ currentRels \\ possibleRels
  -- Saving the conditions/formulas
  nodes' <- setFormulas uid nodes i
  -- Saving the item itself
  let nIds = Set.fromList $ map itemId nodes'
      d = (getD item){arguments = Right nIds} :: Discussion (Item Id)
  SetItem $ item{commitAuthor = uid, commitMessage = "Inserted an instance."} <+ d
  where
    getD = Maybe.fromJust . discussion

    addMissingNodes :: UserId -> [Item Id] -> Instance Headline -> BackendDSL [Item Id]
    addMissingNodes uid is i = do
      let heads = Maybe.mapMaybe (liftM headline . description) is :: [Headline]
          newHeads = names i \\ Set.fromList heads :: Set Headline
          baseI = mempty{commitAuthor = uid, commitMessage = "Item created by Carbon.Backend.Logic:fitInstance"}
          newIs = map (\h -> baseI <+ mempty{headline = h}) $ Set.toList newHeads :: [Item Id]
      (errs, is) <- liftM Either.partitionEithers $ mapM SetItem newIs
      unless (null errs) . error . unlines $ "Problem in Carbon.Backend.Logic:addMissingNodes" : errs
      return is

    iRels :: Instance ItemId -> Set (ItemId, ItemId)
    iRels = Set.unions . map go . conditions
      where
        go ac = let sources = vars $ aCondition ac
                in Set.map (\c -> (c, aHead ac)) sources

    addRels :: UserId -> [(ItemId, ItemId)] -> BackendDSL ()
    addRels uid rels = do
      let baseI = mempty{commitAuthor = uid, commitMessage = "Relation created by Carbon.Backend.Logic:fitInstance"}
          newRs = map (\(s,t) -> baseI <+ mempty{source=s, target=t} <+ RelationCustom) rels
      LogString $ "Relations to create: " ++ show rels
      (errs, _) <- liftM Either.partitionEithers $ mapM SetItem newRs
      unless (null errs) . error . unlines $ ("Problem creating relations in Carbon.Backend.Logic:fitInstance":) errs
      return ()

    delRels :: [Item Id] -> [(ItemId, ItemId)] -> BackendDSL ()
    delRels nodes rels =
      let nRels = concatMap relations nodes
          shrinkR = (source &&& target) . Maybe.fromJust . relation :: Item Id -> (ItemId, ItemId)
          relToNMap = Map.fromList $ map (shrinkR &&& itemId) nodes :: Map (ItemId, ItemId) ItemId
          deletes = Maybe.mapMaybe (`Map.lookup` relToNMap) rels
      in mapM_ DeleteItem deletes

    {-| This shall work in the following steps:
      - Build a Map Headline Id from the items
      - Instance Headline -> Instance Id
      - Set formula as aCondition where itemId = aHead for all conditions.
    |-}
    setFormulas :: UserId -> [Item Id] -> Instance Headline -> BackendDSL [Item Id]
    setFormulas uid items i = do
      LogString "Carbon.Backend.Logic:fitInstance:setFormulas"
      let hToId = Map.fromList . Maybe.catMaybes $ map iToPair items :: Map Headline Id
          i' = fmap (hToId Map.!) i :: Instance Id
          idToExp = Map.fromList . map (aHead &&& aCondition) $ conditions i' :: Map Id (Exp Id)
          changeSet = Set.fromList $ Map.keys idToExp :: Set Id
          (change, keep) = partition (flip Set.member changeSet . itemId) items
          toChange = Maybe.mapMaybe (setFormula uid idToExp) change :: [Item Id]
      (errs, changed) <- liftM Either.partitionEithers $ mapM SetItem toChange
      LogString $ "Transformation is from " ++ show change
      LogString $ "to " ++ show changed
      unless (null errs) . error . unlines $ "Problem in Carbon.Backend.Logic:fitInstance:setFormulas" : errs
      return $ changed ++ keep
      where
        iToPair :: Item Id -> Maybe (Headline, Id)
        iToPair i = do
          d <- description i
          return (headline d, itemId i)

        setFormula :: UserId -> Map Id (Exp Id) -> Item Id -> Maybe (Item Id)
        setFormula uid m i = do
          f <- Map.lookup (itemId i) m
          let ac = mempty :: AcceptanceCondition Id 
              i' = i{commitAuthor = uid, commitMessage = "Formula adapted by Carbon.Backend.Logic:fitInstance:setFormula"}
          return $ i' <+ ac <+ f

{-|
  Removes a Relation, and updates the targets Exp,
  so that it no longer includes the source.
  On custom Conditions this is done via
  Carbon.Data.Logic.Exp:removeVar.
  Otherwise autoCondition is used.
  Returns the Target of the relation after modification.
  Notice that SetItem would still need to be performed,
  since changes are not saved anywhere.
|-}
removeRelation :: Item Id -> BackendDSL (Item Id)
removeRelation rItem
  | Maybe.isNothing $ relation rItem = return rItem
  | otherwise = do -- Change formula
    let t = target $ getR rItem
    i <- liftM (either no id) $ GetItem t
    case proofStandard (getC i) of
      (Just _) -> do -- Formula is changed via autoCondition
        let nRels = filter ((itemId rItem /=) . itemId) $ relations i
        autoCondition $ i <+ nRels
      Nothing -> do -- Formula is changed via removeVar
        let f = formula $ getC i
            f' = removeVar (source $ getR rItem) f
        return $ i <+ getC i <+ f'
  where
    getR = Maybe.fromJust . relation
    getC = Maybe.fromJust . condition -- If it's a target, we expect it to allways have a condition.
    no = error . ("Problem fetching Item in Carbon.Backend.Logic:removeRelation\n"++)
