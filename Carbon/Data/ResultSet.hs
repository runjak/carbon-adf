{-# LANGUAGE FlexibleInstances, OverloadedStrings, MultiParamTypeClasses, TypeSynonymInstances #-}
module Carbon.Data.ResultSet where

import Control.Applicative
import Control.Arrow (first, second)
import Control.Monad
import Data.Aeson ((.=), ToJSON(..), object, FromJSON(..), Value(..), (.:))
import Data.Aeson.Types (Parser)
import Data.Function (on)
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import Data.Vector (Vector, (!?))
import qualified Data.Aeson          as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List           as List
import qualified Data.Maybe          as Maybe
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.Text           as Text
import qualified Data.Vector         as Vector

import Carbon.Common ((?))
import Carbon.Data.Alias
import Carbon.Data.Common
import Carbon.Data.Id
import Carbon.Data.Logic.Diamond
import Carbon.Data.Result
import Carbon.Data.User (User)
import qualified Carbon.Data.User as User

data ResultSet = ResultSet {
  resultSetId :: Maybe ResultSetId
, setCreation :: Timestamp
, results     :: [Result]
, voters      :: [(UserId, Voted)]
} deriving (Show, Read, Eq, Ord)

type ThreeSet = (Set ItemId, Set ItemId, Set ItemId)
fromResults :: Results ItemId -> ResultSet
fromResults (Results rs) =
  let drMap = mkMap . map (first drToSets) $ swapExpand rs :: Map ThreeSet (Set ResultType)
      go    = map (uncurry mkResult . first setsToDr) . Map.toList :: Map ThreeSet (Set ResultType) -> [Result]
  in mempty <+ go drMap
  where
    swapExpand :: [(ResultType, [DiamondResult ItemId])] -> [(DiamondResult ItemId, ResultType)]
    swapExpand = concatMap (\(b, as) -> zip as $ repeat b)

    drToSets :: DiamondResult ItemId -> ThreeSet
    drToSets dr = let is = Set.fromList $ inSet   dr
                      us = Set.fromList $ udecSet dr
                      os = Set.fromList $ outSet  dr
                  in (is, us, os)

    setsToDr :: ThreeSet -> DiamondResult ItemId
    setsToDr (is, us, os) = DiamondResult (Set.toList is) (Set.toList us) $ Set.toList os

    mkMap :: [(ThreeSet, ResultType)] -> Map ThreeSet (Set ResultType)
    mkMap = Map.fromListWith Set.union . map (second $ Set.fromList . return)

    mkResult :: DiamondResult ItemId -> Set ResultType -> Result
    mkResult dRes rTypes = (mempty <+ fromDR dRes) <+ Set.toList rTypes

    fromDR :: DiamondResult ItemId -> Set (ResultState, ItemId)
    fromDR dRes =
      let ins   = zip (repeat   In) $ inSet   dRes
          udecs = zip (repeat Udec) $ udecSet dRes
          outs  = zip (repeat  Out) $ outSet  dRes
      in Set.fromList $ ins ++ udecs ++ outs

-- Instances:
instance FromJSON ResultSet where
  parseJSON (Object v) = do
    let setI r i = r{resultSetId = Just i}
        setS r s = r{setCreation = s}
        setR r s = r{results = s}
        setV r v = r{voters  = v}
        parseI r = msum [liftM (setI r) (v .: "id"),          return r]
        parseS r = msum [liftM (setS r) (v .: "setCreation"), return r]
        parseR r = msum [liftM (setR r) (v .: "results"),     return r]
        parseV r = msum [liftM (setV r) (parseVoters $ HashMap.lookup "voters" v), return r]
    r <- parseV =<< parseR =<< parseS =<< parseI mempty
    guard $ r /= mempty
    return r
    where
      parseVoters :: Maybe Value -> Parser [(Id, Bool)]
      parseVoters (Just (Array a)) = liftM Maybe.catMaybes . mapM parseVoter $ Vector.toList a
      parseVoters _ = return []

      parseVoter :: Value -> Parser (Maybe (Id, Bool))
      parseVoter (Array a)
        | Vector.length a >= 2 = do
          let (x, y) = (Maybe.fromJust $ a !? 0, Maybe.fromJust $ a !? 1)
          x' <- parseId x
          y' <- parseVoted y
          return $ Just (x', y')
        | otherwise = return Nothing
      parseVoter _ = return Nothing

      parseId :: Value -> Parser Id
      parseId (String t) = return . read $ Text.unpack t

      parseVoted :: Value -> Parser Bool
      parseVoted (Bool b) = return b
      parseVoted _        = return False

  parseJSON _ = mzero

instance Insertable ResultSet ResultSetId where
  r <+ i = r{resultSetId = Just i}

instance Insertable ResultSet [Result] where
  r <+ rs = r{results = rs}

instance Monoid ResultSet where
  mempty = ResultSet {
      resultSetId = mempty
    , setCreation = mempty
    , results     = mempty
    , voters      = mempty
    }
  mappend a b = ResultSet {
      resultSetId = (mappend  `on` resultSetId) a b
    , setCreation = (mergeStr `on` setCreation) a b
    , results     = results b
    , voters      = voters  b
    }

instance ToJSON ResultSet where
  toJSON r = object [
      "id"          .= resultSetId r
    , "setCreation" .= setCreation r
    , "results"     .= results     r
    , "voters"      .= voters      r
    ]

-- This one makes usage of sameId possible
instance HasId ResultSet where
  getId = Maybe.fromMaybe mempty . resultSetId

-- Testing if changes to a ResultSet are permitted:
okRDelta :: User -> Maybe ResultSet -> Maybe ResultSet -> Bool
okRDelta = okRDelta' . User.userId

okRDelta' :: UserId -> Maybe ResultSet -> Maybe ResultSet -> Bool
okRDelta' _ Nothing Nothing  = True
okRDelta' _ Nothing (Just _) = False
okRDelta' _ (Just _) Nothing = False
{- Voted for the user may only move from False -> True or stay True.
  Only if the user just voted may the votes of results be larger by one.
  The rest must remain unchanged. -}
okRDelta' uId (Just old) (Just new) = 
  let voted = justVoted uId old new
  in and [sameId old new, sameCreation old new, sameVoters old new, voted, okResults voted old new]

-- Printing a table with the involved constraints to aid debugging:
rDeltaTable :: User -> Maybe ResultSet -> Maybe ResultSet -> IO ()
rDeltaTable = rDeltaTable' . User.userId

rDeltaTable' :: UserId -> Maybe ResultSet -> Maybe ResultSet -> IO ()
rDeltaTable' uId mOld mNew = do
  let bothJust   = (&&) `on` Maybe.isJust
      (old, new) = (Maybe.fromJust mOld, Maybe.fromJust mNew)
      details    = [("sameId:       ", sameId old new)
                   ,("sameCreation: ", sameCreation old new)
                   ,("sameVoters:   ", sameVoters old new)
                   ,("justVoted:    ", justVoted uId old new)
                   ,("okResults:    ", okResults (justVoted uId old new) old new)
                   ]
      always     = [("okRDelta:     ", okRDelta' uId mOld mNew)]
      stats      = bothJust mOld mNew ? (details ++ always, always)
  putStrLn "Carbon.Data.ResultSet:rDeltaTable:"
  putStrLn "----------------------------------"
  forM_ stats $ \(x,y) -> putStr x >> print y
  putStrLn "----------------------------------"
  putStrLn "Arguments:"
  putStrLn "UserId:   " >> print uId
  putStrLn "Old:      " >> print (show mOld)
  putStrLn "New:      " >> print (show mNew)

sameCreation :: ResultSet -> ResultSet -> Bool
sameCreation = (==) `on` setCreation

sameVoters :: ResultSet -> ResultSet -> Bool
sameVoters = (==) `on` (Set.fromList . map fst . voters)

justVoted :: Id -> ResultSet -> ResultSet -> Bool
justVoted uId = -- Not being a voter equals already having voted.
  let fromList l = null l ? (True, snd $ head l)
  in (<) `on` (fromList . filter ((==) uId . fst) . voters)

okResults voted = chkResults voted `on` (List.sortBy (compare `on` resultId) . results)
chkResults _ [] [] = True
chkResults _ [] (_:_) = False
chkResults _ (_:_) [] = False
chkResults voted (o:os) (n:ns) =
  let justId = Maybe.isJust $ resultId o
      sameId = (==) `on` resultId
      sameTypes = (==) `on` (List.sort . List.nub . resultType)
      sameItems = (==) `on` items
      sameVotes = (==) `on` votes
      okVotes = (\x y -> abs (x-y) == 1) `on` votes
  in and [justId, sameId o n, sameItems o n, sameVotes o n || (voted && okVotes o n), chkResults voted os ns]
