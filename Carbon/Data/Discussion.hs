{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
module Carbon.Data.Discussion where

import Control.Applicative
import Control.Monad
import Data.Aeson ((.=), ToJSON(..), object, FromJSON(..), Value(..), (.:))
import Data.Function (on)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import qualified Data.Set as Set

import Carbon.Data.Alias
import Carbon.Data.Common
import Carbon.Data.Id
import Carbon.Data.ResultSet
import Carbon.Data.User

{-|
  I want a Discussion to have arguments that can be extended to items.
  This makes some things a little more difficult:
   - Discussion can't import Carbon.Data.Item or it will form a cycle,
     but I want this to be it's own module
   - Therefore the item type will become a parameter.
     This means that there'll be different class constraints on the item parameter at different times.
|-}
data Discussion item = Discussion {
  discussionId :: Maybe Id
, arguments    :: Either (Set item) (Set ItemId)
, deadline     :: Maybe Timestamp
, participants :: Set UserId
, evaluation   :: EvaluationState
} deriving (Show, Read, Eq, Ord)

data EvaluationState = NotEvaluated | Evaluating | Evaluated
  deriving (Show, Read, Ord, Eq, Enum)

mapItems :: (Functor i, Ord (i a), Ord (i b)) => (a -> b) -> Discussion (i a) -> Discussion (i b)
mapItems f d = d{arguments = mapLeft f $ arguments d}
  where
    mapLeft :: (Functor i, Ord (i a), Ord (i b)) => (a -> b) -> Either (Set (i a)) (Set Id) -> Either (Set (i b)) (Set Id)
    mapLeft f  (Left s) = Left $ Set.map (fmap f) s
    mapLeft _ (Right r) = Right r

-- Instances:
instance FromJSON EvaluationState where
  parseJSON (String "NotEvaluated") = return NotEvaluated
  parseJSON (String "Evaluating") = return Evaluating
  parseJSON _ = mzero

instance (FromJSON i, Ord i) => FromJSON (Discussion i) where
  parseJSON (Object v) = do
    let setI d i = d{discussionId=i}
        setA d a = d{arguments=a}
        setD d l = d{deadline = Just l}
        setP d p = d{participants=p}
        setE d e = d{evaluation=e}
        parseI d = msum [liftM (setI d) (v .: "id"), return d]
        parseA d = msum [liftM (setA d) (v .: "arguments"), return d]
        parseD d = msum [liftM (setD d) (v .: "deadline"), return d]
        parseP d = msum [liftM (setP d) (v .: "participants"), return d]
        parseE d = msum [liftM (setE d) (v .: "evaluation"), return d]
    d <- parseE =<< parseP =<< parseD =<< parseA =<< parseI mempty
    guard $ d /= mempty
    return d
  parseJSON _ = mzero

instance Insertable (Discussion l) Id where
  d <+ i = d{discussionId = Just i}

instance Insertable (Discussion l) (Set l) where
  d <+ args = d{arguments = Left args}

instance Ord l => Insertable (Discussion l) [l] where
  d <+ args = d <+ Set.fromList args

instance Monoid (Discussion i) where
  mempty = Discussion {
      discussionId = mempty
    , arguments    = Right Set.empty
    , deadline     = mempty
    , participants = Set.empty
    , evaluation   = NotEvaluated
    }
  mappend a b = Discussion {
      discussionId = (mappend `on` discussionId) a b
    , arguments    = arguments b
    , deadline     = deadline b
    , participants = participants b
    , evaluation   = evaluation b
    }

instance ToJSON EvaluationState where
  toJSON NotEvaluated = "NotEvaluated"
  toJSON Evaluating   = "Evaluating"

instance ToJSON i => ToJSON (Discussion i) where
  toJSON d = object [
      "id"           .= discussionId d
    , "arguments"    .= arguments d
    , "deadline"     .= deadline d
    , "participants" .= participants d
    , "evaluation"   .= evaluation d
    ]
