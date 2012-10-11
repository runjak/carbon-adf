module OpenBrain.Data.Relation where
{- Defines relations between OpenBrain.Data.Information -}

import Data.Char
import Happstack.Server as S
import System.Time (CalendarTime)

import OpenBrain.Data.Id
import OpenBrain.Data.Information hiding (Collection)

data Relation = Relation {
    comment     :: String
  , creation    :: CalendarTime
  , deletion    :: Maybe CalendarTime
  , relation    :: RelationType
  , relationId  :: RelationId
  , source      :: InformationId
  , target      :: InformationId
  } deriving (Eq, Show)

data RelationType = Parent      -- | Source = Child, Target = Parent
                  | Attack      -- | Source = Attacker
                  | Defense     -- | Source = Defender
                  | Collection  -- | Source = Information that bundles, Target = Element of the collection
                  deriving (Eq, Show, Enum)

instance FromReqURI RelationType where
  fromReqURI s = case map toLower s of
    "parent"      -> Just Parent
    "attack"      -> Just Attack
    "defense"     -> Just Defense
    "collection"  -> Just Collection
    _             -> Nothing

