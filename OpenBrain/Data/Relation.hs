module OpenBrain.Data.Relation(
  Relation(..), RelationType(..)
)where
{- Defines relations between OpenBrain.Data.Information -}

import Data.Char
import Data.Function (on)
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
  } deriving (Show)
instance Eq Relation where
  (==) = (==) `on` relationId
instance Ord Relation where
  compare = compare `on` relationId

data RelationType = Parent     -- | Source = Child, Target = Parent
                  | Collection -- | Source = Information that bundles, Target = Element of the collection
                  | Attack     -- | Source = Attacker, Target = Victim
                  | Defense    -- | Source = Defender, Target = Protegee
                  deriving (Enum, Eq, Ord, Show)

instance FromReqURI RelationType where
  fromReqURI s = case map toLower s of
    "parent"      -> Just Parent
    "attack"      -> Just Attack
    "defense"     -> Just Defense
    "collection"  -> Just Collection
    _             -> Nothing

