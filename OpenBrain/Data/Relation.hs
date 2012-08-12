module OpenBrain.Data.Relation where
{- Defines relations between OpenBrain.Data.Information -}

import System.Time (CalendarTime)

import OpenBrain.Data.Id
import OpenBrain.Data.Information

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

