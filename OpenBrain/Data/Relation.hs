module OpenBrain.Data.Relation where
{- Defines relations between OpenBrain.Data.Information -}

import System.Time (CalendarTime)

import OpenBrain.Data.Id
import OpenBrain.Data.Information

type RelationId = Id
data Relation = Relation {
    creation    :: CalendarTime
  , deletion    :: Maybe CalendarTime
  , relation    :: RelationType
  , relationId  :: RelationId
  , source      :: InformationId
  , target      :: InformationId
  } deriving (Eq, Show)

data RelationType = Parent  -- | Source = Child, Target = Parent
                  | Attack  -- | Source = Attacker
                  | Defense -- | Source = Defender
                  deriving (Eq, Show)

