module OpenBrain.Data.Relation(
  Relation(..), RelationType(..)
  , module AFD
)where
{- Defines relations between OpenBrain.Data.Information -}

import Data.Char
import Happstack.Server as S
import System.Time (CalendarTime)

import OpenBrain.Data.Id
import OpenBrain.Data.Information hiding (Collection)
import OpenBrain.Argumentation.Definitions as AFD (Annotation(..))

data Relation = Relation {
    comment     :: String
  , creation    :: CalendarTime
  , deletion    :: Maybe CalendarTime
  , relation    :: RelationType
  , relationId  :: RelationId
  , source      :: InformationId
  , target      :: InformationId
  } deriving (Eq, Show)

data RelationType = Parent     -- | Source = Child, Target = Parent
                  | Collection -- | Source = Information that bundles, Target = Element of the collection
                  | Argumentation AFD.Annotation -- | Source = Annotation
                  deriving (Eq, Show)
instance Enum RelationType where
  toEnum 0 = Parent
  toEnum 1 = Collection
  toEnum x = Argumentation . toEnum $ x - 2
  fromEnum Parent            = 0
  fromEnum Collection        = 1
  fromEnum (Argumentation a) = 2 + fromEnum a

instance FromReqURI RelationType where
  fromReqURI s = case map toLower s of
    "parent"      -> Just Parent
    "attack"      -> Just $ Argumentation AFD.Attack
    "defense"     -> Just $ Argumentation AFD.Defense
    "collection"  -> Just Collection
    _             -> Nothing

