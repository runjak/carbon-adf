{-# LANGUAGE GADTs #-}
module OpenBrain.Argumentation.Definitions where
{-|
  This module contains definitions for
  what Arguments and Relations may be
  and how they can be used.
  It draws from the Paper 'Argumentation in Artificial Intelligence'
  with the ISBN 978-0-387-98196-3.
|-}
import Data.Function
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

{-|
  This is the basic definition of an ArgumentationFramework from the paper
  with the difference that it uses lists instead of sets.
  The first list is a list of all arguments in the AF,
  the second list is a list of relations (source, target)
  between the arguments in an AF.
|-}
type SimpleAF a = ([a],[(a,a)])

{-|
  Similar to SimpleAF but with better access times.
  Also makes sure that arguments contains no duplicates
  and that no argument is targeted twice.
|-}
data AF a where
  AF :: Ord a => {
    nodes :: Set a         -- | The set of Arguments
  , edges :: Map a (Set a) -- | The set of Relations source->target
  } -> AF a
  {-|
    EAF means Extended ArgumentationFramework
    and in difference to AF an EAF carries along an annotation function
    that maps from tuples of Relations between Arguments (Source, Target)
    to the Annotation type which carries additional information for a relation.
  |-}
  EAF :: Ord a => {
    nodes      :: Set a
  , edges      :: Map a (Set a)
  , annotation :: (a, a) -> Annotation
  } -> AF a

instance (Eq a) => Eq (AF a) where
  af == af' = and [ ((==) `on` nodes) af af'
                  , ((==) `on` edges) af af']

instance (Show a) => Show (AF a) where
  show af = let ns = show $ nodes af
                es = show $ edges af
            in  "AF {nodes = " ++ ns ++ ", edges = " ++ es ++ "}"

{-| Fetches a list of arguments from an ArgumentationFramework. |-}
arguments :: AF a -> Set a
arguments = nodes

{-| Fetches all targets for a single argument |-}
targets :: (Ord a) => AF a -> a -> Set a
targets f a = maybe Set.empty id . Map.lookup a $ edges f

{-| Fetches all relations from an ArgumentationFramework |-}
relations :: (Ord a) => AF a -> Set (a, a)
relations f = let args = arguments f
                  rels = Set.map (\a -> Set.map ((,) a) $ targets f a) args
              in  Set.unions $ Set.toList rels

{-| Conversion from SimpleAF to AF |-}
toAF :: (Ord a) => SimpleAF a -> AF a
toAF (args, rels) = AF {
    nodes = Set.fromList args
  , edges = Map.fromList [(a, Set.fromList [b | (a', b) <- rels, a' == a])|a <- args]
  }

{-| Conversion from AF to SimpleAF |-}
toSimpleAF :: AF a -> SimpleAF a
toSimpleAF af = (Set.toList $ nodes af, [(a, b) | (a, b') <- Map.toList (edges af), b <- Set.toList b'])

{-| Annotations to be used in EAF's |-}
data Annotation = Attack
                | Defense
                deriving (Show, Eq, Ord, Enum)

{-| Enhances an AF a with an annotation function so that it's an EAF. |-}
annotate :: AF a -> ((a, a) -> Annotation) -> AF a
annotate (AF n e)     f = EAF n e f -- | Sets the annotation function f
annotate (EAF n e f') f = EAF n e f -- | Overwrites the annotation functionn f' with f

{-|
  Makes sure a given AF is an EAF by defaulting the annotation to Attack
  for all relations if the AF is not already an EAF.
|-}
mkEAF :: AF a -> AF a
mkEAF af@(EAF _ _ _) = af
mkEAF af@(AF _ _)    = annotate af $ const Attack
