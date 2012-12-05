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
  Comparable to SimpleAF, but with better performance.
  Also nodes' and edges' don't allow for duplicates where SimpleAF does.
|-}
data AF' a = AF' {
    nodes' :: Set a         -- | The set of Arguments
  , edges' :: Map a (Set a) -- | The set of Relations source->target
  }
instance AF AF' where
  nodes = nodes'
  edges = edges'

{-| Conversion from SimpleAF to AF |-}
toAF :: (Ord a) => SimpleAF a -> AF' a
toAF (args, rels) = AF' {
    nodes' = Set.fromList args
  , edges' = Map.fromList [(a, Set.fromList [b | (a', b) <- rels, a' == a])|a <- args]
  }

{-| Conversion from AF to SimpleAF |-}
toSimpleAF :: AF' a -> SimpleAF a
toSimpleAF af = (Set.toList $ nodes' af, [(a, b) | (a, b') <- Map.toList (edges' af), b <- Set.toList b'])

{-|
  The general definition for ArgumentationFrameworks as they'll
  be used in OpenBrain. I use a typeclass here so that the handling
  of these Frameworks can be easily extended which will be necessary
  to allow for defense handling AF or such including values or whatever.
|-}
class AF framework where
  nodes :: framework argument -> Set argument
  edges :: framework argument -> Map argument (Set argument)

{-| Fetches a list of arguments from an ArgumentationFramework. |-}
arguments :: (AF f) => f a -> Set a
arguments = nodes

{-| Fetches all targets for a single argument |-}
targets :: (AF f, Ord a) => f a -> a -> Set a
targets f a = maybe Set.empty id . Map.lookup a $ edges f

{-| Fetches all relations from an ArgumentationFramework |-}
relations :: (AF f, Ord a) => f a -> Set (a, a)
relations f = let args = arguments f
                  rels = Set.map (\a -> Set.map ((,) a) $ targets f a) args
              in  Set.unions $ Set.toList rels

