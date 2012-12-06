module OpenBrain.Argumentation.Semantics (
    Argument, Relation
  , ArgumentationFramework(..)
  , module Graph
) where

import qualified Data.Set as Set

import OpenBrain.Data.Graph as Graph

type Argument = Vertex
type Relation = Edge

class Monad af => ArgumentationFramework af where
  {-|
    A set of Arguments is conflict-free if there are no two Arguments a,b
    in the set so that a attacks b.
  |-}
  conflictFree :: [Argument] -> af Bool
  {-|
    An Argument a is acceptable with respect to a set s of Arguments
    if for each Argument b that attacks a there is an Argument s' in s
    so that s' attacks b.
  |-}
  acceptable :: Argument -> [Argument] -> af Bool
  {-|
    The characteristic function for acceptable,
    which delivers the set s' of all arguments that
    are acceptable with respect to a given set s of arguments.
  |-}
  acceptables :: [Argument] -> af [Argument]
  {-|
    A set of arguments is admissible if it is conflict-free and
    each argument in the set is acceptable with respect to the set.
  |-}
  admissible :: [Argument] -> af Bool
  {-| The set of all admissible sets. |-}
  admissibleSets :: af [[Argument]]
  {-|
    A set of arguments is unattacked if there is no argument outside the set
    that attacks an argument inside the set.
  |-}
  unattacked :: [Argument] -> af Bool
  {-| The set of all unattacked sets. |-}
  unattackedSets :: af [[Argument]]
  {-|
    The restriction modifies the AF so that it only
    contains the arguments given and the relations between these arguments.
  |-}
  restriction :: [Argument] -> af ()
  {-|
    A set of argument is complete if it is admissible
    and every argument that is acceptable towards this set of arguments
    is element of the set.
  |-}
  complete :: [Argument] -> af Bool
  complete args = do -- | Beautify this implementation.
    isAdmissable <- admissible args
    accs <- acceptables args
    let hasAcceptables = (Set.fromList accs) `Set.isSubsetOf` (Set.fromList args)
    return $ isAdmissable && hasAcceptables
  {-| The set of complete sets. |-}
  completeSets :: af [[Argument]]
  {-|
    A set is grounded if it's a least complete set.
    This means the set has no subset that is complete.
  |-}
  grounded :: [Argument] -> af Bool
  {-| The set of all grounded sets. |-}
  groundedSets :: af [[Argument]]
  {-|
    A set of arguments is stable if it is conflict-free and
    all Arguments that are not in the set are attacked by the set.
  |-}
  stable :: [Argument] -> af Bool
  {-|
    A set of arguments is preferred if it's a maximal admissible set.
    This means for any admissible set s that there is no argument a so
    that s with a is admissible.
  |-}
  preferred :: [Argument] -> af Bool
  {-| The set of all preferred sets. |-}
  preferredSets :: af [[Argument]]
  {-|
    Left outside: stage, semi-stable, ideal, cf2, prudent
  |-}
