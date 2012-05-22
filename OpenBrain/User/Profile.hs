module OpenBrain.User.Profile () where
{- This module defines how and which data can be safed in OpenBrain. -}

import OpenBrain.User.Data (UserId)

{-
  Creation of the Profile should follow existing notations in RDF
  which requires some research.
  The profile should also implement funny classes for JSON, HTML and RDF.
  I'm not entirely sure where to put these instances.
-}
data Profile = Profile {
    userId :: UserId
  , something :: Bool
}
