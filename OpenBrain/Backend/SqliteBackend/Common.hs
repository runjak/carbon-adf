module OpenBrain.Backend.SqliteBackend.Common where
{- Definition of the SqliteBackend, that will be enriched with instances to become a full Backend. -}

import Database.HDBC as H

import OpenBrain.Config

data SqliteBackend = SqliteBackend {
    config  :: Config
  , conn    :: ConnWrapper
}
