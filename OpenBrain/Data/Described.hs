module OpenBrain.Data.Described where

type Title       = String
type Description = String
class Described d where
  title       :: d -> Title
  description :: d -> Description
