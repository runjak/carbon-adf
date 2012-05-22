module OpenBrain.Config.Karma (
    KarmaConfig (..)
  , nullKarmaConfig
) where
{- Configuration of Karma values -}

data KarmaConfig = KarmaConfig {
    ratioEditUser   :: Rational
  , ratioDeleteUser :: Rational
} deriving (Eq, Show, Read)

nullKarmaConfig = KarmaConfig {
    ratioEditUser   = (1 / 10)
  , ratioDeleteUser = (1 / 10)
}
