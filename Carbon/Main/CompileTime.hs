module Carbon.Main.CompileTime where
{-|
  This module gathers information related to Carbon at compile time.
  This will allow Carbon to display more information about itself.
|-}
import Language.Haskell.TH      as TH
import qualified System.Process as Process

version = (stringE . init) =<< runIO (Process.readProcess "grep" ["Version", "carbon.cabal"] "")

date = (stringE . init) =<< runIO (Process.readProcess "date" [] "")
