{-# LANGUAGE TemplateHaskell #-}
module OpenBrain.Main.CompileTime where
{-|
  This module gathers information related to OpenBrain at compile time.
  This will allow OpenBrain to display more information about itself.
|-}
import Language.Haskell.TH      as TH
import qualified System.Process as Process

authors = (stringE . init) =<< (runIO $ Process.readProcess "darcs" ["show", "authors"] "")

linesOfCode = (stringE . init) =<< (runIO $ Process.readProcess "./linesOfCode.sh" [] "")

version = (stringE . init) =<< (runIO $ Process.readProcess "grep" ["Version", "openBrain.cabal"] "")

date = (stringE . init) =<< (runIO $ Process.readProcess "date" [] "")
