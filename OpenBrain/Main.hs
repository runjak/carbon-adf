{-# LANGUAGE DoAndIfThenElse #-}
module Main (main) where
{-
  The Main module, which is supposed to link everything together.
-}
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Maybe (isJust, fromJust)
import System.Environment (getArgs)

import OpenBrain.Backend (loadBackend)
import OpenBrain.Config (Config, nullConfig, readConfig, writeConfig)
import qualified OpenBrain.Main.Reflection as Reflection
import qualified OpenBrain.Website as Web (serve)

main :: IO ()
main = do
  args <- getArgs
  case args of
    "--help":_  -> help
    "-help":_   -> help
    "help":_    -> help
    "info":_    -> putStrLn Reflection.info
    "nullConfig":path:_ -> do
      putStrLn $ "Creating a nullConfig in '" ++ path ++ "'."
      writeConfig path nullConfig
    path:_ -> do
      putStrLn $ "Loading config from '" ++ path ++ "'."
      mConf <- readConfig path
      case mConf of
        (Just c) -> startup c
        Nothing -> putStrLn $ "Could not load config from '" ++ path ++ "'."
    _ -> help

help :: IO ()
help = mapM_ putStrLn [
    "openBrain Version " ++ Reflection.version
  , "----------------------------------------------------"
  , "Simple start:      $ openBrain <configFile>"
  , "Create nullConfig: $ openBrain nullConfig <location>"
  , "Misc information:  $ openrain info"
  , "Get this message:  $ openBrain {--help,-help,help}"
  ]

startup :: Config -> IO ()
startup config = do
  mBackend <- runMaybeT $ loadBackend config
  case mBackend of
    (Just b) -> Web.serve b config
    Nothing -> putStrLn "Could not load backend."
