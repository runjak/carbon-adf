{-# LANGUAGE DoAndIfThenElse #-}
module Main (main) where
{-
  The Main module, which is supposed to link everything together.
-}
import Backend (Backend, loadBackend)
import Config (Config, nullConfig, readConfig, writeConfig)
import qualified Website.Main as Web (serve)

import Control.Monad
import Data.Maybe (isJust, fromJust)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    "--help":_  -> help
    "-help":_   -> help
    "help":_    -> help
    "nullConfig":path:_ -> do
      putStrLn $ "Creating a nullConfig in '" ++ path ++ "'."
      writeConfig path nullConfig
    path:_ -> do
      putStrLn $ "Loading config from '" ++ path ++ "'."
      mConf <- readConfig path
      if isJust mConf
        then startup $ fromJust mConf
        else putStrLn $ "Could not load config from '" ++ path ++ "'."
    _ -> help

help :: IO ()
help = mapM_ putStrLn [
    "openBrain Version 0.1"
  , "---------------------"
  , "Simple start:       $openBrain <configFile>"
  , "Create nullConfig:  $openBrain nullConfig <location>"
  , "Get this message:   $openBrain {--help,-help,help,}"
  ]

startup :: Config -> IO ()
startup config = do
  mBackend <- loadBackend config
  if isJust mBackend
  then Web.serve (fromJust mBackend) config
  else putStrLn "Could not load backend."
