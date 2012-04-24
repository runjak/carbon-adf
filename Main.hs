module Main (main) where
{-
  The Main module, which is supposed to link everything together.
-}

import Config (Config, nullConfig, readConfig, writeConfig)
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
        then putStrLn . show $ fromJust mConf -- | change here to work with config
        else putStrLn $ "Could not load config from '" ++ path ++ "'."
    _ -> help

help :: IO ()
help = do
  putStrLn "Herthlokel Version 0.0"
  putStrLn "----------------------"
  putStrLn "Simple start:       $herthlokel <configFile>"
  putStrLn "Create nullConfig:  $herthlokel nullConfig <location>"
  putStrLn "Get this message:   $herthlokel {--help,-help,help,}"
