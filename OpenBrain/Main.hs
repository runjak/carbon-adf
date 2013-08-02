{-# LANGUAGE DoAndIfThenElse #-}
module Main (main) where
{-
  The Main module, which is supposed to link everything together.
-}
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Maybe (isJust, fromJust)
import System.Environment (getArgs)

import OpenBrain.Backend (loadBackend, tryBackend)
import OpenBrain.Config (Config, nullConfig, readConfig, writeConfig)
import OpenBrain.Data.Id
import qualified OpenBrain.Backend.Logic   as BLogic
import qualified OpenBrain.Data.Logic      as Logic
import qualified OpenBrain.Main.Reflection as Reflection
import qualified OpenBrain.Website         as Web (serve)

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
    "diamond":path:did:rename:_ -> diamond path did $ read rename
    "parse":method:[]     -> parse method Nothing
    "parse":method:file:_ -> parse method $ Just file
    path:_ -> withConfig path startup
    _ -> help

help :: IO ()
help = mapM_ putStrLn [
    "openBrain Version " ++ Reflection.version
  , "----------------------------------------------------------------------"
  , "Simple start:      $ openBrain <configFile::Filepath>"
  , "Create nullConfig: $ openBrain nullConfig <location::Filepath>"
  , "Input for diamond: $ openBrain diamond <configFile::Filepath>"
  , "                           <discussionId::Int> <rename::Bool>"
  , "Parsing stuff:     $ openBrain parse <method::String> [file::Filepath]"
  , "                 -- with method from {'ac','diamond','exp','instance'}"
  , "Misc information:  $ openrain info"
  , "Get this message:  $ openBrain {--help,-help,help}"
  ]

withConfig :: FilePath -> (Config -> IO ()) -> IO ()
withConfig path go = do
  putStrLn $ "Loading config from '" ++ path ++ "'."
  mConf <- readConfig path
  case mConf of
    (Just c) -> go c
    Nothing  -> putStrLn $ "Could not load config from '" ++ path ++ "'."

{-|
  Function to generate input for diamond for test purposes.
  The given String is expected to belong to a discussionId.
  Input for diamond will be printed to console.
|-}
diamond :: FilePath -> String -> Bool -> IO ()
diamond path did' rename = do
  executor <- tryBackend path
  let did = fromId . wrap $ read did'
  input <- executor $ BLogic.diamondInput rename did
  putStrLn $ "OpenBrain.Backend.Logic:diamondInput " ++ show did
  putStrLn input

{-|
  Function to test parsing capabilities.
|-}
parse :: String -> Maybe FilePath -> IO ()
parse = parse'
  where
    parse' "ac"       = goAc       <=< input
    parse' "diamond"  = goDiamond  <=< input
    parse' "exp"      = goExp      <=< input
    parse' "instance" = goInstance <=< input
    parse' x = const . putStrLn $ "Undefined parse method: "++x

    input :: Maybe FilePath -> IO (String, String)
    input Nothing      = liftM ((,) "StdIn") getContents
    input (Just fName) = liftM ((,) fName) $ readFile fName

    goAc       = either putStrLn print . uncurry (Logic.execParser  Logic.parseAc     )
    goDiamond  = either putStrLn print . uncurry (Logic.execParser  Logic.parseDiamond)
    goExp      = either putStrLn print . uncurry (Logic.execParser' Logic.parseExp    )
    goInstance = either putStrLn print . uncurry (Logic.execParser  Logic.parseDiamond)

{-|
  Normal running webservice
|-}
startup :: Config -> IO ()
startup config = do
  mBackend <- runMaybeT $ loadBackend config
  case mBackend of
    (Just b) -> Web.serve b config
    Nothing -> putStrLn "Could not load backend."
