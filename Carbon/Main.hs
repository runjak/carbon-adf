{-# LANGUAGE DoAndIfThenElse #-}
module Main (main) where
{-
  The Main module, which is supposed to link everything together.
-}
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Maybe (isJust, fromJust)
import System.Environment (getArgs)
import qualified System.IO.Strict as Strict

import Carbon.Backend (loadBackend, tryBackend)
import Carbon.Config (Config, nullConfig, readConfig, writeConfig)
import Carbon.Data.Id
--import qualified Carbon.Backend.Logic   as BLogic
import qualified Carbon.Data.Logic as Logic
import qualified Carbon.Data.Logic.Evaluation as Evaluation
import qualified Carbon.Main.Reflection as Reflection
import qualified Carbon.Website as Web (serve)

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
--  "diamond":path:did:rename:_ -> diamond path did $ read rename
    "parse":method:[]             -> parse method Nothing
    "parse":"diamond":conf:[]     -> parseD conf Nothing
    "parse":"diamond":conf:file:_ -> parseD conf $ Just file
    "parse":method:file:_         -> parse method $ Just file
    path:_ -> withConfig path startup
    _ -> help

help :: IO ()
help = mapM_ putStrLn [
    "carbon Version " ++ Reflection.version ++ " complete build: " ++ Reflection.date
  , "---------------------------------------------------------------------------------"
  , "Simple start:      $ carbon <configFile::Filepath>"
  , "Create nullConfig: $ carbon nullConfig <location::Filepath>"
  , "Input for diamond: $ carbon diamond <configFile::Filepath>"
  , "                           <discussionId::Int> <rename::Bool>"
  , "Parsing stuff:     $ carbon parse {ac, exp, instance} [file::Filepath]"
  , "                     carbon parse diamond <configFile::Filepath> [file::Filepath]"
  , "Misc information:  $ openrain info"
  , "Get this message:  $ carbon {--help,-help,help}"
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
--diamond :: FilePath -> String -> Bool -> IO ()
--diamond path did' rename = do
--  executor <- tryBackend path
--  let did = fromId . wrap $ read did'
--  input <- executor $ BLogic.diamondInput rename did
--  putStrLn $ "Carbon.Backend.Logic:diamondInput " ++ show did
--  putStrLn input

{-|
  Function to test parsing capabilities.
|-}
parse :: String -> Maybe FilePath -> IO ()
parse = parse'
  where
    parse' "ac"       = goAc       <=< getInput
    parse' "exp"      = goExp      <=< getInput
    parse' "instance" = goInstance <=< getInput
    parse' x = const . putStrLn $ "Undefined parse method: "++x

    goAc       = either putStrLn print . uncurry (Logic.execParser  Logic.parseAc      )
    goExp      = either putStrLn print . uncurry (Logic.execParser' Logic.parseExp     )
    goInstance = either putStrLn print . uncurry (Logic.execParser  Logic.parseInstance)

parseD :: FilePath -> Maybe FilePath -> IO ()
parseD c input = withConfig c $ \conf -> do
  (fName, fData) <- getInput input
  putStrLn "Starting evaluation…"
  results <- Evaluation.run conf fName fData
  putStrLn "Results obtained from Evaluation.run:"
  print results

{-|
  Normal running webservice
|-}
startup :: Config -> IO ()
startup config = do
  mBackend <- runMaybeT $ loadBackend config
  case mBackend of
    (Just b) -> Web.serve b config
    Nothing -> putStrLn "Could not load backend."

getInput :: Maybe FilePath -> IO (String, String)
getInput Nothing      = liftM ((,) "StdIn") getContents
getInput (Just fName) = liftM ((,) fName) . Strict.run $ Strict.readFile fName
