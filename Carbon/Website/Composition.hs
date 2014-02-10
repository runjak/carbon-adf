module Carbon.Website.Composition where

import Control.Concurrent.STM (TVar)
import Control.Monad
import Data.Text (Text)
import System.INotify (INotify)
import qualified Control.Concurrent.STM as STM
import qualified Data.List as List
import qualified Data.Text as Text
import qualified System.INotify as INotify

import Carbon.Config.Composition (Composition(..))
import Carbon.Website.Common
import Carbon.Website.Monad

serve :: OBW Response
serve = do
  t' <- gets compositum
  t  <- liftIO . STM.atomically $ STM.readTVar t'
  return . responseHTML $ toResponse t

compose :: Composition -> IO (TVar Text, INotify)
compose c = do
  putStrLn "Compositing files…"
  t <- compose' c
  t' <- STM.atomically $ STM.newTVar t
  putStrLn "Setting up INotify…"
  n <- INotify.initINotify
  mapM_ (watch n t') $ paths c
  putStrLn "Composition complete."
  return (t', n)
  where
    paths :: Composition -> [FilePath]
    paths c
      | null (replacements c) = [baseFile c]
      | otherwise = let go = concatMap $ paths . snd
                    in List.nub $ baseFile c : go (replacements c)

    watch :: INotify -> TVar Text -> FilePath -> IO INotify.WatchDescriptor 
    watch n t' f =
      let action = do
            putStrLn $ "INotify: composition triggered due to modification of file: " ++ f
            t <- compose' c
            STM.atomically $ STM.writeTVar t' t
            putStrLn "INotify: composition complete."
      in INotify.addWatch n [INotify.Modify] f $ const action

compose' :: Composition -> IO Text
compose' c = do
  putStrLn $ "Reading file " ++ baseFile c
  f <- liftM Text.pack . readFile $ baseFile c
  foldM go f $ replacements c
  where
    go :: Text -> (Text, Composition) -> IO Text
    go input (match, composition) = do
      replacement <- compose' composition
      putStrLn $ "Replacing matches for " ++ Text.unpack match
      return $ Text.replace match replacement input
