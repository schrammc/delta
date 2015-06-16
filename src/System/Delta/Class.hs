module System.Delta.Class where

import FRP.Sodium
import System.Delta.Base

import Control.Monad

class FileWatcher a where
  -- | Each type provides a default watcher for a pass
  defaultWatcher  :: FilePath -> IO a

  -- | An event that gives some info on changed files (disjunct from
  -- deleted and new files)
  changedFiles    :: a -> Event FileInfo

  -- | An event that fires for each new file
  newFiles        :: a -> Event FilePath

  -- | An event that fires for each deleted path
  deletedFiles    :: a -> Event FilePath

  -- | Free all possibly used resources. No event will fire after
  -- this.
  cleanUpAndClose :: a -> IO ()

  -- | Merge two watchers that are watching different directories
  mergeWatchers :: a -> a -> a

  -- | Create a watcher on all of the given paths
  watchPaths :: (FileWatcher a) => [FilePath] -> Maybe (IO a)
  watchPaths []    = Nothing
  watchPaths paths = Just $ do
    watchers <- mapM defaultWatcher paths
    let combinedWatcher = foldl1 mergeWatchers watchers
    return $ combinedWatcher
