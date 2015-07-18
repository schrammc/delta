module System.Delta.Class where

import FRP.Sodium
import System.Delta.Base

import Control.Monad

-- | A class for watching a directory based on functional reactive programming
-- At the core of this class are three event streams:
--
-- * @changedFiles@ is a stream of 'FileInfo's on changed files
--
-- * @newFiles@ is a stream of canonicalized 'FilePath's of newly created files
--
-- * @deletedFiles@ is a stream of canonicalized 'FilePath's of deleted files
class FileWatcher a where
  -- | Each type provides a default watcher for a pass
  defaultWatcher  :: FilePath -> IO a

  -- | An event that gives some info on changed files (disjunct from
  -- deleted and new files)
  changedFiles    :: a -> Event FilePath

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
