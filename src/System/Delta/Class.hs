module System.Delta.Class where

import FRP.Sodium
import System.Delta.Base

import Control.Monad

-- | A type for watching a directory based on functional reactive programming
-- At the core of this class are three event streams:
--
-- * @changedFiles@ is a stream of canonicalized 'FilePath's on changed files
--
-- * @newFiles@ is a stream of canonicalized 'FilePath's of newly created files
--
-- * @deletedFiles@ is a stream of canonicalized 'FilePath's of deleted files
data FileWatcher =
  FileWatcher { newFiles        :: Event FilePath -- ^ Newly created files, renamed
                                                  -- files
              , deletedFiles    :: Event FilePath -- ^ Deleted files, renamed files
              , changedFiles    :: Event FilePath -- ^ Changed files
              , cleanUpAndClose :: IO ()          -- ^ A function to clean and close
                                                  -- ^ this watcher
              }
-- | Merge two watchers that are watching different directories
mergeWatchers :: FileWatcher -> FileWatcher -> FileWatcher
mergeWatchers w1 w2 = FileWatcher{
    newFiles = (newFiles w1) `merge` (newFiles w2)
  , deletedFiles = (deletedFiles w1) `merge` (deletedFiles w2)
  , changedFiles = (changedFiles w1) `merge` (changedFiles w2)
  , cleanUpAndClose = cleanUpAndClose w1 >> cleanUpAndClose w2
  }

