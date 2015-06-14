module System.Delta.Class where

import FRP.Sodium
import System.Delta.Base

class FileWatcher a where
  defaultWatcher  :: FilePath -> IO a
  changedFiles    :: a -> Event FileInfo
  newFiles        :: a -> Event FilePath
  deletedFiles    :: a -> Event FilePath
  cleanUpAndClose :: a -> IO ()
