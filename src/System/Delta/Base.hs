module System.Delta.Base where

import System.IO.Error
import System.FilePath
import System.Directory
import Data.Time.Clock.POSIX

newtype FileInfo = FileInfo (FilePath,Integer,Bool)
                 deriving (Ord,Eq,Show)

fileInfoPath :: FileInfo -> FilePath
fileInfoPath (FileInfo (path,_,_)) = path

-- | File modification time in milliseconds
fileInfoTimestamp :: FileInfo -> Integer
fileInfoTimestamp (FileInfo (_,time,_)) = time

-- | Is the file a directory
fileInfoIsDir :: FileInfo -> Bool
fileInfoIsDir (FileInfo (_,_,dir)) = dir

mkFileInfo :: FilePath -> IO (FileInfo)
mkFileInfo path  = do
  isDir  <- doesDirectoryExist path
  isFile <- doesFileExist path
  modTime <- getModificationTime path
  let timeMillis = 1000 * (floor $ utcTimeToPOSIXSeconds modTime)
  return $ FileInfo (path, timeMillis, isDir)
