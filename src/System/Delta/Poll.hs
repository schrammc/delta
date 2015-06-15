module System.Delta.Poll where

import Control.Concurrent
import Control.Monad (foldM)

import qualified Data.Map as M
import Data.Maybe (catMaybes)

import FRP.Sodium

import System.Delta.Base
import System.Delta.Class

import System.Directory
import System.FilePath

data PollWatcher = PollWatcher
                     FilePath
                     (Event FileInfo)
                     (Event FilePath)
                     (Event FilePath)
                     ThreadId

instance FileWatcher PollWatcher where
  defaultWatcher = createPollWatcher 15
  changedFiles (PollWatcher _ e _ _ _) = e
  newFiles     (PollWatcher _ _ e _ _) = e
  deletedFiles (PollWatcher _ _ _ e _) = e
  cleanUpAndClose (PollWatcher _ _ _ _ tId) = killThread tId

createPollWatcher :: Int -> FilePath -> IO PollWatcher
createPollWatcher secs path = do
  (changedEvent, pushChanged) <- sync $ newEvent
  (deletedEvent, pushDeleted) <- sync $ newEvent
  (newFileEvent, pushNewFile) <- sync $ newEvent
  watcherId <- startWatchThread path pushNewFile pushDeleted pushChanged secs
  return $ PollWatcher path changedEvent newFileEvent deletedEvent watcherId

recursiveDescent path = recursiveDescent' M.empty path

-- | Recursively traverse a folder, follows symbolic links,
-- doesn't loop however
recursiveDescent' :: M.Map FilePath FileInfo
                  -> FilePath
                  -> IO (M.Map FilePath FileInfo)
recursiveDescent' visited path | M.member path visited = return visited
recursiveDescent' visited path = do
  isDir  <- doesDirectoryExist path
  inf <- mkFileInfo path
  let visitedWithCurrent = M.insert path inf visited
  if not isDir
  then return $ visitedWithCurrent
  else do
    contentsUnfiltered <- getDirectoryContents path
    let contentsFiltered = filter (\x -> x /= "." && x /= "..") contentsUnfiltered
        contentsAbs = (combine path) <$> contentsFiltered
    foldM recursiveDescent' visitedWithCurrent contentsAbs


diffChangedFiles :: M.Map FilePath FileInfo 
             -> M.Map FilePath FileInfo
             -> [FileInfo]
diffChangedFiles before after =
  catMaybes . M.elems $ M.intersectionWith f before after
  where
    f beforeInfo afterInfo =
      if fileInfoTimestamp beforeInfo < fileInfoTimestamp afterInfo
      then Just afterInfo
      else Nothing

diffNewFiles :: M.Map FilePath FileInfo
         -> M.Map FilePath FileInfo
         -> [FileInfo]
diffNewFiles before after = M.elems $ M.difference after before

diffDeletedFiles :: M.Map FilePath FileInfo
             -> M.Map FilePath FileInfo
             -> [FileInfo]
diffDeletedFiles before after = M.elems $ M.difference before after

startWatchThread :: FilePath
                 -> (FilePath -> Reactive ()) -- ^ Push new files / dirs
                 -> (FilePath -> Reactive ()) -- ^ Push deleted files / dirs
                 -> (FileInfo -> Reactive ()) -- ^ Push changed files / dirs
                 -> Int -- ^ Seconds between polls
                 -> IO ThreadId
startWatchThread path pushNew pushDeleted pushChanged secs = do
  curr <- recursiveDescent path
  forkIO $ go curr
  where
    go last = do
      threadDelay $ secs * 1000 * 1000
      curr <- recursiveDescent path
      sync $ mapM_ (pushChanged) (diffChangedFiles last curr)
      sync $ mapM_ (pushNew    ) (fileInfoPath <$> diffNewFiles last curr)
      sync $ mapM_ (pushDeleted) (fileInfoPath <$> diffDeletedFiles last curr)
      go curr
