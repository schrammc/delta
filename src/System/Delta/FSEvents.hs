--------------------------------------------------------------------------------
-- |
-- Module: System.Delta.FSEvents
--
-- Uses the FSEvents API on MacOS to detect file changes.

--------------------------------------------------------------------------------
module System.Delta.FSEvents ( FSEventsWatcher
                             ) where

import Control.Monad

import Data.Bits
import Data.Word

import qualified FRP.Sodium as Sodium
import FRP.Sodium (sync,merge)

import System.Delta.Class
import System.OSX.FSEvents

-- | Helper Method 
(=<=) :: Event -> Word64 -> Bool
(Event{eventFlags=fls}) =<= flags = 0 < (fls .&. flags)

itemIsFile :: Event -> Bool
itemIsFile e = e =<= eventFlagItemIsFile

itemIsDir :: Event -> Bool
itemIsDir = (=<= eventFlagItemIsDir)

itemIsCreated :: Event -> Bool
itemIsCreated = (=<= eventFlagItemCreated)

itemIsRemoved :: Event -> Bool
itemIsRemoved = (=<= eventFlagItemRemoved)

itemIsChanged :: Event -> Bool
itemIsChanged = (=<= eventFlagItemModified)

data FSEventsWatcher = FSEventsWatcher
                         [(FilePath,EventStream)]
                         (Sodium.Event FilePath)
                         (Sodium.Event FilePath)
                         (Sodium.Event FilePath)

instance FileWatcher FSEventsWatcher where
  defaultWatcher path = do
    (changedEvent, pushChanged) <- sync $ Sodium.newEvent
    (deletedEvent, pushDeleted) <- sync $ Sodium.newEvent
    (newFileEvent, pushNewFile) <- sync $ Sodium.newEvent
    let callback :: Event -> IO()
        callback = \e ->
                    when (itemIsFile e) $ do
                      when (itemIsCreated e) (sync $ pushNewFile $ eventPath e)
                      when (itemIsRemoved e) (sync $ pushDeleted $ eventPath e)
                      when (itemIsChanged e) (sync $ pushChanged $ eventPath e)
    evStream <- eventStreamCreate [path] 1 False False True callback
    return $ FSEventsWatcher
               [(path,evStream)]
               changedEvent
               newFileEvent
               deletedEvent
  changedFiles (FSEventsWatcher _ e _ _) = e
  newFiles     (FSEventsWatcher _ _ e _) = e
  deletedFiles (FSEventsWatcher _ _ _ e) = e
  cleanUpAndClose (FSEventsWatcher lst _ _ _) =
    mapM_ (eventStreamDestroy) (snd <$> lst)
  mergeWatchers (FSEventsWatcher l1 ch1 new1 del1)
                (FSEventsWatcher l2 ch2 new2 del2) =
    FSEventsWatcher
      (l1 ++ l2)
      (ch1  `merge` ch2 )
      (new1 `merge` new2)
      (del1 `merge` del2)

