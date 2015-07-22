--------------------------------------------------------------------------------
-- |
-- Module: System.Delta.FSEvents
--
-- Uses the FSEvents API on MacOS to detect file changes.

--------------------------------------------------------------------------------
module System.Delta.FSEvents ( createFSEventsWatcher
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

createFSEventsWatcher path = do
    (changedEvent, pushChanged) <- sync $ Sodium.newEvent
    (deletedEvent, pushDeleted) <- sync $ Sodium.newEvent
    (newFileEvent, pushNewFile) <- sync $ Sodium.newEvent
    let callback = \e ->
                    when (itemIsFile e) $ do
                      when (itemIsCreated e) (sync $ pushNewFile $ eventPath e)
                      when (itemIsRemoved e) (sync $ pushDeleted $ eventPath e)
                      when (itemIsChanged e) (sync $ pushChanged $ eventPath e)
    evStream <- eventStreamCreate [path] 1 False False True callback
    return $ FileWatcher
               newFileEvent
               deletedEvent
               changedEvent
               (eventStreamDestroy evStream)
