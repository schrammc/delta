{-# LANGUAGE GADTs #-}
module System.Delta.Callback ( CallbackWatcher
                             , CallbackId
                               
                             -- * Construction
                             , withCallbacks
                               
                             -- * Adding callbacks
                             , withDeleteCallback
                             , withChangedCallback
                             , withNewCallback

                             -- * Removing callbacks
                             , unregisterCallback
                             , removeAllCallbacks

                             -- * Closing the watcher
                             , closeCallbackWatcher
                             )where

import FRP.Sodium
import System.Delta.Base
import System.Delta.Class

import qualified Data.Map as M

import Control.Concurrent.MVar

-- | Id of a callback in a 'CallbackWatcher'
newtype CallbackId = CallbackId Integer
                   deriving (Eq, Ord)  

-- | Provides a callback based interface to an FRP base 'FileWatcher'
data CallbackWatcher where
  CallbackWatcher :: {
    baseWatcher :: FileWatcher
  , nextCallbackId :: MVar CallbackId
  , watcherCallbacks :: MVar (M.Map CallbackId (IO ()))
  } -> CallbackWatcher


-- | Raise the callback id of a callback watcher
raiseId :: CallbackWatcher -> IO (CallbackId)
raiseId w = do
  (CallbackId n) <- takeMVar $ nextCallbackId w
  putMVar (nextCallbackId w) (CallbackId $ n+1)
  return (CallbackId n)

-- | Add an action to unregister a callback
addCallbackUnregister :: CallbackWatcher -> IO () -> IO CallbackId
addCallbackUnregister w removeCallback = do
  newId <- raiseId w
  mp <- takeMVar $ watcherCallbacks w
  putMVar (watcherCallbacks w) (M.insert newId removeCallback mp)
  return newId
  
-- | Wrap a file watcher in a datatype that allows adding callbacks
withCallbacks :: FileWatcher -> IO CallbackWatcher
withCallbacks w = do
  nextIdVar <- newMVar (CallbackId 0)
  callbacks <- newMVar (M.empty)
  return $ CallbackWatcher w nextIdVar callbacks

-- | Add a callback that is executed when file deletion is detected
withDeleteCallback :: CallbackWatcher
                   -> (FilePath -> IO ()) -- ^ An IO action on the deleted path
                   -> IO (CallbackId)
withDeleteCallback watcher action = do
  unregisterCallback <- callbackOnEvent (deletedFiles $ baseWatcher watcher) action
  addCallbackUnregister watcher unregisterCallback

-- | Add a callback that is executed when file creation is detected
withNewCallback :: CallbackWatcher
                -> (FilePath -> IO ()) -- ^ An IO action on the new path
                -> IO (CallbackId)
withNewCallback watcher action = do
  unregisterCallback <- callbackOnEvent (newFiles $ baseWatcher watcher) action
  addCallbackUnregister watcher unregisterCallback

-- | Add a callback on a changed file
withChangedCallback :: CallbackWatcher
                    -> (FilePath -> IO ()) -- ^ Action on changed file
                    -> IO (CallbackId)
withChangedCallback watcher action = do
  unregisterCallback <- callbackOnEvent (changedFiles $ baseWatcher watcher) action
  addCallbackUnregister watcher unregisterCallback

-- | Unregister the given CallbackId from the FileWatcher
-- does nothing if the CallbackId is not in the watcher
unregisterCallback :: CallbackWatcher -> CallbackId -> IO ()
unregisterCallback watcher cId = do
  mp <- takeMVar $ watcherCallbacks watcher
  case M.lookup cId mp of
    Nothing -> return ()
    Just action -> action
  putMVar (watcherCallbacks watcher) (M.delete cId mp)

-- | Remove all callbacks form the watcher. They will not be called after this
removeAllCallbacks :: CallbackWatcher -> IO ()
removeAllCallbacks watcher = do
  mp <- takeMVar $ watcherCallbacks watcher
  putMVar (watcherCallbacks watcher) M.empty
  sequence_ (M.elems mp)

-- | Remove all callbacks and close the underlying FileWatcher
closeCallbackWatcher :: CallbackWatcher -> IO ()
closeCallbackWatcher watcher = do
  removeAllCallbacks watcher
  cleanUpAndClose $ baseWatcher watcher
  

-- | Add a listener to an event, return the action to unregister the listener
callbackOnEvent :: Event a -> (a -> IO ()) -> IO (IO ())
callbackOnEvent e action = sync $ listen e action
