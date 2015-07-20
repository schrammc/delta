{-# LANGUAGE RankNTypes, CPP #-}
--------------------------------------------------------------------------------
-- |
-- Module : System.Delta
-- Copyright: (c) Christof Schramm 2015
-- License: LGPL v2
--
-- Maintainer: Christof Schramm
-- Stability: Experimental
--
-- = Description
--
-- An umberella package for the delta library. This library can be used to monitor
-- changed / new / deleted files in a given directory (or set of directories).
--
-- On non OS X systems this library polls the directories of interest
-- recursively in certain intervals, but I will add OS-specific functionality to
-- monitor the filesystem.
--
-- On OS X this library can use the File System Events API to detect changes
-- without recursive directory traversal.
--
-- = Examples
--
-- Create a watcher that prints a line if a new file is created in the monitored
-- directory:
--
-- @
-- printNewFilePaths basePath = do
--   watcher <- deltaDirWithCallbacks basePath
--   withNewCallback watcher (\\x -> putStrLn $ "new file: " ++ x)
-- @
--------------------------------------------------------------------------------
module System.Delta ( module System.Delta.Base
                    , module System.Delta.Poll

                    -- * Important functions
                    , deltaDir
                    , deltaDirWithCallbacks

                    -- * FRP based interface
                    , FileWatcher(..)

                    -- * Callback based interface
                    , CallbackWatcher
                    , CallbackId
                    , withCallbacks
                    -- ** Adding callbacks
                    , withDeleteCallback
                    , withChangedCallback
                    , withNewCallback
                    -- ** Removing callbacks
                    , unregisterCallback
                    , removeAllCallbacks
                    , closeCallbackWatcher                
                    )where

import System.Delta.Base
import System.Delta.Class
import System.Delta.Callback
import System.Delta.Poll
#if defined(__APPLE__)
import System.Delta.FSEvents
#endif

-- | Build a file watcher, the concrete implementation is operating system
-- dependent.
--
-- * The default uses polling ('createPollWatcher')
--
-- * The watcher for OS X uses the FS Events API 'createFSEventsWatcher'
deltaDir :: FilePath -> IO FileWatcher
deltaDir path = do
#if defined(__APPLE__)
  createFSEventsWatcher path
#else
  createPollWatcher 5 path
#endif

-- | Build a file watcher that allows to register callbacks
deltaDirWithCallbacks :: FilePath -> IO CallbackWatcher
deltaDirWithCallbacks path = deltaDir path >>= withCallbacks
