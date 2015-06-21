{-# LANGUAGE RankNTypes #-}
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
-- Currently this library polls the directories of interest recursively in certain
-- intervals, but I will add OS-specific functionality to monitor the filesystem.
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
                    , module System.Delta.Class
                    , FileWatcher(..)

                    -- * Callback based interface
                    , module System.Delta.Callback
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
import System.Delta.Poll
import System.Delta.Class
import System.Delta.Callback

-- | Build a file watcher, this method will change later
deltaDir :: FilePath -> IO PollWatcher
deltaDir path = defaultWatcher path

-- | Build a file watcher that allows to register callbacks
deltaDirWithCallbacks :: FilePath -> IO (CallbackWatcher PollWatcher)
deltaDirWithCallbacks path = deltaDir path >>= withCallbacks
