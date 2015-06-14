{-# LANGUAGE RankNTypes #-}
module System.Delta ( module System.Delta.Base
                    , module System.Delta.Poll
                    , module System.Delta.Class
                    , module System.Delta.Callback
                    , deltaDir
                    )where

import System.Delta.Base
import System.Delta.Poll
import System.Delta.Class
import System.Delta.Callback

-- | Build a file watcher
deltaDir :: (FileWatcher a) => FilePath -> IO a
deltaDir path = defaultWatcher path >>= return . watcherId

-- | Build a file watcher that allows to register callbacks
deltaDirWithCallbacks :: (FileWatcher a) => FilePath -> IO (CallbackWatcher a)
deltaDirWithCallbacks path = deltaDir path >>= withCallbacks

watcherId :: (FileWatcher a) => a -> a
watcherId a = a
