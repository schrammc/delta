{-# LANGUAGE RankNTypes #-}
module System.Delta ( module System.Delta.Base
                    , module System.Delta.Poll
                    , module System.Delta.Class
                    , module System.Delta.Callback
                    , deltaDir
                    , deltaDirWithCallbacks
                    )where

import System.Delta.Base
import System.Delta.Poll
import System.Delta.Class
import System.Delta.Callback

-- | Build a file watcher
deltaDir :: FilePath -> IO PollWatcher
deltaDir path = defaultWatcher path

-- | Build a file watcher that allows to register callbacks
deltaDirWithCallbacks :: FilePath -> IO (CallbackWatcher PollWatcher)
deltaDirWithCallbacks path = deltaDir path >>= withCallbacks
