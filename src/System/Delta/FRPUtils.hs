module System.Delta.FRPUtils where

import Control.Concurrent
import Control.Monad

import FRP.Sodium

data Ticker a = Ticker{ tickerInterval  :: Int
                      , tickerEvent     :: Event a
                      , tickerTerminate :: IO ()
                      }

periodical :: Int -- ^ Milliseconds
           -> a   -- ^ Item that is sent periodically
           -> IO (Ticker a)
periodical ms v = do
  (e,push) <- sync $ newEvent
  tId <- forkIO . forever $ do
    threadDelay $ 1000 * ms
    sync $ push v    
  return $ Ticker ms e (killThread tId)
