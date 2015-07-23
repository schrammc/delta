{-# LANGUAGE RecursiveDo #-}
module Main (main) where

import Control.Concurrent
import Control.Exception
import Control.Monad

import qualified FRP.Sodium (value)
import FRP.Sodium hiding (value)
import FRP.Sodium.IO
import Options.Applicative

import System.Exit
import System.Delta
import System.Delta.FRPUtils
import System.Directory
import System.IO
import System.Process

data Input = Input { inputSeconds :: Int
                   , inputVerbose :: Bool
                   , inputDir     :: FilePath
                   , inputCommand :: String
                   }
                   deriving Show
main = do

  -- Parse command line options
  input <- execParser opts
  inputDirExists <- doesDirectoryExist $ inputDir input

  -- Check for existence of input dir
  when (not inputDirExists)
       (do
           hPutStrLn stderr $ inputDir input  ++ " doesn't exist"
           exitFailure
       )

  -- Create a directory watcher, start running it and sleep until we get an
  -- exception or an interrupt. In both cases we close the watcher and exit.
  bracket (do
              when (inputVerbose input) $ putStrLn "Started watching."
              deltaDir $ inputDir input
          )
          (\watcher -> do
            when (inputVerbose input) $ putStrLn "Closing watcher."
            cleanUpAndClose watcher -- Will run after Ctrl-C
          )
          (\watcher -> do
              -- We execute the command on all possible incoming events
              let mergedEvent = (newFiles     watcher) `merge`
                                (deletedFiles watcher) `merge`
                                (changedFiles watcher)

              -- Start a process
              ticker <- if inputSeconds input > 0
                        then Just <$> periodical (1000 * (inputSeconds input)) ()
                        else return Nothing

              sync $ mkRunEvent (inputVerbose input)
                                mergedEvent
                                ticker
                                (runCmd input)

              -- Sleep till interrupted (or exception)
              forever $ threadDelay $ 50000000
          )
  where
    opts  = info (helper <*> optsP) (fullDesc)
    optsP = Input
            <$> option auto  ( long "interval"
                               <> short 'i'
                               <> metavar "INTERVAL"
                               <> help "Run at most every n seconds"
                               <> value 3
                             )
            <*> flag False True (long "verbose"
                                 <> short 'v'
                                 <> help "Print extra output"
                                 )
            <*> argument str ( metavar "FILE"
                               <> help "The directory that is watched"
                             )
            <*> argument str (  metavar "CMD"
                             <> help "The command to run"
                             )
    runCmd input = do
      when (inputVerbose input) $ putStrLn "Starting process."
      (_,_,_,procHandle) <-
        createProcess $ (shell $ inputCommand input){ std_in  = Inherit
                                                    , std_out = Inherit
                                                    , std_err = Inherit
                                                    }
      waitForProcess procHandle
      when (inputVerbose input) $ putStrLn "Process done"
      return ()
      
-- | Create an action that will run the given IO action when changes occur. The
-- given action will not be run twice inbetween two firings of the ticker event.
mkRunEvent :: Bool              -- ^ Verbose output
           -> Event FilePath    -- ^ Changed / Deleted / New file event
           -> Maybe (Ticker ()) -- ^ The periodical event
           -> IO ()             -- ^ Execute the input command
           -> Reactive ()
mkRunEvent verbose change tickerMaybe run =
  case tickerMaybe of
   Just ticker -> mdo
     let tickerE = tickerEvent ticker
     enabled <- hold True  $ (const False <$> fire) `merge` (const True <$> tickerE)

     let fire = gate change enabled

     listen fire runAction

     return ()
   Nothing -> listen change runAction >> return ()
  where
    runAction path = do
      when verbose $ putStrLn ("Running command after change in file " ++ path)
      run
