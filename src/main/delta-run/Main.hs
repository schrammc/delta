{-# LANGUAGE RecursiveDo #-}
module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad

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
  input <- execParser opts
  inputDirExists <- doesDirectoryExist $ inputDir input

  when (not inputDirExists)
       (do
           hPutStrLn stderr $ inputDir input  ++ " doesn't exist"
           exitFailure
       )

  bracket (do
              when (inputVerbose input) $ putStrLn "Started watching."
              deltaDir $ inputDir input
          )
          (\watcher -> do
            when (inputVerbose input) $ putStrLn "Closing watcher."
            cleanUpAndClose watcher -- Will run after Ctrl-C
          )
          (\watcher -> do
              let mergedEvent = (newFiles     watcher) `merge`
                                (deletedFiles watcher) `merge`
                                (changedFiles watcher)

              ticker <- periodical (1000 * (inputSeconds input)) ()

              runE <- sync $ mkRunEvent
                               mergedEvent
                               (tickerEvent ticker)
                               (runCmd input)
                               
              _ <- sync $  listen runE (id)

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
                               <> value 1
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
              
            
mkRunEvent :: Event FilePath  -- ^ Changed / Deleted / New file event
           -> Event ()        -- ^ The periodical event
           -> IO ()           -- ^ Execute the input command
           -> Reactive (Event (IO ()))
mkRunEvent change ticker run = do
  rec
    enabled <- hold True $ (const False <$> fire  ) `merge` (const True <$> ticker)
    fireDue <- hold False $ (const True <$> change) `merge` (const False <$> fire )
    let firable = (&&) <$> enabled <*> fireDue

    -- Only when we have become firable after not being firable
    fire <- filterE (id) <$> ( collectE (\c -> \p ->  ((c /= p) && c,c))
                               True
                               (updates firable)
                             )

  return (const run <$> fire)
  
