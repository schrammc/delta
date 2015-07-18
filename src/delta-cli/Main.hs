module Main where

import System.Delta
import System.Delta.FSEvents
import System.Environment

import Control.Monad
import Control.Concurrent

main = do
  args <- getArgs
  case args of
    [path] -> do
                fw <- defaultWatcher path :: IO FSEventsWatcher
                watcher <- withCallbacks fw
                withNewCallback watcher (\x -> putStrLn $ "new:\t" ++ x)
                withDeleteCallback watcher (\x -> putStrLn $ "del:\t" ++ x)
                withChangedCallback watcher (\x ->
                                              putStrLn $ "changed:\t" ++ x
                                            )
                forever $ threadDelay (1000 * 1000)
    _      -> putStrLn errorString

errorString = "This is a simple command line interface to the delta\
              \library. Call the program with delta-cli <path>, where path\
              \is the path of the folder / file you want to watch."
