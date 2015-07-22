module Main where

import Control.Exception

import Test.Hspec

import System.Delta
import System.Directory
import System.FilePath

spec :: Spec
spec = do
    describe "The delta library" $ do
      it "should create a watcher without an exception" $ do

        bracket
          (do
            tempdir <- getTemporaryDirectory
            createDirectory $ tempdir </> "opdir"
            return $ tempdir </> "opdir"
          )
          removeDirectory
          (\path -> do
              watcher <- deltaDir path
              v <- cleanUpAndClose watcher
              v `shouldBe` ()
          )
        
        
main :: IO ()
main = hspec spec
