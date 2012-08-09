module Main where

import           Control.Applicative
import           Control.Monad
import           System.Directory

import           Fay.Mover
import           Fay.Mover.Util

config :: Config
config = Config {
      srcDir = "/Users/adam/repos/fay/examples"
    , destDir = "/Users/adam/repos/fay-mover/test-dest"
    }

main :: IO ()
main = do
  mapM removeFile =<< (extFiles "js" . destDir) config
  buildFay config
  len <- length <$> extFiles "js" (destDir config)
  putStrLn $ if len == 0
    then "Test Failed, destination folder is empty"
    else "Test OK"
