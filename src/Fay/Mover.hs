{-# LANGUAGE DeriveDataTypeable #-}

module Fay.Mover (
    Config (..)
  , buildFay
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Typeable
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process

import           Fay.Mover.Util

-- import qualified Language.Fay.Compiler as Fay

-- | Configuration

data Config = Config {
    srcDir :: FilePath
  , destDir :: FilePath
  , includeDirs :: [FilePath]
  } deriving Typeable

-- | Build

buildFay :: Config -> IO ()
buildFay config = do
  files <- extFiles "hs" (srcDir config)
  forM_ files $ \f -> do
             let dest = (destDir config </> filename (toJsName f))
             putStrLn $ "compiling " ++ filename f
             code <- system $ "fay -include=" ++ intercalate "," includeDirs ++ "-autorun \"" ++ f ++ "\""
             if (code == ExitSuccess) then do
               putStrLn $ "OK"
               renameFile (toJsName f) dest
             else
               putStrLn $ "FAIL"
