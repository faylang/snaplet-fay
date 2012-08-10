{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE ViewPatterns            #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

module Snap.Snaplet.Fay (
         CompileMethod (..)
       , Fay
       , initFay
       , fayServe
       ) where


import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State.Class
import           Data.String
import qualified Data.ByteString.Char8 as BS
import           Snap.Snaplet
import           Snap.Core
import           Snap.Util.FileServe
import           System.Directory
import           System.FilePath

import           Snap.Snaplet.Fay.Internal

-- | Snaplet initialization

initFay :: FilePath      -- ^ The location of the Fay source files
        -> Bool          -- ^ Print information when compiling or deleting files
        -> CompileMethod -- ^ CompileOnDemand: Compile the requested file only
                         --   CompileAll: Compile all modified files on every request and
                         --     delete orphaned js files
        -> SnapletInit b Fay
initFay srcDir verbose compileMethod =
    makeSnaplet "fay"
                "Fay integration that provides automatic (re)compilation during development"
                Nothing $
                 do
                   fp <- getSnapletFilePath
                   dirExists <- liftIO $ doesDirectoryExist fp
                   -- Create the snaplet directory
                   unless dirExists . liftIO $ createDirectory fp
                   return $ Fay srcDir fp [srcDir] verbose compileMethod


-- | Serves the compiled Fay scripts

fayServe :: Handler b Fay ()
fayServe = do
  cfg <- get
  compileWithMethod (compileMethod cfg)

compileWithMethod :: CompileMethod -> Handler b Fay ()
compileWithMethod CompileOnDemand = do
  cfg <- get
  req <- getRequest
  let uri = srcDir cfg </> (toHsName . filename . BS.unpack . rqURI) req
  res <- liftIO (compileFile cfg uri)
  case res of
    Just s -> writeLBS $ fromString s
    Nothing -> return ()

compileWithMethod CompileAll = do
   s <- getSnapletFilePath
   cfg <- get
   liftIO (compileAll cfg)
   serveDirectory s
