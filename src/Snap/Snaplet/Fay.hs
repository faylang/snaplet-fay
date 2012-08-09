{-# LANGUAGE OverloadedStrings       #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

module Snap.Snaplet.Fay (
         Fay
       , initFay
       , fayServe
       ) where


import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State.Class
import           Snap.Snaplet
import           Snap.Util.FileServe
import           System.Directory

import           Snap.Snaplet.Fay.Internal

-- | Snaplet initialization

initFay :: FilePath -- ^ The location of the Fay source files
        -> Bool     -- ^ Print information when compiling or deleting files
        -> SnapletInit b Fay
initFay srcDir verbose =
    makeSnaplet "fay"
                "Fay integration that provides automatic (re)compilation during development"
                Nothing $
                 do
                   fp <- getSnapletFilePath
                   dirExists <- liftIO $ doesDirectoryExist fp
                   -- Create the snaplet directory
                   unless dirExists . liftIO $ createDirectory fp
                   return $ Fay srcDir fp [srcDir] verbose


-- | Serves the compiled Fay scripts

fayServe :: Handler b Fay ()
fayServe = do
  s <- getSnapletFilePath
  cfg <- get
  liftIO (buildFay cfg)
  serveDirectory s
