{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE ViewPatterns            #-}
{-# LANGUAGE ScopedTypeVariables     #-}
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
import           Control.Monad.Trans.Writer
import qualified Data.ByteString.Char8 as BS
import qualified Data.Configurator as C
import           Data.List
import           Data.Maybe
import           Data.String
import           Snap.Core
import           Snap.Snaplet
import           Snap.Util.FileServe
import           System.Directory
import           System.FilePath

import           Paths_snaplet_fay
import           Snap.Snaplet.Fay.Internal

methodFromString :: String -> Maybe CompileMethod
methodFromString "CompileOnDemand" = Just CompileOnDemand
methodFromString "CompileAll" = Just CompileAll
methodFromString _ = Nothing

-- | Snaplet initialization
initFay :: SnapletInit b Fay
initFay = makeSnaplet "fay" description datadir $ do
  config <- getSnapletUserConfig
  fp <- getSnapletFilePath

  (opts, errs) <- runWriterT $ do
    srcDir           <- logErr "Must specify srcDir" $ C.lookup config "srcDir"
    compileMethodStr <- logErr "Must specify compileMethod" $ C.lookup config "compileMethod"
    compileMethod    <- case compileMethodStr of
                        Just x -> logErr "Invalid compileMethod" . return $ methodFromString x
                        Nothing -> return Nothing
    verbose          <- logErr "Must specify verbose" $ C.lookup config "verbose"

    return (srcDir, verbose, compileMethod)

  let fay = case opts of
              (Just srcDir, Just verbose, Just compileMethod) -> Fay srcDir fp [srcDir] verbose compileMethod
              _ -> error $ intercalate "\n" errs

  liftIO $ do
    dirExists <-doesDirectoryExist fp
    -- Create the snaplet directory
    unless dirExists . liftIO $ createDirectory fp

  return fay

  where
    datadir = Just $ liftM (++ "/resources") getDataDir

    description = "fay fay fay"

    logErr :: MonadIO m => t -> IO (Maybe a) -> WriterT [t] m (Maybe a)
    logErr err m = do
        res <- liftIO m
        when (isNothing res) (tell [err])
        return res

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
