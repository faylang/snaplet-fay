{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

module Snap.Snaplet.Fay (
         Fay
       , initFay
       , fayServe
       ) where

import           Control.Applicative
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
    compileMethodStr <- logErr "Must specify compileMethod" $ C.lookup config "compileMethod"
    compileMethod    <- case compileMethodStr of
                        Just x -> logErr "Invalid compileMethod" . return $ methodFromString x
                        Nothing -> return Nothing
    verbose          <- logErr "Must specify verbose" $ C.lookup config "verbose"
    prettyPrint      <- logErr "Must specify prettyPrint" $ C.lookup config "prettyPrint"
    includeDirs      <- logErr "Must specify includeDirs" $ C.lookup config "includeDirs"
    let inc = maybe [] (split ',') includeDirs
    inc' <- liftIO $ mapM canonicalizePath inc
    return (verbose, compileMethod, prettyPrint, inc')

  let fay = case opts of
              (Just verbose, Just compileMethod, Just prettyPrint, includeDirs) ->
                Fay fp verbose compileMethod prettyPrint (fp : includeDirs)
              _ -> error $ intercalate "\n" errs

  -- Make sure snaplet/fay, snaplet/fay/src, snaplet/fay/js are present.
  liftIO $ mapM_ createDirUnlessExists [fp, srcDir fay, destDir fay]

  return fay

  where
    -- TODO Use split package
    split :: Eq a => a -> [a] -> [[a]]
    split _ [] = []
    split a as = takeWhile (/= a) as : split a (drop 1 $ dropWhile (/= a) as)

    createDirUnlessExists fp = do
      dirExists <- doesDirectoryExist fp
      unless dirExists $ createDirectory fp

    datadir = Just $ liftM (++ "/resources") getDataDir

    description = "Automatic (re)compilation and serving of Fay files"

    logErr :: MonadIO m => t -> IO (Maybe a) -> WriterT [t] m (Maybe a)
    logErr err m = do
        res <- liftIO m
        when (isNothing res) (tell [err])
        return res

-- | Serves the compiled Fay scripts using the chosen compile method.
fayServe :: Handler b Fay ()
fayServe = do
  cfg <- get
  compileWithMethod (compileMethod cfg)

compileWithMethod :: CompileMethod -> Handler b Fay ()
compileWithMethod CompileOnDemand = do
  cfg <- get
  uri <- (srcDir cfg </>) . toHsName . filename . BS.unpack . rqURI <$> getRequest
  res <- liftIO (compileFile cfg uri)
  case res of
    Success s -> writeLBS $ fromString s
    NotFound -> do
      modifyResponse $ setResponseStatus 404 "Not Found"
      writeBS "File not found."
      finishWith =<< getResponse
    Error err -> do
      modifyResponse $ setResponseStatus 500 "Internal Server Error"
      writeBS . BS.pack $ err
      finishWith =<< getResponse

compileWithMethod CompileAll = do
  cfg <- get
  liftIO (compileAll cfg)
  serveDirectory (destDir cfg)
