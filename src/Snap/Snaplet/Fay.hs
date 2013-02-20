{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

module Snap.Snaplet.Fay (
         Fay
       , initFay
       , fayServe
       , fayax
       , toFayax
       , fromFayax
       ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State.Class
import           Control.Monad.Trans.Writer
import qualified Data.Aeson                 as A
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy       as BL
import qualified Data.Configurator          as C
import           Data.Data
import           Data.List
import           Data.Maybe
import           Data.String
import           Fay.Convert
import           Snap.Core
import           Snap.Snaplet
import           Snap.Util.FileServe
import           System.Directory
import           System.FilePath

import           Paths_snaplet_fay
import           Snap.Snaplet.Fay.Internal

-- | Snaplet initialization
initFay :: SnapletInit b Fay
initFay = makeSnaplet "fay" description datadir $ do
  config <- getSnapletUserConfig
  fp <- getSnapletFilePath

  (opts, errs) <- runWriterT $ do
    compileModeStr <- logErr "Must specify compileMode" $ C.lookup config "compileMode"
    compileMode    <- case compileModeStr of
                        Just x -> logErr "Invalid compileMode" . return $ compileModeFromString x
                        Nothing -> return Nothing
    verbose          <- logErr "Must specify verbose" $ C.lookup config "verbose"
    prettyPrint      <- logErr "Must specify prettyPrint" $ C.lookup config "prettyPrint"
    includeDirs      <- logErr "Must specify includeDirs" $ C.lookup config "includeDirs"
    let inc = maybe [] (split ',') includeDirs
    inc' <- liftIO $ mapM canonicalizePath inc
    packages         <- logErr "Must specify packages" $ C.lookup config "packages"
    let packs = maybe [] (split ',') packages
    return (verbose, compileMode, prettyPrint, inc', packs)

  let fay = case opts of
              (Just verbose, Just compileMode, Just prettyPrint, includeDirs, packages) ->
                Fay
                  { snapletFilePath = fp
                  , verbose         = verbose
                  , compileMode     = compileMode
                  , prettyPrint     = prettyPrint
                  , _includeDirs    = fp : includeDirs
                  , packages        = packages
                  }
              _ -> error $ intercalate "\n" errs

  -- Make sure snaplet/fay, snaplet/fay/src, snaplet/fay/js are present.
  liftIO $ mapM_ createDirUnlessExists [fp, srcDir fay, destDir fay]

  when (Production == compileMode fay) (liftIO $ compileAll fay)

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

    compileModeFromString :: String -> Maybe CompileMode
    compileModeFromString "Development" = Just Development
    compileModeFromString "Production" = Just Production
    compileModeFromString _ = Nothing

-- | Serves the compiled Fay scripts using the chosen compile mode.
fayServe :: Handler b Fay ()
fayServe = do
  modifyResponse . setContentType $ "text/javascript;charset=utf-8"
  get >>= compileWithMode . compileMode

-- | Send and receive JSON.
-- | Automatically decodes a JSON request into a Fay record which is
-- | passed to `g`. The handler `g` should then return a Fay record (of
-- | a possibly separate type) which is encoded and passed back as a
-- | JSON response.
-- | If you only want to send JSON and handle input manually, use toFayax.
-- | If you want to receive JSON and handle the response manually, use fromFayax
fayax :: (Data f1, Read f1, Show f2) => (f1 -> Handler h1 h2 f2) -> Handler h1 h2 ()
fayax g = do
  res <- decode
  case res of
    Left body -> send500 $ Just body
    Right res -> toFayax . g $ res

-- | fayax only sending JSON.
toFayax :: Show f2 => Handler h1 h2 f2 -> Handler h1 h2 ()
toFayax g = do
  modifyResponse . setContentType $ "text/json;charset=utf-8"
  writeLBS . A.encode . showToFay =<< g

-- | fayax only recieving JSON.
fromFayax :: (Data f1, Read f1) => (f1 -> Handler h1 h2 ()) -> Handler h1 h2 ()
fromFayax g = do
  res <- decode
  case res of
    Left body -> send500 $ Just body
    Right res -> g res

-- | Read the request input and convert to a Fay value.
decode :: (Data f1, Read f1) => Handler h1 h2 (Either ByteString f1)
decode = do
  body <- readRequestBody 1024 -- Nothing will break by abusing this :)!
  res <- return $ A.decode body >>= readFromFay
  return $ case res of
    Nothing -> Left. BS.concat . BL.toChunks $ "Could not decode " `BL.append` body
    Just x -> Right x

-- | Compiles according to the specified mode.
compileWithMode :: CompileMode -> Handler b Fay ()
compileWithMode Development = do
  cfg <- get
  uri <- (srcDir cfg </>) . toHsName . filename . BS.unpack . rqURI <$> getRequest
  res <- liftIO (compileFile cfg uri)
  case res of
    Success s -> writeBS $ fromString s
    NotFound -> send404 Nothing
    Error err -> send500 . Just . BS.pack $ err
-- Production compilation has already been done.
compileWithMode Production = get >>= serveDirectory . destDir

-- | Respond with Not Found
send404 :: Maybe ByteString -> Handler a b ()
send404 msg = do
  modifyResponse $ setResponseStatus 404 "Not Found"
  writeBS $ fromMaybe "Not Found" msg
  finishWith =<< getResponse

-- | Respond with Internal Server Error
send500 :: Maybe ByteString -> Handler a b ()
send500 msg = do
  modifyResponse $ setResponseStatus 500 "Internal Server Error"
  writeBS $ fromMaybe "Internal Server Error" msg
  finishWith =<< getResponse
