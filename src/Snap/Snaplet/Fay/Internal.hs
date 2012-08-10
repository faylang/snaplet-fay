{-# LANGUAGE ViewPatterns       #-}

module Snap.Snaplet.Fay.Internal where

import           Control.Applicative
import           Control.Monad
import           Data.Default
import qualified Language.Fay.Compiler as F
import qualified Language.Fay.Types as F
import           System.Directory
import           System.FilePath


-- | Configuration

data Fay = Fay {
    srcDir :: FilePath
  , destDir :: FilePath
  , includeDirs :: [FilePath]
  , verbose :: Bool
  , compileMethod :: CompileMethod
  }

data CompileMethod = CompileOnDemand | CompileAll

compileFile :: Fay -> FilePath -> IO (Maybe String)
compileFile config f = do
  res <- F.compileFile (def { F.configDirectoryIncludes = includeDirs config }) True f
  case res of
    Right out -> do
      when (verbose config) . putStrLn $ "snaplet-fay: Compiled " ++ f
      return $ Just out
    Left err -> do
      putStrLn $ "snaplet-fay: Error compiling " ++ f ++ ":"
      print err
      return Nothing

-- | Check if a file should be recompiled, either when the hs file was
-- | updated or the file hasn't been compiled at all.
shouldCompile :: Fay -> FilePath -> IO Bool
shouldCompile config hsFile = do
  jsExists <- doesFileExist (jsPath config hsFile)
  if not jsExists
    then return True
    else do
      hsmod <- getModificationTime hsFile
      jsmod <- getModificationTime (jsPath config hsFile)
      return $ hsmod > jsmod

-- | Checks the specified source folder and compiles all new and modified scripts.
-- Also removes any js files whose Fay source has been deleted.
-- At the moment all files are checked each request. This will change.

compileAll :: Fay -> IO ()
compileAll config = do
  -- Fetch all hs files that don't have a corresponding js
  -- file or has been updated since the js file was last compiled.
  files <- filterM (shouldCompile config) =<< extFiles "hs" (srcDir config)

  -- Compile
  forM_ files $ \f -> do
     res <- compileFile config f
     case res of
       Just s -> writeFile (jsPath config f) s
       Nothing -> return ()

  -- Remove js files that don't have a corresponding source hs file
  oldFiles <- extFiles "js" (destDir config) >>= filterM (liftM not . doesFileExist . hsPath config)
  forM_ oldFiles $ \f -> do
    removeFile f
    when (verbose config) . putStrLn $ "snaplet-fay: Removed orphaned " ++ f

  where
    -- Convert back and forth between the filepaths of hs and js files


-- | Helpers

-- | Checks if a string ends with another string
hasSuffix :: String -> String -> Bool
hasSuffix s suffix = reverse suffix == take (length suffix) (reverse s)

-- | Extract the filename from a filepath
filename :: FilePath -> FilePath
filename = reverse . takeWhile (/= '/') . reverse

-- | Convert a JS filename to a Haskell filename
toHsName :: String -> String
toHsName x = case reverse x of
               ('s':'j':'.': (reverse -> file)) -> file ++ ".hs"
               _ -> x

-- | Gets the filepath of the files with the given file extension in a folder
extFiles :: String -> FilePath -> IO [FilePath]
extFiles ext dir = map (dir </>) . filter (`hasSuffix` ('.' : ext)) <$> getDirectoryContents dir

-- | Convert back and forth between the locations of js and fay files
jsPath :: Fay -> FilePath -> FilePath
jsPath config f = destDir config </> filename (F.toJsName f)

hsPath :: Fay -> FilePath -> FilePath
hsPath config f = srcDir config </> filename (toHsName f)
