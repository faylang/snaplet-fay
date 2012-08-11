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

-- | Compile a single file, print errors if they occur and return the
-- | compiled source if successful.
compileFile :: Fay -> FilePath -> IO (Maybe String)
compileFile config f = do
  exists <- doesFileExist f
  if not exists
    then do
      putStrLn $ "snaplet-fay: Could not find: " ++ hsRelativePath f
      return Nothing
    else do
      res <- F.compileFile (def { F.configDirectoryIncludes = includeDirs config }) True f
      case res of
        Right out -> do
          verbosePut config $ "Compiled " ++ hsRelativePath f
          writeFile (jsPath config f) out
          return $ Just out
        Left err -> do
          putStrLn $ "snaplet-fay: Error compiling " ++ hsRelativePath f ++ ":"
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
-- All files are checked each request.
--
-- NOTE:
--
-- Currently import dependencies are not handled, if a dependency has
-- changed the dependet will not be recompiled
compileAll :: Fay -> IO ()
compileAll config = do
  -- Fetch all hs files that don't have a corresponding js
  -- file or has been updated since the js file was last compiled.
  files <- filterM (shouldCompile config) =<< extFiles "hs" (srcDir config)

  -- Compile.
  forM_ files $ compileFile config

  -- Remove js files that don't have a corresponding source hs file.
  oldFiles <- extFiles "js" (destDir config) >>= filterM (liftM not . doesFileExist . hsPath config)
  forM_ oldFiles $ \f -> do
    removeFile f
    verbosePut config $ "Removed orphaned " ++ jsRelativePath f

  where
    -- Convert back and forth between the filepaths of hs and js files.


-- | Helpers

-- | Checks if a string ends with another string.
hasSuffix :: String -> String -> Bool
hasSuffix s suffix = reverse suffix == take (length suffix) (reverse s)

-- | Extract the filename from a filepath.
filename :: FilePath -> FilePath
filename = reverse . takeWhile (/= '/') . reverse

-- | Convert a JS filename to a Haskell filename.
toHsName :: String -> String
toHsName x = case reverse x of
               ('s':'j':'.': (reverse -> file)) -> file ++ ".hs"
               _ -> x

-- | Gets the filepath of the files with the given file extension in a folder.
extFiles :: String -> FilePath -> IO [FilePath]
extFiles ext dir = map (dir </>) . filter (`hasSuffix` ('.' : ext)) <$> getDirectoryContents dir

-- | Convert from the location of a js file to the location of its source hs file.
jsPath :: Fay -> FilePath -> FilePath
jsPath config f = destDir config </> filename (F.toJsName f)

-- | Convert from the location of a hs file to the location of the destination js file.
hsPath :: Fay -> FilePath -> FilePath
hsPath config f = srcDir config </> filename (toHsName f)

-- | Get the relative path of a js file.
jsRelativePath :: FilePath -> FilePath
jsRelativePath f = "snaplets/fay/js" </> filename f

-- | Get the relative path of a hs file.
hsRelativePath :: FilePath -> FilePath
hsRelativePath f = "snaplets/fay/src" </> filename f

-- | Helper for printing messages when the verbose flag is set
verbosePut :: Fay -> String -> IO ()
verbosePut config = when (verbose config) . putStrLn . ("snaplet-fay: " ++ )
