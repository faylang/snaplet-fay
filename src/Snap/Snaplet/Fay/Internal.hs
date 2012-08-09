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
  }


-- | Checks the specified source folder and compiles all new and modified scripts.
-- Also removes any js files whose Fay source has been deleted.
-- At the moment all files are checked each request. This will change.

buildFay :: Fay -> IO ()
buildFay config = do
  -- Compile/recompile all hs files that don't have a corresponding js
  -- file or has been updated since the js file was last compiled.
  files <- do
    fs <- extFiles "hs" (srcDir config)
    flip filterM fs $ \f -> do
      jsExists <- doesFileExist (jsPath f)
      if not jsExists
      then return True
      else do
        hsmod <- getModificationTime f
        jsmod <- getModificationTime (jsPath f)
        return $ hsmod > jsmod

  forM_ files $ \f -> do
    when (verbose config) $ putStrLn ("compiling " ++ filename f)
    F.compileFromTo (def { F.configDirectoryIncludes = includeDirs config }) True f (jsPath f)

  -- Remove js files that don't have a corresponding source hs file
  oldFiles <- extFiles "js" (destDir config) >>= filterM (liftM not . doesFileExist . hsPath)
  forM_ oldFiles $ \f -> removeFile f >> when (verbose config) (putStrLn $ "Removed " ++ f)

  where
    -- Convert back and forth between the filepaths of hs and js files
    jsPath f = destDir config </> filename (F.toJsName f)
    hsPath f = srcDir config </> filename (toHsName f)


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
