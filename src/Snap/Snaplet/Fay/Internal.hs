{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Snap.Snaplet.Fay.Internal where

import           Control.Applicative
import           Control.Monad
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Default
import qualified Fay as F
import qualified Fay.Compiler.Config as F
import           System.Directory
import           System.FilePath


-- | Configuration
data Fay = Fay {
    snapletFilePath :: FilePath
  , verbose         :: Bool
  , compileMode     :: CompileMode
  , prettyPrint     :: Bool
  , _includeDirs    :: [FilePath]
  , packages        :: [String]
  } deriving (Show)

-- | Location of .hs files
srcDir :: Fay -> FilePath
srcDir = (</> "src") . snapletFilePath

-- | Location of .js files
destDir :: Fay -> FilePath
destDir = (</> "js") . snapletFilePath

-- | Where to check for imports
includeDirs :: Fay -> [FilePath]
includeDirs config = srcDir config : _includeDirs config

-- | Compile on every request or when Snap starts.
data CompileMode = Development | Production
  deriving (Eq, Show)

-- | Used by callers of compileFile to get the status of compilation.
data CompileResult = Success String | NotFound | Error String

-- | Compile a single file, print errors if they occur and return the
-- | compiled source if successful.
compileFile :: Fay -> FilePath -> IO CompileResult
compileFile config f = do
  exists <- doesFileExist f
  if not exists
    then do
      putStrLn $ "snaplet-fay: Could not find: " ++ hsRelativePath f
      return NotFound
    else do
      f' <- canonicalizePath f
      res <- flip F.compileFile f' $ F.addConfigPackages (packages config) $
                                      F.addConfigDirectoryIncludePaths (includeDirs config) $
                                        def { F.configPrettyPrint = prettyPrint config
                                            , F.configFilePath = Just f'
                                            }
      case res of
        Right out -> do
          verbosePut config $ "Compiled " ++ hsRelativePath f
          writeFile (jsPath config f) out
          return $ Success out
        Left err -> do
          let errString = C.unpack . A.encode $ "snaplet-fay: Error compiling " ++ hsRelativePath f ++ ":\n" ++ F.showCompileError err
          putStrLn errString
          -- return Success so the browser will treat this as a normal JavaScript file.
          -- As of writing this, this means that Error is not used.
          return $ Error $ "console.error(" ++ errString ++ ");"

-- | Checks the specified source folder and compiles all new and modified scripts.
-- Also removes any js files whose Fay source has been deleted.
-- All files are checked each request.
compileAll :: Fay -> IO ()
compileAll config = do
  -- Find files, only checks the root snaplet directory
  -- TODO check subdirs.
  files <- extFiles "hs" (srcDir config)

  -- Compile.
  forM_ files $ compileFile config

  -- Remove js files that don't have a corresponding source hs file.
  oldFiles <- extFiles "js" (destDir config) >>= filterM (liftM not . doesFileExist . hsPath config)
  forM_ oldFiles $ \f -> do
    removeFile f
    verbosePut config $ "Removed orphaned " ++ jsRelativePath f


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

-- | Relative path to a js file.
jsRelativePath :: FilePath -> FilePath
jsRelativePath f = "snaplets/fay/js" </> filename f

-- | Relative path to a hs file.
hsRelativePath :: FilePath -> FilePath
hsRelativePath f = "snaplets/fay/src" </> filename f

-- | Print log messages when the verbose flag is set
verbosePut :: Fay -> String -> IO ()
verbosePut config = when (verbose config) . putStrLn . ("snaplet-fay: " ++ )
