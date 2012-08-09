module Main where

-- | The tests pass if the program produces no output

import           Control.Applicative
import           Control.Monad
import           System.Directory
import           System.FilePath

import           Snap.Snaplet.Fay
import           Snap.Snaplet.Fay.Internal


config :: Fay
config = Fay {
      srcDir = "test-files"
    , destDir = "test-dest"
    , includeDirs = ["test-files"]
    , verbose = False
    }


assertM :: String -> IO Bool -> IO ()
assertM s f = f >>= \b -> unless b (putStrLn s)


assert :: String -> Bool -> IO ()
assert _ True = return ()
assert s False = putStrLn s


touch :: FilePath -> IO ()
touch fp = writeFile fp "main = return ()"


rmf :: FilePath -> IO ()
rmf fp = doesFileExist fp >>= (`when` removeFile fp)


main :: IO ()
main = do
  mapM_ removeFile =<< (extFiles "js" . destDir) config
  buildFay config
  len <- length <$> extFiles "js" (destDir config)
  assert "0" (len > 0)

  rmf (srcDir config </> "NewFile.hs")
  rmf (destDir config </> "NewFile.js")

  assertM "1" $ not <$> doesFileExist (destDir config </> "NewFile.js")
  touch $ srcDir config </> "NewFile.hs"

  buildFay config
  assertM "2" $ doesFileExist (destDir config </> "NewFile.js")
  removeFile $ srcDir config </> "NewFile.hs"

  buildFay config
  assertM "3" $ not <$> doesFileExist (destDir config </> "NewFile.js")
