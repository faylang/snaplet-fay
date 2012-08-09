{-# LANGUAGE ViewPatterns #-}

module Fay.Mover.Util where

import           Control.Applicative
import           Control.Monad
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process

-- | Generic Helpers

hasPrefix :: String -> String -> Bool
hasPrefix s prefix = prefix == take (length prefix) s

hasSuffix :: String -> String -> Bool
hasSuffix s suffix = reverse suffix == take (length suffix) (reverse s)

filename :: FilePath -> FilePath
filename = reverse . takeWhile (/= '/') . reverse

-- | Convert a Haskell filename to a JS filename.
toJsName :: String -> String
toJsName x = case reverse x of
               ('s':'h':'.': (reverse -> file)) -> file ++ ".js"
               _ -> x

-- | Gets all files with the given file extension in a folder.
extFiles :: String -> FilePath -> IO [FilePath]
extFiles ext dir = map (dir </>) . filter (`hasSuffix` ('.' : ext)) <$> getDirectoryContents dir
