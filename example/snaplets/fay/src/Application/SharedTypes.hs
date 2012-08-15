{-# LANGUAGE NoImplicitPrelude #-}

module Application.SharedTypes where

import Language.Fay.Prelude
import Language.Fay.FFI

data CTR = CTR { time :: String }
  deriving (Show)
instance Foreign CTR
