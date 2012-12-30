{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Dom where

import           Language.Fay.FFI
import           Language.Fay.Prelude


data Element
instance Foreign Element
data Global
instance Foreign Global
data Document
instance Foreign Document

addOnload :: Foreign f => Fay f -> Fay ()
addOnload = ffi "window.addEventListener(\"load\", %1)"

createElement :: String -> Fay Element
createElement = ffi "document.createElement(%1)"

setInterval :: Double -> Fay () -> Fay Double
setInterval = ffi "setInterval(%2, %1)"
