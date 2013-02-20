{-# LANGUAGE EmptyDataDecls    #-}

module Dom where

import FFI
import Prelude


data Element
data Global
data Document

addOnload :: Fay f -> Fay ()
addOnload = ffi "window.addEventListener(\"load\", %1)"

createElement :: String -> Fay Element
createElement = ffi "document.createElement(%1)"

setInterval :: Double -> Fay () -> Fay Double
setInterval = ffi "setInterval(%2, %1)"
