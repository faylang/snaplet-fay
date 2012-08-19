{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Dom where

import           Language.Fay.FFI
import           Language.Fay.Prelude


data Element
instance Foreign Element
data Event
instance Foreign Event
data Global
instance Foreign Global
data Document
instance Foreign Document

instance Foreign Integer

head :: [a] -> a
head (x:_) = x

getWindow :: Fay Global
getWindow = ffi "window"

getDocument :: Fay Document
getDocument = ffi "document"

addOnload :: Foreign f => Fay f -> Fay ()
addOnload = ffi "window.addEventListener(\"load\", %1)"

stopProp :: Event -> Fay ()
stopProp = ffi "%1.stopPropagation()"

preventDefault :: Event -> Fay ()
preventDefault = ffi "%1.preventDefault()"

createElement :: String -> Fay Element
createElement = ffi "document.createElement(%1)"

printS :: String -> Fay ()
printS = ffi "console.log(%1)"

print :: Foreign f => f -> Fay ()
print = ffi "console.log(%1)"

setTimeout :: Integer -> Fay () -> Fay Integer
setTimeout = ffi "setInterval(%2, %1)"
