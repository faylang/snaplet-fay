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

head :: [a] -> a
head (x:_) = x

getBody :: Fay Element
getBody = firstByTag "body"

getWindow :: Fay Global
getWindow = ffi "window"

getDocument :: Fay Document
getDocument = ffi "document"

firstByTag :: String -> Fay Element
firstByTag tag = byTag tag >>= (return . head)

byTag :: String -> Fay [Element]
byTag = ffi "document.getElementsByTagName(%1)"

byId :: String -> Fay Element
byId = ffi "document.getElementById(%1)"

addEvent :: Foreign f => f -> String -> (Event -> Fay ()) -> Fay ()
addEvent = ffi "%1.addEventListener(%2,%3)"

addOnload :: Foreign f => Fay f -> Fay ()
addOnload = ffi "window.addEventListener(\"load\", %1)"

stopProp :: Event -> Fay ()
stopProp = ffi "%1.stopPropagation()"

preventDefault :: Event -> Fay ()
preventDefault = ffi "%1.preventDefault()"

createElement :: String -> Fay Element
createElement = ffi "document.createElement(%1)"

setInnerHtml :: Element -> String -> Fay ()
setInnerHtml = ffi "%1.innerHTML = %2"

appendChild :: Element -> Element -> Fay ()
appendChild = ffi "%1.appendChild(%2)"

printS :: String -> Fay ()
printS = ffi "console.log(%1)"

print :: Foreign f => f -> Fay ()
print = ffi "console.log(%1)"

