{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing #-}

module Index where

import Language.Fay.FFI
import Language.Fay.Prelude

import Dom

main :: Fay ()
main = addOnload onload

onload :: Fay ()
onload = do
  contents <- byId "content"
  div <- createElement "div"
  setInnerHtml div "This element was created by Fay through an onload handler!"
  appendChild contents div

  currentTime
  button <- byId "current-time-button"
  addEvent button "click" (const currentTime)

  return ()

data CTR = CTR { time :: String }
instance Foreign CTR

currentTime :: Fay ()
currentTime = do
  ajaxJson "/ajax/current-time" handleResponse

handleResponse :: CTR -> Fay ()
handleResponse (CTR time) = do
  el <- byId "current-time"
  setInnerHtml el time

ajaxJson :: String -> (CTR -> Fay ()) -> Fay ()
ajaxJson = ffi "jQuery.ajax(%1, { success : %2 })"
