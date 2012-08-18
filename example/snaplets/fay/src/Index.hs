{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing #-}

module Index where

import           Language.Fay.FFI
import           Language.Fay.Prelude

-- | Time is shared between Snap and Fay
-- | Location: src/Application/SharedTypes.hs
-- import Time
import           Application.SharedTypes
-- | Dom is a Fay only module
-- | Location: snaplets/fay/src
import           Dom

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

currentTime :: Fay ()
currentTime = do
  ajaxJson "/ajax/current-time" $ \(CTR time) -> do
    el <- byId "current-time"
    setInnerHtml el time

ajaxJson :: Foreign a => String -> (a -> Fay ()) -> Fay ()
ajaxJson = ffi "jQuery.ajax(%1, { success : %2 })"
