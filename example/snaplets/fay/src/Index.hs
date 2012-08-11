module Index where

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
  addEvent button "click" currentTime

  return ()

currentTime :: Fay ()
currentTime =
  ajaxJson "/ajax/current-time" handleResponse
    where
      handleResponse json = do
        ctr <- jsonToCTR json
        el <- byId "current-time"
        setInnerHtml el (time ctr)

data Json
instance Foreign Json

data CurrentTimeResponse = CTR { time :: String }
instance Foreign CurrentTimeResponse

jsonToCTR :: Json -> Fay CurrentTimeResponse
jsonToCTR json = do t <- attrS json "time"
                    return $ CTR t

ajaxJson :: String -> (Json -> Fay ()) -> Fay ()
ajaxJson = ffi "jQuery.ajax(%1, { success : %2 })"

attrS :: Foreign f => f -> String -> Fay String
attrS = ffi "%1[%2]"
