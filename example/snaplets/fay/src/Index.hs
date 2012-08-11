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
  return ()
