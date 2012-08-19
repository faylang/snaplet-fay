{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing #-}

module JQuery where

import           Language.Fay.FFI
import           Language.Fay.Prelude

data JQuery
instance Foreign JQuery

ajax :: String -> Fay () -> Fay ()
ajax = ffi "jQuery.ajax(%1, { success : %2 })"

ajaxJson :: Foreign f => String -> (f -> Fay ()) -> Fay ()
ajaxJson = ffi "jQuery.ajax(%1, { success : %2 })"

jPost :: (Foreign f, Foreign g) => String -> f -> (g -> Fay ()) -> Fay ()
jPost = ffi "jQuery.ajax(%1, { data: JSON.stringify(%2), type: 'POST', processData: false, contentType: 'text/json', success: %3 })"

ajaxHtml :: String -> (String -> Fay()) -> Fay ()
ajaxHtml = ffi "jQuery.ajax(%1, { success : %2 })"

jShow :: String -> JQuery -> Fay JQuery
jShow = ffi "jQuery(%2).show(%1)"

hide :: String -> JQuery -> Fay JQuery
hide = ffi "jQuery(%2).hide(%1)"

child :: String -> JQuery -> Fay JQuery
child = ffi "jQuery(%1,%2)[0]"

addClass :: String -> JQuery -> Fay JQuery
addClass = ffi "jQuery(%2).addClass(%1)"

removeClass :: String -> JQuery -> Fay JQuery
removeClass = ffi "jQuery(%2).removeClass(%1)"

j :: String -> Fay JQuery
j = ffi "jQuery(%1)"

html :: String -> JQuery -> Fay JQuery
html = ffi "jQuery(%2).html(%1)"

appendTo :: JQuery -> JQuery -> Fay JQuery
appendTo = ffi "jQuery(%1).appendTo(%2)"

click :: Fay () -> JQuery -> Fay JQuery
click = ffi "jQuery(%2).click(%1)"
