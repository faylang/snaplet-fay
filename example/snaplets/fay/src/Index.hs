{-# LANGUAGE EmptyDataDecls    #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

module Index where

import FFI
import Prelude

-- | Time is shared between Snap and Fay
-- | Location: snaplets/fay/src/Application/SharedTypes.hs
import Application.SharedTypes
-- | Dom is a Fay only module
-- | Location: snaplets/fay/src
import Dom
-- | The fay-jquery package
import JQuery

void :: Fay f -> Fay ()
void f = f >> return ()

main :: Fay ()
main = mapM_ addOnload [onload, registrationOnload, loginOnload, void (select "#logout" >>= click (const submitLogout))]

(=<<) :: (a -> Fay b) -> Fay a -> Fay b
(=<<) = flip (>>=)

onload :: Fay ()
onload = void $ do
  contents <- select "#content"
  div <- select "<div></div>"
  setHtml "This element was created by Fay through an onload handler!" div
  appendTo contents div

  currentTime
  setInterval 2000 currentTime

currentTime :: Fay ()
currentTime =
  ajaxJson "/ajax/current-time" (\(Time time) -> void $ select "#current-time" >>= setHtml time)

formOnload :: String -> Fay () -> Fay ()
formOnload buttonSel getForm = void $ select buttonSel >>= click (const getForm)

registrationOnload :: Fay ()
registrationOnload = formOnload "#viewRegisterForm" requestRegisterHtml

loginOnload :: Fay ()
loginOnload = formOnload "#viewLoginForm" requestLoginHtml

requestHtml :: String -> Fay () -> Fay ()
requestHtml url submitAction = do
  formContainer <- select "#formContainer"
  hide Slow formContainer
  ajaxHtml url (\h -> do
    setHtml h formContainer
    findSelector "form" formContainer >>= submit (\e -> preventDefault e >> submitAction)
    jshow Slow formContainer
    return ())

typeof :: f -> String
typeof = ffi "typeof %1"

requestRegisterHtml :: Fay ()
requestRegisterHtml = requestHtml "/ajax/register-form" submitRegister

requestLoginHtml :: Fay ()
requestLoginHtml = requestHtml "/ajax/login-form" submitLogin

submitRegister :: Fay ()
submitRegister = do
  json <- select "#formContainer form" >>= formJson :: Fay UserRegister
  jPost "/ajax/register" json (\c -> case c of
    Fail -> select "#loginStatus" >>= showStatus Error "Oops! Username taken or fields have length < 4"
    OK -> do
      select "#loginStatus" >>= showStatus Notice "Account created!"
      select "#formContainer" >>= hide Fast >> requestLoginHtml)

submitLogin :: Fay ()
submitLogin = do
  form <- select "#formContainer form"
  json <- formJson form :: Fay UserLogin
  jPost "/ajax/login" json (\c -> case c of
    BadLogin -> select "#loginStatus" >>= showStatus Error "Oops! Bad login information!"
    LoggedIn -> void $ do
      select "#loginStatus" >>= showStatus Notice "Logged in! Too bad there is no additional functionality for you now."
      select "#formContainer" >>= hide Fast)

submitLogout :: Fay ()
submitLogout = ajaxJson "/ajax/logout" (\_ -> void $ select "#loginStatus" >>= showStatus Notice "You have been logged out.")

data Status = Error | Notice

statusClass :: Status -> String
statusClass Error = "error"
statusClass Notice = "notice"

showStatus :: Status -> String -> JQuery -> Fay ()
showStatus status msg el = void $
  return el >>= hide Fast >>=
    removeClass (statusClass Error) >>= removeClass (statusClass Notice) >>=
      setHtml msg >>=
        addClass (statusClass status) >>=
          jshow Fast >> return ()

formJson :: JQuery -> Fay f
formJson = ffi "Helpers.formJson(%1)"

-- jQuery additions

jPost :: String -> Automatic f -> (Automatic g -> Fay ()) -> Fay ()
jPost = ffi "jQuery.ajax(%1, { data: JSON.stringify(%2), type: 'POST', processData: false, contentType: 'text/json', success: %3 })"

ajaxHtml :: String -> (String -> Fay()) -> Fay ()
ajaxHtml = ffi "jQuery.ajax(%1, { success : %2 })"

ajaxJson :: String -> (Automatic f -> Fay ()) -> Fay ()
ajaxJson = ffi "jQuery.ajax(%1, { success : %2 })"
