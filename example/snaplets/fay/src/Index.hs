{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

module Index where

import           Language.Fay.FFI
import           Language.Fay.Prelude

-- | Time is shared between Snap and Fay
-- | Location: snaplets/fay/src/Application/SharedTypes.hs
-- import Time
import           Application.SharedTypes
-- | Dom and JQuery are Fay only modules
-- | Location: snaplets/fay/src
import           Dom
import           JQuery

void :: Fay f -> Fay ()
void f = f >> return ()

main :: Fay ()
main = mapM_ addOnload [onload, registrationOnload, loginOnload, void (j "#logout" >>= click submitLogout)]

(=<<) :: (a -> Fay b) -> Fay a -> Fay b
(=<<) = flip (>>=)

onload :: Fay ()
onload = void $ do
  contents <- j "#content"
  div <- j "<div></div>"
  html "This element was created by Fay through an onload handler!" div
  appendTo div contents

  setTimeout 5000 currentTime

currentTime :: Fay ()
currentTime =
  ajaxJson "/ajax/current-time" $ \(Time time) -> void $ j "#current-time" >>= html time

formOnload :: String -> Fay () -> Fay ()
formOnload buttonSel getForm = void $ j buttonSel >>= click getForm

registrationOnload :: Fay ()
registrationOnload = formOnload "#viewRegisterForm" requestRegisterHtml

loginOnload :: Fay ()
loginOnload = formOnload "#viewLoginForm" requestLoginHtml

requestHtml :: String -> Fay () -> Fay ()
requestHtml url submitAction = do
  formContainer <- j "#formContainer"
  hide "slow" formContainer
  ajaxHtml url $ \h -> void $ do
    html h formContainer
    submit <- child ".submit" formContainer
    click submitAction submit
    jShow "slow" formContainer

requestRegisterHtml :: Fay ()
requestRegisterHtml = requestHtml "/ajax/register-form" submitRegister

requestLoginHtml :: Fay ()
requestLoginHtml = requestHtml "/ajax/login-form" submitLogin

submitRegister :: Fay ()
submitRegister = do
  json <- j "#formContainer form" >>= formJson :: Fay UserRegister
  jPost "/ajax/register" json $ \c -> case c of
    Fail -> j "#loginStatus" >>= showStatus Error "Oops! Username taken or fields have length < 4"
    OK -> do
      j "#loginStatus" >>= showStatus Notice "Account created!"
      j "#formContainer" >>= hide "fast" >> requestLoginHtml

submitLogin :: Fay ()
submitLogin = do
  form <- j "#formContainer form"
  json <- formJson form :: Fay UserLogin
  jPost "/ajax/login" json $ \c -> case c of
    BadLogin -> j "#loginStatus" >>= showStatus Error "Oops! Bad login information!"
    LoggedIn -> void $ do
      j "#loginStatus" >>= showStatus Notice "Logged in! Too bad there is no additional functionality for you now."
      j "#formContainer" >>= hide "fast"

submitLogout :: Fay ()
submitLogout = ajax "/ajax/logout" (void $ j "#loginStatus" >>= showStatus Notice "You have been logged out.")

data Status = Error | Notice

statusClass :: Status -> String
statusClass Error = "error"
statusClass Notice = "notice"

showStatus :: Status -> String -> JQuery -> Fay ()
showStatus status msg el = void $
  return el >>= hide "fast" >>=
    removeClass (statusClass Error) >>= removeClass (statusClass Notice) >>=
      html msg >>=
        addClass (statusClass status) >>=
          jShow "fast"

formJson :: Foreign f => JQuery -> Fay f
formJson = ffi "Helpers.formJson(%1)"
