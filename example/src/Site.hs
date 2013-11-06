{-# LANGUAGE OverloadedStrings #-}

module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Trans
import           Data.ByteString                             (ByteString)
import qualified Data.Text                                   as T
import qualified Data.Text.Encoding                          as T
import           Data.Time.Clock
import           Snap.Snaplet
import           Snap.Snaplet.Auth                           hiding (session)
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Fay
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Application
import           Application.SharedTypes


currentTimeAjax :: AppHandler Time
currentTimeAjax = Time . T.pack . show <$> liftIO getCurrentTime

-- TODO this can be handled automatically by heistServe
registerForm :: AppHandler ()
registerForm = render "register-form"
loginForm :: AppHandler ()
loginForm = render "login-form"

register :: UserRegister -> Handler App (AuthManager App) UserRegisterResponse
register (UserRegister u p pc)
  | T.length u < 4 || T.length p < 4 || p /= pc = return Fail
  | otherwise = do
    exists <- usernameExists u
    if exists then return Fail else (createUser u (T.encodeUtf8 p) >> return OK)

login :: UserLogin -> Handler App (AuthManager App) UserLoginResponse
login (UserLogin u p r) =
  either (return BadLogin) (return LoggedIn) <$>
    loginByUsername u (ClearText $ T.encodeUtf8 p) r

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [
           ("/ajax/current-time",  toFayax currentTimeAjax)
         , ("/ajax/login",         with auth $ fayax login)
         , ("/ajax/login-form",    loginForm)
         , ("/ajax/logout",        with auth logout)
         , ("/ajax/register",      with auth $ fayax register)
         , ("/ajax/register-form", registerForm)
         , ("/fay",                with fay fayServe)
         , ("/static",             serveDirectory "static")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    s <- nestSnaplet "sess" session $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    h <- nestSnaplet "" heist $ heistInit "templates"
    f <- nestSnaplet "fay" fay $ initFay
    a <- nestSnaplet "auth" auth $ initJsonFileAuthManager defAuthSettings session "users.json"
    addAuthSplices h auth
    addRoutes routes
    return $ App h f s a
