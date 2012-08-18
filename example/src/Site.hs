{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Monad.Trans
import           Data.Aeson
import           Data.ByteString         (ByteString)
import           Data.Time.Clock
import           Language.Fay.Convert
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Fay
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Application
import           Application.SharedTypes


currentTimeAjax :: AppHandler ()
currentTimeAjax = do
  t <- liftIO getCurrentTime
  modifyResponse . setContentType $ "text/json;charset=utf-8"
  writeLBS $ encode $ showToFay (CTR (show t))


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [
           ("/ajax/current-time", currentTimeAjax)
         , ("/fay", with fay fayServe)
         , ("/static", serveDirectory "static")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    f <- nestSnaplet "fay" fay $ initFay
    addRoutes routes
    return $ App h f
