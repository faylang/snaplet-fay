{-# LANGUAGE TemplateHaskell #-}

module Application where
------------------------------------------------------------------------------
import           Data.Lens.Template
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Fay
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session
------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _fay :: Snaplet Fay
    , _session :: Snaplet SessionManager
    , _auth :: Snaplet (AuthManager App)
    }

makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist

------------------------------------------------------------------------------
type AppHandler = Handler App App
