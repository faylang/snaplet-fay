{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Application.SharedTypes where

import           Language.Fay.FFI
import           Language.Fay.Prelude

data Time = Time { time :: String }
            deriving (Read,Data,Typeable,Show)
instance Foreign Time

data UserRegister = UserRegister { user :: String, password :: String, passwordConfirmation :: String }
                    deriving (Read,Data,Typeable,Show)
instance Foreign UserRegister

data UserLogin = UserLogin { ul_user :: String, ul_password :: String, remember :: Bool }
                    deriving (Read,Data,Typeable,Show); instance Foreign UserLogin

data UserRegisterResponse = Fail | OK
                            deriving (Read,Data,Typeable,Show)
instance Foreign UserRegisterResponse

data UserLoginResponse = LoggedIn | BadLogin
                            deriving (Read,Data,Typeable,Show)
instance Foreign UserLoginResponse
