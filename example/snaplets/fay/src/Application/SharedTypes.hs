{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE CPP #-}

module Application.SharedTypes where

import           Prelude
import           Data.Data
#ifdef FAY
import           FFI
#endif


data Time = Time { time :: String }
            deriving (Read,Data,Typeable,Show)

data UserRegister = UserRegister { user :: String, password :: String, passwordConfirmation :: String }
                    deriving (Read,Data,Typeable,Show)

data UserLogin = UserLogin { ul_user :: String, ul_password :: String, remember :: Bool }
                    deriving (Read,Data,Typeable,Show)

data UserRegisterResponse = Fail | OK
                            deriving (Read,Data,Typeable,Show)

data UserLoginResponse = LoggedIn | BadLogin
                            deriving (Read,Data,Typeable,Show)
