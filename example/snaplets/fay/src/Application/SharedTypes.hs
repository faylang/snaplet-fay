{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE CPP #-}

module Application.SharedTypes where

#ifdef FAY
import           FFI
import           Prelude
#else
import           Language.Fay.FFI
#endif
import           Data.Data


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
