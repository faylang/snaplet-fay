{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Application.SharedTypes where

import           Data.Data
import           Fay.Text  (Text)
import           Prelude
#ifdef FAY
import           FFI
#endif


data Time = Time { time :: Text }
            deriving (Read,Data,Typeable,Show)

data UserRegister = UserRegister { user :: Text, password :: Text, passwordConfirmation :: Text }
                    deriving (Read,Data,Typeable,Show)

data UserLogin = UserLogin { ul_user :: Text, ul_password :: Text, remember :: Bool }
                    deriving (Read,Data,Typeable,Show)

data UserRegisterResponse = Fail | OK
                            deriving (Read,Data,Typeable,Show)

data UserLoginResponse = LoggedIn | BadLogin
                            deriving (Read,Data,Typeable,Show)
