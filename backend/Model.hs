{-# LANGUAGE FlexibleInstances #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Handler.DB
import Yesod.Auth.HashDB (HashDBUser(..))

-- You can define all of your database entities in the entities file.
-- You can find more information on ersistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
-- share [mkPersist sqlSettings, mkMigrate "migrateAll"]
--    $(persistFileWith lowerCaseSettings "config/models")
--
instance HashDBUser User where
    userPasswordHash = (Just . userPassword)
    userPasswordSalt = (Just . userSalt)
    setSaltAndPasswordHash s h u = u {
        userSalt     = s,
        userPassword = h
    }

