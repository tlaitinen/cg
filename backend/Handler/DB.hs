{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Handler.DB 
    ( module Handler.DB.Enums, module Handler.DB.Internal, module Handler.DB.Routes ) where
import Handler.DB.Enums
import Handler.DB.Internal
import Handler.DB.RouteFiles
import Handler.DB.RouteFilesFile
import Handler.DB.RouteUsergroupcontents
import Handler.DB.RouteUsergroups
import Handler.DB.RouteUsergroupsUserGroup
import Handler.DB.RouteUsergroupitems
import Handler.DB.RouteUsergroupitemsUserGroupItem
import Handler.DB.RouteUsers
import Handler.DB.RouteUsersUser
import Handler.DB.RouteVersions
import Handler.DB.RouteSettings

import Handler.DB.Routes
import Yesod.Core
import Yesod.Auth
import Yesod.Persist
import Database.Esqueleto
import Prelude
type DBRoute = Route DB
 
instance (YesodAuthPersist master,
          AuthId master ~ Key User,
          AuthEntity master ~ User,
          YesodPersistBackend master ~ SqlBackend) => YesodSubDispatch DB (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesDB)
