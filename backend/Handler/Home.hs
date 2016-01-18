{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where
import Yesod.Auth
import Import
import Handler.DB
import Data.Time
import Handler.Utils
getHomeR :: Handler Value
getHomeR = do
    (Entity _ u) <- requireAuth
    today <- liftIO $ fmap utctDay getCurrentTime
    return $ object [
            "user" .= object [
                "name" .= userName u,
                "firstName" .= userFirstName u,
                "lastName" .= userLastName u,
                "email" .= userEmail u,
                "config" .= userConfig u,
                "validContract" .= isContractValid u today
            ]
        ]
