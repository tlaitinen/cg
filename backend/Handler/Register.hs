{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Register (postRegisterR) where
import Prelude ((!!))
import Yesod.Auth
import Import
import Text.Shakespeare.Text hiding (toText)
import Handler.DB
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Data.Time
import Network.HTTP.Types (status400)
import qualified Data.Aeson as A
import Data.Aeson.TH
import Handler.Utils
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Network.Mail.SMTP (sendMail)
import Network.Mail.Mime
data Params = Params {
    p_userName  :: Text,
    p_firstName :: Text,
    p_lastName  :: Text,
    p_organization :: Text,
    p_email     :: Text,
    p_deliveryEmail :: Text,
    p_recaptchaResponse  :: Text
} 
$(deriveJSON defaultOptions{fieldLabelModifier = drop 2} ''Params)

postRegisterR :: Handler Value
postRegisterR = do 
    jr <- parseJsonBody
    p <- case jr of
        A.Error err -> failure $ A.String $ T.pack err
        A.Success p -> return p
    ip <- getIp
    app <- getYesod
    let settings = appSettings app
    runDB $ do
        mu <- getBy $ UniqueUserEmail Active $ p_email p
        when (isJust mu) $ failure $ A.String "email-unavailable"
        mug <- getBy $ UniqueUserGroup Active $ p_userName p
        if isJust mug
            then failure $ A.String "username-unavailable"
            else do
                let ug = (newUserGroup $ p_userName p) {
                            userGroupEmail        = p_deliveryEmail p,
                            userGroupOrganization = Just $ p_organization p
                        }

                ugId <- insert ug
                now <- liftIO getCurrentTime
                let today = utctDay now
                token <- liftIO $ rndString 43
                let u = (newUser ugId (p_userName p)) {
                        userFirstName               = p_firstName p,
                        userLastName                = p_lastName p,
                        userEmail                   = p_email p,
                        userPasswordResetToken      = Just token,
                        userContractStartDate       = Just today,
                        userContractEndDate         = Just $ addDays 30 today,
                        userPasswordResetValidUntil = Just $ addUTCTime 3600 now
                    }
                uId <- insert u    
                _ <- insert $ newUserGroupItem ugId uId UserGroupModeReadWrite
                _ <- insert $ (newUserGroupContent ugId) {
                        userGroupContentUserContentId = Just uId
                    }
                _ <- insert $ (newUserGroupContent ugId) {
                        userGroupContentUserGroupContentId = Just ugId
                    }
                _ <- insert $ (newUserGroupContent $ toSqlKey 1) {
                        userGroupContentUserGroupContentId = Just ugId
                    }
                _ <- insert $ (newUserGroupContent $ toSqlKey 1) {
                        userGroupContentUserContentId = Just uId
                    }
                liftIO $ do
                    (plain, html) <- messageBody $ url settings uId u
                    mail <- simpleMail 
                        (Address 
                            (Just $ T.concat [
                                    p_firstName p, " ", p_lastName p
                                ])
                            (p_email p))
                        (Address Nothing $ appSenderEmail settings)
                        (renderMessage app langs MsgRegisterEmailTitle)
                        plain html []   
                    sendMail (appSmtpAddress settings) mail
                return $ A.object [
                        "success" .= True
                    ]
    where
        url settings uId u = T.concat [
                appRoot settings,
                "/?userId=", T.pack $ show $ fromSqlKey uId,
                "&token=",
                fromMaybe "" $ userPasswordResetToken u
            ]
        langs = ["fi"]
        messageBody url = do
            let html = LT.fromStrict $(codegenFile "templates/register.html")
            let plain = LT.fromStrict $(codegenFile "templates/register.txt")
            return (plain,html)
        failure err = sendResponseStatus status400 $ A.object [
                "success" .= False,
                "error" .= err
            ]
        
