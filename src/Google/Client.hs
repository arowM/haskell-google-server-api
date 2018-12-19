{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      :  Google.Client

Define functions to call Google APIs.
-}
module Google.Client
  ( getToken
  , postCalendarEvent
  , postGmailSend
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Base64.URL (encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Data (Data)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Mail.Mime
import Servant.API
  ( (:<|>)(..)
  , (:>)
  , Capture
  , FormUrlEncoded
  , FromHttpApiData
  , Header
  , JSON
  , Post
  , ReqBody
  , ToHttpApiData
  )
import Servant.Client
  ( BaseUrl(BaseUrl)
  , ClientEnv(..)
  , ClientM
  , Scheme(..)
  , ServantError
  , client
  , runClientM
  )

import qualified Google.Form as Form
import Google.JWT (JWT)
import qualified Google.JWT as JWT
import qualified Google.Response as Response

newtype Bearer = Bearer
  { _unBearer :: Text
  } deriving ( Data
             , Eq
             , FromHttpApiData
             , FromJSON
             , Generic
             , Ord
             , Show
             , ToHttpApiData
             , ToJSON
             , Typeable
             )

type API
  = "oauth2" :> "v4" :> "token" :>
    ReqBody '[ FormUrlEncoded] Form.Token :>
    Post '[ JSON] Response.Token
  :<|> "calendar" :> "v3" :> "calendars" :>
    Capture "calendarId" Text :>
    "events" :>
    Header "Authorization" Bearer :>
    ReqBody '[ JSON] Form.CalendarEvent :>
    Post '[ JSON] Response.CalendarEvent
  :<|> "gmail" :> "v1" :> "users" :> "me" :> "messages" :> "send" :>
    Header "Authorization" Bearer :>
    ReqBody '[ JSON] Form.GmailSend :>
    Post '[ JSON] Response.GmailSend

api :: Proxy API
api = Proxy

getToken' :: Form.Token -> ClientM Response.Token
postCalendarEvent' ::
     Text
  -> Maybe Bearer
  -> Form.CalendarEvent
  -> ClientM Response.CalendarEvent
postGmailSend' :: Maybe Bearer -> Form.GmailSend -> ClientM Response.GmailSend
getToken' :<|> postCalendarEvent' :<|> postGmailSend' = client api

getToken ::
     Maybe JWT.Email
  -> JWT
  -> [JWT.Scope]
  -> IO (Either ServantError Response.Token)
getToken maccount jwt scopes = do
  manager <- newManager tlsManagerSettings
  Right a <- JWT.getSignedJWT jwt maccount scopes Nothing
  runClientM
    (getToken' $
     Form.Token
       { grantType = googleGrantType
       , assertion = decodeUtf8 . JWT.unSignedJWT $ a
       })
    (ClientEnv manager googleBaseUrl)

postCalendarEvent ::
     Response.Token
  -> Form.CalendarEvent
  -> IO (Either ServantError Response.CalendarEvent)
postCalendarEvent token event = do
  manager <- newManager tlsManagerSettings
  runClientM
    (postCalendarEvent'
       (Form.email . Form.creator $ event)
       (pure . toBearer $ token)
       event)
    (ClientEnv manager googleBaseUrl)

postGmailSend ::
     Response.Token -> Form.Email -> IO (Either ServantError Response.GmailSend)
postGmailSend token email = do
  manager <- newManager tlsManagerSettings
  mail <- (renderMail' =<< Form.toMail email)
  let gmailSend = Form.GmailSend {raw = decodeUtf8 $ encode $ LBS.toStrict mail}
  runClientM
    (postGmailSend' (pure . toBearer $ token) gmailSend)
    (ClientEnv manager googleBaseUrl)

toBearer :: Response.Token -> Bearer
toBearer Response.Token {accessToken} = Bearer $ "Bearer " <> accessToken

{- =================
 -  Constant values
 - ================= -}
googleGrantType :: Text
googleGrantType = "urn:ietf:params:oauth:grant-type:jwt-bearer"

googleBaseUrl :: BaseUrl
googleBaseUrl = BaseUrl Https "www.googleapis.com" 443 ""
