{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      :  Google.Client

Define functions to call Google APIs.
-}
module Google.Client
  ( getToken
  , getCalendarEventList
  , postCalendarEvent
  , postGmailSend
  , getDriveFileList
  , createDriveFileMultipart
  , exportDriveFile
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Base64.URL (encode)
import qualified Data.ByteString as BS
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
  , Get
  , Header
  , JSON
  , Post
  , QueryParam
  , ReqBody
  , ToHttpApiData
  )
import Servant.Client
  ( BaseUrl(BaseUrl)
  , ClientM
  , Scheme(..)
#if MIN_VERSION_servant(0, 16, 0)
  , ClientError
#else
  , ServantError
#endif
  , client
  , mkClientEnv
  , runClientM
  )

import qualified Google.Form as Form
import Google.JWT (JWT)
import qualified Google.JWT as JWT
import qualified Google.Response as Response
import Google.Types (Multipart, Arbitrary)

#if !MIN_VERSION_servant(0, 16, 0)
type ClientError = ServantError
#endif

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
    QueryParam "singleEvents" Bool :>
    QueryParam "timeMin" Form.DateTime :>
    QueryParam "timeMax" Form.DateTime :>
    QueryParam "orderBy" Text :>
    Get '[ JSON] Response.CalendarEventList
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
  :<|> "drive":> "v3" :> "files" :>
    Header "Authorization" Bearer :>
    QueryParam "q" Text :>
    QueryParam "orderBy" Text :>
    Get '[ JSON] Response.FileList
  :<|> "upload" :> "drive":> "v3" :> "files" :>
    Header "Authorization" Bearer :>
    QueryParam "uploadType" Text :>
    ReqBody '[ Multipart] Form.MultipartBody :>
    Post '[ JSON] Response.FileResource
  :<|> "drive":> "v3" :> "files" :>
    Capture "fileId" Text :>
    "export" :>
    Header "Authorization" Bearer :>
    QueryParam "mimeType" Text :>
    Get '[ Arbitrary] BS.ByteString



api :: Proxy API
api = Proxy

getToken' :: Form.Token -> ClientM Response.Token
getCalendarEventList' ::
     Text
  -> Maybe Bearer
  -> Maybe Bool
  -> Maybe Form.DateTime
  -> Maybe Form.DateTime
  -> Maybe Text
  -> ClientM Response.CalendarEventList
postCalendarEvent' ::
     Text
  -> Maybe Bearer
  -> Form.CalendarEvent
  -> ClientM Response.CalendarEvent
postGmailSend' :: Maybe Bearer -> Form.GmailSend -> ClientM Response.GmailSend
getDriveFileList' ::
     Maybe Bearer
  -> Maybe Text
  -> Maybe Text
  -> ClientM Response.FileList
createDriveFileMultipart' ::
     Maybe Bearer
  -> Maybe Text
  -> Form.MultipartBody
  -> ClientM Response.FileResource
exportDriveFile' ::
     Text
  -> Maybe Bearer
  -> Maybe Text
  -> ClientM BS.ByteString
getToken'
  :<|> getCalendarEventList'
  :<|> postCalendarEvent'
  :<|> postGmailSend'
  :<|> getDriveFileList'
  :<|> createDriveFileMultipart'
  :<|> exportDriveFile'
  = client api

getToken ::
     Maybe JWT.Email
  -> JWT
  -> [JWT.Scope]
  -> IO (Either ClientError Response.Token)
getToken maccount jwt scopes = do
  manager <- newManager tlsManagerSettings
  Right a <- JWT.getSignedJWT jwt maccount scopes Nothing
  runClientM
    (getToken' $
     Form.Token
       { grantType = googleGrantType
       , assertion = decodeUtf8 . JWT.unSignedJWT $ a
       })
    (mkClientEnv manager googleBaseUrl)

getCalendarEventList ::
     Response.Token
  -> Text
  -> Maybe Bool
  -> Maybe Form.DateTime
  -> Maybe Form.DateTime
  -> Maybe Text
  -> IO (Either ClientError Response.CalendarEventList)
getCalendarEventList token calendarId singleEvents timeMin timeMax orderBy = do
  manager <- newManager tlsManagerSettings
  runClientM
    (getCalendarEventList'
       calendarId
       (pure . toBearer $ token)
       singleEvents
       timeMin
       timeMax
       orderBy)
    (mkClientEnv manager googleBaseUrl)

postCalendarEvent ::
     Response.Token
  -> Form.CalendarEvent
  -> IO (Either ClientError Response.CalendarEvent)
postCalendarEvent token event = do
  manager <- newManager tlsManagerSettings
  runClientM
    (postCalendarEvent'
       (Form.email . Form.creator $ event)
       (pure . toBearer $ token)
       event)
    (mkClientEnv manager googleBaseUrl)

postGmailSend ::
     Response.Token -> Form.Email -> IO (Either ClientError Response.GmailSend)
postGmailSend token email = do
  manager <- newManager tlsManagerSettings
  mail <- (renderMail' =<< Form.toMail email)
  let gmailSend = Form.GmailSend {raw = decodeUtf8 $ encode $ LBS.toStrict mail}
  runClientM
    (postGmailSend' (pure . toBearer $ token) gmailSend)
    (mkClientEnv manager googleBaseUrl)

getDriveFileList ::
     Response.Token
  -> Maybe Text
  -> Maybe Text
  -> IO (Either ServantError Response.FileList)
getDriveFileList token query orderBy = do
  manager <- newManager tlsManagerSettings
  runClientM
    (getDriveFileList'
       (pure . toBearer $ token)
       query
       orderBy)
    (mkClientEnv manager googleBaseUrl)

createDriveFileMultipart ::
     Response.Token
  -> Form.MultipartBody
  -> IO (Either ServantError Response.FileResource)
createDriveFileMultipart token body = do
  manager <- newManager tlsManagerSettings
  runClientM
    (createDriveFileMultipart'
      (pure . toBearer $ token)
      (Just "multipart")
      body)
    (mkClientEnv manager googleBaseUrl)

exportDriveFile ::
     Response.Token
  -> Text
  -> Text
  -> IO (Either ServantError BS.ByteString)
exportDriveFile token fileId mimeType = do
  manager <- newManager tlsManagerSettings
  runClientM
    (exportDriveFile'
       fileId
       (pure . toBearer $ token)
       (Just mimeType))
    (mkClientEnv manager googleBaseUrl)

toBearer :: Response.Token -> Bearer
toBearer Response.Token {accessToken} = Bearer $ "Bearer " <> accessToken

{- =================
 -  Constant values
 - ================= -}
googleGrantType :: Text
googleGrantType = "urn:ietf:params:oauth:grant-type:jwt-bearer"

googleBaseUrl :: BaseUrl
googleBaseUrl = BaseUrl Https "www.googleapis.com" 443 ""
