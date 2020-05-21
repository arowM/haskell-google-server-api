{-# LANGUAGE TemplateHaskell #-}

{- |
Module      :  Google.Form

Define data types to represent all of the requests that are sent to the API.
-}
module Google.Form
  ( CalendarEvent(..)
  , GmailSend(..)
  , Account(..)
  , DateTime(..)
  , Email(..)
  , toMail
  , FileId(..)
  , MediaType(..)
  , FileResource(..)
  , MediaContent(..)
  , Multipart
  , MultipartBody(..)
  , ConversionFormat(..)
  , fromFormat
  , QueryString(..)
  , SortKey(..)
  , Order(..)
  , Token(..)
  ) where

import Data.Aeson (encode)
import Data.Aeson.TH (Options(..), defaultOptions, deriveJSON)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as BSB
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import Data.Maybe (maybeToList)
import Data.Monoid ((<>))
import Data.String (IsString(..))
import Data.Text (Text, intercalate)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (fromStrict)
import Data.Time.Clock (UTCTime)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Network.HTTP.Media ((//), (/:))
import qualified Network.HTTP.Media as Media
import Network.Mail.Mime (Address(..), Mail(..), renderAddress, simpleMail)
import Servant.API (Accept(..), MimeRender(..))
import Web.FormUrlEncoded (Form(..), ToForm(toForm))
import Web.Internal.HttpApiData (toQueryParam)
import Web.HttpApiData (ToHttpApiData(..), toUrlPieces)
import qualified Data.HashMap.Strict as HashMap

data Account = Account
  { email :: Text
  } deriving (Eq, Generic, Show, Typeable)

deriveJSON defaultOptions ''Account

instance IsString Account where
  fromString = Account . fromString

newtype DateTime = DateTime
  { dateTime :: UTCTime
  } deriving (Eq, Generic, Show, Typeable, ToHttpApiData)

deriveJSON defaultOptions ''DateTime

data CalendarEvent = CalendarEvent
  { creator :: Account
  , attendees :: [Account]
  , summary :: Text
  , description :: Text
  , start :: DateTime
  , end :: DateTime
  } deriving (Eq, Generic, Show, Typeable)

deriveJSON defaultOptions ''CalendarEvent

data Token = Token
  { grantType :: Text
  , assertion :: Text
  } deriving (Eq, Generic, Show, Typeable)

instance ToForm Token where
  toForm token =
    Form . HashMap.fromList $
    [ ("grant_type", [toQueryParam (grantType token)])
    , ("assertion", [toQueryParam (assertion token)])
    ]

data Email = Email
  { to :: Text
  , from :: Text
  , replyTo :: Maybe Text
  , ccs :: [Text]
  , subject :: Text
  , body :: Text
  } deriving (Eq, Generic, Show, Typeable)

deriveJSON defaultOptions ''Email

toMail :: Email -> IO Mail
toMail Email {..} = do
  mail <-
    simpleMail
      (Address Nothing to)
      (Address Nothing from)
      subject
      body'
      body'
      []
  pure $
    mail
      { mailHeaders =
          mailHeaders mail <> do
            rt <- maybeToList replyTo
            pure ("Reply-To", renderAddress $ Address Nothing rt)
      , mailCc = map (Address Nothing) ccs
      }
  where
    body' = fromStrict body

data GmailSend = GmailSend
  { raw :: Text
  } deriving (Eq, Generic, Show, Typeable)

deriveJSON defaultOptions ''GmailSend


newtype FileId = FileId
  { fileId :: Text
  } deriving (Eq, Generic, Show, Typeable, ToHttpApiData)

deriveJSON defaultOptions {unwrapUnaryRecords = True} ''FileId


newtype MediaType = MediaType
  { mediaTypeName :: Text
  } deriving (Eq, Generic, Show, Typeable)

deriveJSON defaultOptions {unwrapUnaryRecords = True} ''MediaType


data FileResource = FileResource
  { name :: Maybe Text
  , mimeType :: Maybe MediaType
  , parents :: Maybe [FileId]
  } deriving (Eq, Generic, Show, Typeable)

deriveJSON defaultOptions ''FileResource


newtype MediaContent = MediaContent
  { content :: BS.ByteString
  } deriving (Eq, Generic, Show, Typeable)


data Multipart

boundary :: LBS.ByteString
boundary = "314159265358979323846"

instance Accept Multipart where
  contentType _ = "multipart" // "related" /: ("boundary", LBS.toStrict boundary)

data MultipartBody = MultipartBody
  { metadata :: FileResource
  , mediaType :: MediaType
  , mediaContent :: MediaContent
  } deriving (Eq, Generic, Show, Typeable)

instance MimeRender Multipart MultipartBody where
  mimeRender _ MultipartBody{..} =
    mconcat
      [ "\r\n--" <> boundary <> "\r\n"
      , "Content-Type: application/json; charset=UTF-8"
      , "\r\n\r\n"
      , encode metadata
      , "\r\n--" <> boundary <> "\r\n"
      , "Content-Type: " <> (LBS.fromStrict $ encodeUtf8 $ mediaTypeName mediaType)
      , "\r\n"
      , "Content-Transfer-Encoding: base64"
      , "\r\n\r\n"
      , LBS.fromStrict $ BSB.encode $ (content mediaContent)
      , "\r\n--" <> boundary <> "--"
      ]


-- https://developers.google.com/drive/api/v3/ref-export-formats
data ConversionFormat
  = FormatHtml
  | FormatHtmlZipped
  | FormatPlainText
  | FormatRichText
  | FormatOpenOfficeDoc
  | FormatPdf
  | FormatMsWordDoc
  | FormatEpub
  | FormatMsExcel
  | FormatOpenOfficeSheet
  | FormatCsv
  | FormatTsv
  | FormatJpeg
  | FormatPng
  | FormatSvg
  | FormatMsPowerPoint
  | FormatMsOfficePresentation
  | FormatJson
  deriving (Eq, Generic, Show, Typeable)

fromFormat :: ConversionFormat -> Media.MediaType
fromFormat FormatHtml                 = "text" // "html"
fromFormat FormatHtmlZipped           = "application" // "zip"
fromFormat FormatPlainText            = "text" // "plain"
fromFormat FormatRichText             = "application" // "rtf"
fromFormat FormatOpenOfficeDoc        = "application" // "vnd.oasis.opendocument.text"
fromFormat FormatPdf                  = "application" // "pdf"
fromFormat FormatMsWordDoc            = "application" // "vnd.openxmlformats-officedocument.wordprocessingml.document"
fromFormat FormatEpub                 = "application" // "epub+zip"
fromFormat FormatMsExcel              = "application" // "vnd.openxmlformats-officedocument.spreadsheetml.sheet"
fromFormat FormatOpenOfficeSheet      = "application" // "x-vnd.oasis.opendocument.spreadsheet"
fromFormat FormatCsv                  = "text" // "csv"
fromFormat FormatTsv                  = "text" // "tab-separated-values"
fromFormat FormatJpeg                 = "image" // "jpeg"
fromFormat FormatPng                  = "image" // "png"
fromFormat FormatSvg                  = "image" // "svg+xml"
fromFormat FormatMsPowerPoint         = "application" // "vnd.openxmlformats-officedocument.presentationml.presentation"
fromFormat FormatMsOfficePresentation = "application" // "vnd.openxmlformavnd.oasis.opendocument.presentation"
fromFormat FormatJson                 = "application" // "vnd.google-apps.script+json"

instance ToHttpApiData ConversionFormat where
  toUrlPiece = toUrlPiece . show . fromFormat


data SortKey
  = CreatedTime
  | Folder
  | ModifiedByMeTime
  | ModifiedTime
  | Name
  | NameNatural
  | QuotaBytesUsed
  | Recency
  | SsharedWithMeTime
  | Starred
  | ViewedByMeTime
  deriving (Eq, Generic, Show, Typeable)

instance ToHttpApiData SortKey where
  toUrlPiece NameNatural = "name_natural"
  toUrlPiece key         = toUrlPiece . headToLower .  show $ key
    where
      headToLower :: String -> String
      headToLower [] = []
      headToLower (x : xs) = toLower x : xs


newtype QueryString = QueryString
  { queryString :: Text
  } deriving (Eq, Generic, Show, Typeable, ToHttpApiData)

deriveJSON defaultOptions {unwrapUnaryRecords = True} ''QueryString


data Order
  = Asc SortKey
  | Desc SortKey
  deriving (Eq, Generic, Show, Typeable)

instance ToHttpApiData Order where
  toUrlPiece (Asc key)  = toUrlPiece key
  toUrlPiece (Desc key) = toUrlPiece key <> " desc"

instance ToHttpApiData [Order] where
  toUrlPiece = (intercalate ",") . toUrlPieces
