{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

{- |
Module      :  Google.Type

Define basic data types.
-}
module Google.Type
  ( FileId(..)
  , MediaType(..)
  , MediaContent(..)
  , Metadata(..)
  , Arbitrary
  , Multipart
  , ConversionFormat(..)
  , SortKey(..)
  , QueryString(..)
  , Order(..)
  , LabelId(..)
  ) where

import Data.Aeson.TH (Options(..), defaultOptions, deriveJSON)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import Data.List.NonEmpty (NonEmpty((:|)))
#if !MIN_VERSION_base(4, 9, 0)
import Data.Monoid ((<>))
#endif
import Data.Text (Text, intercalate)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Network.HTTP.Media ((//), (/:))
import qualified Network.HTTP.Media as Media
import Servant.API (Accept(..), MimeUnrender(..))
import Web.HttpApiData (ToHttpApiData(..), toUrlPieces)


newtype FileId = FileId
  { fileId :: Text
  } deriving (Eq, Generic, Show, Typeable, ToHttpApiData)

deriveJSON defaultOptions {unwrapUnaryRecords = True} ''FileId


newtype MediaType = MediaType
  { mediaTypeName :: Text
  } deriving (Eq, Generic, Show, Typeable)

deriveJSON defaultOptions {unwrapUnaryRecords = True} ''MediaType


newtype MediaContent = MediaContent
  { content :: BS.ByteString
  } deriving (Eq, Generic, Show, Typeable)


data Metadata = Metadata
  { name :: Maybe Text
  , mimeType :: Maybe MediaType
  , parents :: Maybe [FileId]
  } deriving (Eq, Generic, Show, Typeable)

deriveJSON defaultOptions ''Metadata


data Arbitrary

instance Accept Arbitrary where
  contentTypes _ =
    fromFormat <$>
      FormatHtml :|
        [ FormatHtmlZipped
        , FormatPlainText
        , FormatRichText
        , FormatOpenOfficeDoc
        , FormatPdf
        , FormatMsWordDoc
        , FormatEpub
        ]

instance MimeUnrender Arbitrary MediaContent where
  mimeUnrender _ = Right . MediaContent . LBS.toStrict


data Multipart

instance Accept Multipart where
  contentType _ = "multipart" // "related" /: ("boundary", "314159265358979323846")


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

newtype LabelId = LabelId Text
  deriving (Eq, Generic, Show, Typeable, ToHttpApiData)

instance ToHttpApiData [LabelId] where
  toUrlPiece = (intercalate ",") . toUrlPieces
