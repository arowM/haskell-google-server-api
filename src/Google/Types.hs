{-# LANGUAGE TypeOperators #-}

{- |
Module      :  Google.Types

Define data types to represent content types that are used in Google Drive API.
-}
module Google.Types
  ( Arbitrary
  , Multipart
  ) where

import Data.Aeson (encode)
import qualified Data.ByteString.Base64 as BSB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Monoid ((<>))
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable (Typeable)
import Network.HTTP.Media ((//), (/:))
import Servant.API
  ( Accept(..)
  , MimeRender(..)
  , MimeUnrender(..)
  )

import qualified Google.Form as Form



data Arbitrary
  deriving Typeable

instance Accept Arbitrary where
  contentTypes _ =
    ( "text" // "plain" ) :|
    [ "text" // "html"
    , "application" // "pdf"
    -- , other content type
    ]

instance MimeUnrender Arbitrary BS.ByteString where
  mimeUnrender _ = Right . id . LBS.toStrict


data Multipart
  deriving Typeable

boundary :: LBS.ByteString
boundary = "314159265358979323846"

instance Accept Multipart where
  contentType _ = "multipart" // "related" /: ("boundary", LBS.toStrict boundary)

instance MimeRender Multipart Form.MultipartBody where
  mimeRender _ Form.MultipartBody{..} =
    mconcat
      [ "\r\n--" <> boundary <> "\r\n"
      , "Content-Type: application/json; charset=UTF-8"
      , "\r\n\r\n"
      , encode metadata
      , "\r\n--" <> boundary <> "\r\n"
      , "Content-Type: " <> (LBS.fromStrict $ encodeUtf8 mediaType)
      , "\r\n"
      , "Content-Transfer-Encoding: base64"
      , "\r\n\r\n"
      , LBS.fromStrict $ BSB.encode mediaContent
      , "\r\n--" <> boundary <> "--"
      ]
