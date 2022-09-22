{-# LANGUAGE CPP #-}
-- | Create a signed JWT needed to make the access token request
-- to gain access to Google APIs for server to server applications.
--
-- For all usage details, see https://developers.google.com/identity/protocols/OAuth2ServiceAccount
--
-- This module is borrowed from google-oauth2-jwt package.
module Google.JWT
  ( JWT
  , HasJWT(..)
  , readServiceKeyFile
  , SignedJWT(..)
  , Email(..)
  , Scope(..)
  , getSignedJWT
  ) where

import Codec.Crypto.RSA.Pure
  ( PrivateKey(..)
  , PublicKey(..)
  , hashSHA256
  , rsassa_pkcs1_v1_5_sign
  )
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Aeson ((.:), decode)
import Data.Aeson.Types (parseMaybe)
import Data.ByteString (ByteString)
import Data.ByteString.Base64.URL (encode)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Maybe (fromJust, fromMaybe)
#if !MIN_VERSION_base(4, 11, 0)
import Data.Monoid ((<>))
#endif
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.UnixTime (getUnixTime, utSeconds)
import Foreign.C.Types (CTime(..))
import OpenSSL.EVP.PKey (toKeyPair)
import OpenSSL.PEM (PemPasswordSupply(PwNone), readPrivateKey)
import OpenSSL.RSA (rsaD, rsaE, rsaN, rsaP, rsaQ, rsaSize)

class HasJWT a where
  getJwt :: a -> JWT

instance HasJWT JWT where
  getJwt :: JWT -> JWT
  getJwt = id

data JWT = JWT
  { clientEmail :: Email
  , privateKey :: PrivateKey
  } deriving (Eq, Show, Read)

readServiceKeyFile :: FilePath -> IO (Maybe JWT)
readServiceKeyFile fp = do
  content <- LBS.readFile fp
  runMaybeT $ do
    result <- MaybeT . pure . decode $ content
    (pkey, clientEmail) <-
      MaybeT . pure . flip parseMaybe result $ \obj -> do
        pkey <- obj .: "private_key"
        clientEmail <- obj .: "client_email"
        pure (pkey, clientEmail)
    liftIO $ JWT <$> (pure $ Email clientEmail) <*> (fromPEMString pkey)

newtype SignedJWT = SignedJWT
  { unSignedJWT :: ByteString
  } deriving (Eq, Show, Read, Ord)

newtype Email = Email
  { unEmail :: Text
  } deriving (Eq, Show, Read, Ord)

data Scope
  = ScopeCalendarFull
  | ScopeCalendarRead
  | ScopeGmailFull
  | ScopeGmailSend
  | ScopeDriveFile
  | ScopeDriveMetadataRead
  | ScopeSpreadsheets
  deriving (Eq, Show, Read, Ord)

{-| Make sure if you added new scope, update configuration in page bellow.
  https://admin.google.com/uzuz.jp/AdminHome?chromeless=1#OGX:ManageOauthClients
-}
scopeUrl :: Scope -> Text
scopeUrl ScopeCalendarFull = "https://www.googleapis.com/auth/calendar"
scopeUrl ScopeCalendarRead = "https://www.googleapis.com/auth/calendar.readonly"
scopeUrl ScopeGmailSend = "https://www.googleapis.com/auth/gmail.send"
scopeUrl ScopeGmailFull = "https://www.googleapis.com/auth/gmail"
scopeUrl ScopeDriveFile = "https://www.googleapis.com/auth/drive.file"
scopeUrl ScopeDriveMetadataRead = "https://www.googleapis.com/auth/drive.metadata.readonly"
scopeUrl ScopeSpreadsheets = "https://www.googleapis.com/auth/spreadsheets"

-- | Get the private key obtained from the
-- Google API Console from a PEM 'String'.
--
-- >fromPEMString "-----BEGIN PRIVATE KEY-----\nB9e [...] bMdF\n-----END PRIVATE KEY-----\n"
-- >
fromPEMString :: String -> IO PrivateKey
fromPEMString s =
  fromJust . toKeyPair <$> readPrivateKey s PwNone >>= \k ->
    return
      PrivateKey
        { private_pub =
            PublicKey
              {public_size = rsaSize k, public_n = rsaN k, public_e = rsaE k}
        , private_d = rsaD k
        , private_p = rsaP k
        , private_q = rsaQ k
        , private_dP = 0
        , private_dQ = 0
        , private_qinv = 0
        }

-- | Create the signed JWT ready for transmission
-- in the access token request as assertion value.
--
-- >grant_type=urn%3Aietf%3Aparams%3Aoauth%3Agrant-type%3Ajwt-bearer&assertion=
--
getSignedJWT ::
     JWT
  -> Maybe Email
  -- ^ The email address of the user for which the
  -- application is requesting delegated access.
  -> [Scope]
  -- ^ The list of the permissions that the application requests.
  -> Maybe Int
  -- ^ Expiration time (maximum and default value is an hour, 3600).
  -> IO (Either String SignedJWT) -- ^ Either an error message or a signed JWT.
getSignedJWT JWT {..} msub scs mxt = do
  let xt = fromIntegral (fromMaybe 3600 mxt)
  unless (xt >= 1 && xt <= 3600) (fail "Bad expiration time")
  t <- getUnixTime
  let i =
        mconcat
          [ header
          , "."
          , toB64 $
            mconcat
              [ "{\"iss\":\"" <> unEmail clientEmail <> "\","
              , maybe mempty (\(Email sub) -> "\"sub\":\"" <> sub <> "\",") msub
              , "\"scope\":\"" <> T.intercalate " " (map scopeUrl scs) <> "\","
              , "\"aud\":\"https://www.googleapis.com/oauth2/v4/token\","
              , "\"exp\":" <> toT (utSeconds t + CTime xt) <> ","
              , "\"iat\":" <> toT (utSeconds t) <> "}"
              ]
          ]
  return $
    either
      (\err -> Left $ "RSAError: " <> show err)
      (\s -> return $ SignedJWT $ i <> "." <> encode (toStrict s))
      (rsassa_pkcs1_v1_5_sign hashSHA256 privateKey $ fromStrict i)
  where
    toT = T.pack . show
    header = toB64 "{\"alg\":\"RS256\",\"typ\":\"JWT\"}"

toB64 :: Text -> ByteString
toB64 = encode . encodeUtf8
