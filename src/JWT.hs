module JWT
  ( JWT (..)
  , JWTError (..)
  , decodeJWT
  , getClaimsSet
  , getSubject
  ) where

import Control.Lens
import Crypto.JOSE
import Crypto.JWT (ClaimsSet (..), JWT (..), claimSub, getString, jwtClaimsSet)
import Data.Text (Text, pack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

data JWTError = ValidationError
              | JOSEError Text
  deriving (Show, Eq)

getClaimsSet :: JWT -> ClaimsSet
getClaimsSet = jwtClaimsSet

getSubject :: JWT -> Maybe Text
getSubject jwt =
  getClaimsSet jwt ^. claimSub >>= getString

decodeJWT :: Text -> Either JWTError JWT
decodeJWT encJWT =
  case decodeCompact (encode encJWT) of
    (Left err) -> Left $ JOSEError (pack . show $ err)
    (Right jwt) -> Right jwt
  where
    encode = TLE.encodeUtf8 . TL.fromStrict
