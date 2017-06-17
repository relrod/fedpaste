{-# LANGUAGE OverloadedStrings #-}
module Web.Modernpaste.Client where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)
import Web.Modernpaste.Types

-- | This is the most convenient way to call into the modernpaste API.
--
-- Given a 'Modernpaste' config, an API path, and a JSONifyable object (anything
-- with a 'ToJSON' instance defined on it), we POST to the API and try to parse
-- and return the response.
--
-- If parsing fails (e.g. we get JSON that doesn't match what we expect, or
-- is malformed), we return 'Nothing'. Otherwise, we return a
-- 'ModernpasteResponse'.
--
-- If we parse but we discover that we get an error back, we return an
-- appropriate 'MPError'. Otherwise, we return an 'MPSuccess' with the decoded
-- result.
modernpastePost
  :: (ToJSON a, FromJSON b)
  => Modernpaste -- ^ Modernpaste site configuration
  -> String -- ^ The full path of the endpoint
  -> a -- ^ The data to send to the endpoint
  -> IO (Maybe (ModernpasteResponse b)) -- ^ Data we get back from the endpoint
modernpastePost (Modernpaste baseurl) path obj = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest (baseurl ++ path)
  let request' =
        request { method = "POST"
                , requestBody = RequestBodyLBS $ encode obj
                , requestHeaders = [("Content-Type", "application/json")]
                }
  --print request'
  response <- httpLbs request' manager
  --print response
  return . decode . responseBody $ response

-- | Create a paste.
createPaste
  :: Modernpaste -- ^ Modernpaste site configuration
  -> CreatePasteRequest -- ^ The paste data
  -> IO (Maybe (ModernpasteResponse CreatePasteResponse)) -- ^ Data we get back
createPaste mp = modernpastePost mp "/api/paste/submit"
