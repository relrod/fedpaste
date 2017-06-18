{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module : Web.Modernpaste.Client
-- Copyright : (C) 2017 Red Hat, Inc.
-- License : MIT (see LICENSE file)
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
-- Portability : OverloadedStrings
--
-- This module provides high-level access to the modernpaste JSON API.
--
-- In the 'Web.Modernpaste.Types' module, we introduce a monad transformer
-- stack, 'MPResponseT' which encapsulates responses from the API and threads
-- around the client configuration between functions for us by using ReaderT.
--
-- The simplest use of this library looks something like this:
--
-- @
--   let req =
--     createPaste (CreatePasteRequest "main = print 1234"
--                  Nothing
--                  (Just "Hello, Haskell!")
--                  (Just "haskell")
--                  Nothing)
--   cfg = Modernpaste "https://modernpaste.stg.fedoraproject.org"
--   runMPResponseT c req
-- @
------------------------------------------------------------------------------
module Web.Modernpaste.Client where

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
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
  => String -- ^ The full path of the endpoint
  -> a -- ^ The data to send to the endpoint
  -> MPResponseT (ModernpasteResponse b) -- ^ Data we get back from the endpoint
modernpastePost path obj = do
  Modernpaste baseurl <- ask
  manager <- liftIO $ newManager tlsManagerSettings
  request <- liftIO $ parseRequest (baseurl ++ path)
  let request' =
        request { method = "POST"
                , requestBody = RequestBodyLBS $ encode obj
                , requestHeaders = [("Content-Type", "application/json")]
                }
  --liftIO $ print request'
  response <- liftIO $ httpLbs request' manager
  --liftIO $ print response
  MPResponseT . lift . MaybeT . return . decode . responseBody $ response

runMPResponseT :: MPConfig -> MPResponseT a -> IO (Maybe a)
runMPResponseT c (MPResponseT a) = runMaybeT (runReaderT a c)

-- | Create a paste.
createPaste
  :: CreatePasteRequest -- ^ The paste data
  -> MPResponseT (ModernpasteResponse CreatePasteResponse) -- ^ Data we get back
createPaste = modernpastePost "/api/paste/submit"
