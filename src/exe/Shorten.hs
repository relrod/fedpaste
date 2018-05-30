{-# LANGUAGE OverloadedStrings #-}

module Shorten where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)

shorten :: T.Text -> IO String
shorten url = do
  manager <- newManager tlsManagerSettings
  request' <- parseRequest ("https://da.gd/s")
  let request =
        setQueryString [ ("url", Just (T.encodeUtf8 url))
                       , ("strip", Nothing)
                       ] request'
  response <- httpLbs request manager
  return . BL.unpack . responseBody $ response
