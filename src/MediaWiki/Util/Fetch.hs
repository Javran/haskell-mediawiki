{-# LANGUAGE OverloadedStrings #-}
module MediaWiki.Util.Fetch
  ( readContentsURL
  , postContentsURL
  , AuthUser (..)
  , nullAuthUser
  ) where

import Network.HTTP.Types
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import Data.String

type URLString = String

data AuthUser
  = AuthUser
    { authUserName :: String
    , authUserPass :: String
    }

nullAuthUser :: AuthUser
nullAuthUser = AuthUser
    { authUserName = ""
    , authUserPass = ""
    }

readContentsURL :: URLString -> IO String
readContentsURL u = do
    mgr <- newManager tlsManagerSettings
    req <- parseRequest u
    resp <- httpLbs req mgr
    let st = responseStatus resp
    if st == ok200
      -- TODO: quick and dirty, will be text in future.
      then return (LBS.unpack . responseBody $ resp)
      else error $ "error with status code: " ++ show (statusCode st)

postContentsURL :: Maybe AuthUser
                -> URLString
		-> [(String,String)]
		-> [Cookie]
		-> String
		-> IO ([Cookie],[(String,String)], String)
postContentsURL mbU u hdrs csIn body = do
    mgr <- newManager tlsManagerSettings
    req0 <- parseRequest u
    let req1 = req0 { requestHeaders =
                          requestHeaders req0
                          ++ map (\(x,y) -> (fromString x, fromString y)) hdrs
                    , method = "POST"
                    , requestBody = fromString body
                    , cookieJar = Just (createCookieJar csIn)
                    }
        req2 = case mbU of
            Nothing -> req1
            Just (AuthUser name pass) -> applyBasicAuth (fromString name) (fromString pass) req1
    resp <- httpLbs req2 mgr
    let st = responseStatus resp
    if st == ok200
      then do
        let csOut = destroyCookieJar (responseCookieJar resp)
        -- TODO: quick and dirty, will be text in future.
        -- TODO: we know "show" uses "original" so that's fine
        --       before we solve the problem of duplicating the dependency everywhere
        --       I don't like to add any new ones if possible.
        return (csOut, map (\(x,y) -> (show x,BS.unpack y)) . responseHeaders $ resp, LBS.unpack . responseBody $ resp)
      else error $ "error with status code: " ++ show (statusCode st)
