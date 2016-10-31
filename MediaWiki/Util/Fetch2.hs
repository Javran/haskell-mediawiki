module MediaWiki.Util.Fetch2
  ( readContentsURL
  , readUserContentsURL
  , postContentsURL
  , AuthUser (..)
  , nullAuthUser
  ) where

import MediaWiki.Util.Fetch (AuthUser(..), nullAuthUser, URLString)

import Network.HTTP.Types
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.ByteString.Lazy.Char8 as LBS

-- TODO

readContentsURL :: URLString -> IO String
readContentsURL u = do
    mgr <- newManager tlsManagerSettings
    req <- parseRequest u
    resp <- httpLbs req mgr
    let st = responseStatus resp
    if st == ok200
      -- quick and dirty
      then return (LBS.unpack . responseBody $ resp)
      else error $ "error with status code: " ++ show (statusCode st)

readUserContentsURL :: Maybe AuthUser -> Bool -> Bool -> URLString -> [(String,String)] -> IO ([(String,String)], String)
readUserContentsURL = undefined

postContentsURL :: Maybe AuthUser
                -> URLString
		-> [(String,String)]
		-> [cookie]
		-> String
		-> IO ([cookie],[(String,String)], String)
postContentsURL = undefined
