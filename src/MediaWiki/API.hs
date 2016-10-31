{-# LANGUAGE
    ExistentialQuantification
  , OverloadedStrings
  #-}
--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API
-- Description : A Haskell MediaWiki API binding
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- A Haskell MediaWiki API binding.
--
--------------------------------------------------------------------
module MediaWiki.API
  ( module MediaWiki.API
  , URLString
  ) where

import MediaWiki.API.Base
import MediaWiki.API.Types
import MediaWiki.API.Output

import MediaWiki.Util.Fetch as Fetch
import Codec.MIME.Type

import MediaWiki.API.Query.SiteInfo.Import as SI
import MediaWiki.API.Action.Login.Import as Login

import Data.Maybe
import Data.Text (unpack)

import Control.Exception as CE
import Data.Typeable

import MediaWiki.API.Utils
import Text.XML.Light.Types
import Control.Monad

-- | @webGet url req@ issues a GET to a MediaWiki server, appending
-- @api.php?@ followed by the request @req@ to the URL base @url@.
webGet :: URLString -> Request -> IO String
webGet url req = readContentsURL url_q
  where
    url_q = url ++ "api.php?" ++ showRequest req

-- | @webGet mbUser url req@ issues a POST to a MediaWiki server, appending
-- @api.php?@ followed by the request @req@ to the URL base @url@.
webPost :: Maybe Fetch.AuthUser -> URLString -> String -> Request -> IO ([(String,String)], String)
webPost mbUser url act req = do
    let url_q  = url ++ "api.php?action="++act
        pload  = showRequest req
    (_cookiesOut, hs, p) <-
        postContentsURL
            mbUser
            url_q
            [ ("Content-Type", unpack $ showMIMEType form_mime_ty) ]
            [{-no cookies..-}]
            pload
    return (hs, p)
 where
  form_mime_ty = Application "x-www-form-urlencoded"

webPostXml :: (String -> Either (String,[String]) a)
           -> Maybe Fetch.AuthUser
           -> URLString
           -> String
           -> Request
           -> IO (Maybe a)
webPostXml p mbUser url act req = do
    (_hs,mb) <- webPost mbUser url act req
    case mb of
        "" -> return Nothing
        ls ->
          case p ls of
              Left (x,errs) ->
                case parseError ls of
                    Right e -> throwMWError e
                    _ -> putStrLn (x ++ ':':' ':unlines errs) >> return Nothing
              Right x  -> return (Just x)

webGetXml :: (String -> Either (String,[String]) a)
          -> URLString
          -> Request
          -> IO (Maybe a)
webGetXml p url req = do
    ls <- webGet url req
    case p ls of
        Left (x,errs) ->
            case parseError ls of
                Right e -> throwMWError e
                _ -> putStrLn (x ++ ':':' ':unlines errs) >> return Nothing
        Right x  -> return (Just x)

queryPage :: PageName -> QueryRequest
queryPage pg = emptyQuery{quTitles=[pg]}

mkQueryAction :: APIRequest a => QueryRequest -> a -> Action
mkQueryAction q qr =
    case queryKind qr of
        QProp s -> mkQuery q{quProps=PropKind s:quProps q}
        QList s -> mkQuery q{quLists=ListKind s:quLists q}
        QMeta s -> mkQuery q{quMetas=MetaKind s:quMetas q}
        QGen  s -> mkQuery q{quGenerator=Just (GeneratorKind s)}
  where
    mkQuery q' = Query q' (toReq qr)

-- | @loginWiki u usr pass@ logs in to MediaWiki install at @url@ as
-- user @usr@ with password credentials @pass@. Notice that we don't
-- presently allow HTTP Auth to happen in conjunction with the Wiki
-- login.
loginWiki :: URLString -> String -> String -> IO (Maybe LoginResponse)
loginWiki url usr pwd = webPostXml Login.stringXml Nothing url "login" req
  where
    req = emptyXmlRequest (Login (emptyLogin usr pwd))

queryInfo :: URLString -> PageName -> IO String
queryInfo url pgName = webGet url req
  where
    req = emptyXmlRequest (mkQueryAction (queryPage pgName) infoRequest)

querySiteIWInfo :: URLString -> IO (Maybe SiteInfoResponse)
querySiteIWInfo url = webGetXml SI.stringXml url req
  where
    req = emptyXmlRequest
            (mkQueryAction (queryPage "XP")
             siteInfoRequest{siProp=["interwikimap"]})

queryLangPage :: URLString -> PageName -> Maybe String -> IO String
queryLangPage url pgName mb = webGet url req
  where
    req = emptyXmlRequest
            (mkQueryAction (queryPage pgName)
             langLinksRequest{llContinueFrom=mb})

parseError :: String -> Either (String,[{-Error msg-}String]) MediaWikiError
parseError = parseDoc xmlError

xmlError :: Element -> Maybe MediaWikiError
xmlError e = do
  guard (elName e == nsName "api")
  p  <- pNode "error" (children e)
  return mwError{ mwErrorCode = fromMaybe "" $ pAttr "code" p
                , mwErrorInfo = fromMaybe "" $ pAttr "info" p
                }

-- MW exceptions/errors:

data MediaWikiError
 = MediaWikiError
     { mwErrorCode :: String
     , mwErrorInfo :: String
     } deriving ( Typeable )

mwError :: MediaWikiError
mwError = MediaWikiError "" ""

data SomeMWException = forall e . Exception e => SomeMWException e
  deriving Typeable

instance Show SomeMWException where
    show (SomeMWException e) = show e

instance Exception SomeMWException

mwToException :: Exception e => e -> SomeException
mwToException = toException . SomeMWException

mwFromException :: Exception e => SomeException -> Maybe e
mwFromException x = do
    SomeMWException a <- fromException x
    cast a

instance Exception MediaWikiError where
    toException = mwToException
    fromException = mwFromException

throwMWError :: MediaWikiError -> IO a
throwMWError = throwIO

catchMW :: IO a -> (MediaWikiError -> IO a) -> IO a
catchMW = CE.catch

handleMW :: (MediaWikiError -> IO a) -> IO a -> IO a
handleMW h e = catchMW e h

tryMW :: IO a -> IO (Either MediaWikiError a)
tryMW f = handleMW (return . Left) (Right <$> f)

instance Show MediaWikiError where
    show x = unlines
               [ "MediaWiki error:"
               , ""
               , " Code: " ++ mwErrorCode x
               , " Info: " ++ mwErrorInfo x
               ]
