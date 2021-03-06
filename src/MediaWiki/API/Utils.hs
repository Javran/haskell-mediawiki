--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Utils
-- Description : MediaWiki API internal utility functions.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- MediaWiki API internal utility functions.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Utils
  ( module MediaWiki.API.Utils
  , fromMaybe
  ) where

import Text.XML.Light as XML
import Data.Maybe
import Data.List
import Control.Monad

import MediaWiki.Util.Codec.URLEncoder ( encodeString )

type ParseErrorMsgs = [String]
type ParseResult a = Either (String, ParseErrorMsgs) a

-- select nodes base on element names
pNodes :: String -> [XML.Element] -> [XML.Element]
pNodes x = filter ((n ==) . elName)
  where
    n = nsName x

-- select nodes base on element names, only the first hit returns
pNode :: String -> [XML.Element] -> Maybe XML.Element
pNode x es = listToMaybe (pNodes x es)

-- same as pNode, but extract the string content
pLeaf :: String -> [XML.Element] -> Maybe String
pLeaf x es = strContent <$> pNode x es

pAttr :: String -> XML.Element -> Maybe String
pAttr x e = lookup (nsName x) [ (k,v) | Attr k v <- elAttribs e ]

pMany :: String -> (XML.Element -> Maybe a) -> [XML.Element] -> [a]
pMany p f = mapMaybe f . pNodes p

children :: XML.Element -> [XML.Element]
children e = onlyElems (elContent e)

nsName :: String -> QName
nsName = unqual

without :: [String] -> [XML.Attr] -> [XML.Attr]
without xs = filter (\ a -> attrKey a `notElem` qxs)
  where
    qxs = map nsName xs

parseDoc :: (Element -> Maybe a) -> String -> ParseResult a
parseDoc f s =
    case parseXMLDoc s of
        Nothing -> Left (s, ["not valid XML content"])
        Just d  ->
            case f d of
                Nothing -> Left (s,["unexpected XML response"])
                Just x  -> Right x

xmlContinue :: String -> String -> Element -> Maybe String
xmlContinue tgName atName e = do
  guard (elName e == nsName "query-continue")
  let es1 = children e
  p  <- pNode tgName es1
  pAttr atName p

mbDef :: a -> Maybe a -> Maybe a
mbDef x Nothing = Just x
mbDef _ v = v

readMb :: Read a => String -> Maybe a
readMb x = case reads x of
             ((v,_):_) -> Just v
             _ -> Nothing

piped :: [String] -> String
piped = intercalate "|"

opt :: String -> String -> Maybe (String,String)
opt a b = Just (a, encodeString b)

optB :: String -> Bool -> Maybe (String,String)
optB _ False = Nothing
optB a _ = Just (a,"")

opt1 :: String -> [String] -> Maybe (String,String)
opt1 _ [] = Nothing
opt1 a b = Just (a,encodeString $ piped b)

mbOpt :: String -> (a -> String) -> Maybe a -> Maybe (String,String)
mbOpt tg f = fmap (\x -> (tg,encodeString $ f x))
