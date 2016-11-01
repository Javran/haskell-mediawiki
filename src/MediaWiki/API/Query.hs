module MediaWiki.API.Query
  ( CategoryInfo(..)
  , CategoryInfoRequest(..)
  , CategoryInfoResponse(..)

  , AllCategoriesRequest(..)
  , AllCategoriesResponse(..)

  , ImageInfo(..)
  , ImageInfoRequest(..)
  , ImageInfoResponse(..)

  , AllImagesRequest(..)
  , AllImagesResponse(..)
  ) where

import Data.Default
import Data.Maybe
import Control.Monad

import MediaWiki.API.Types
import MediaWiki.API.Utils

import Text.XML.Light.Types
import Text.XML.Light.Proc ( strContent )

data CategoryInfoRequest = CategoryInfoRequest

instance APIRequest CategoryInfoRequest where
    queryKind _ = QProp "categoryinfo"
    showReq _ = []

instance Default CategoryInfoRequest where
    def = CategoryInfoRequest

data CategoryInfoResponse
 = CategoryInfoResponse
     { ciPages :: [CategoryInfo]
     }

instance Default CategoryInfoResponse where
    def = CategoryInfoResponse
          { ciPages = []
          }

data CategoryInfo
  = CategoryInfo
     { ciPage       :: PageTitle
     , ciSize       :: Maybe Int
     , ciPageSize   :: Maybe Int
     , ciFiles      :: Maybe Int
     , ciSubCats    :: Maybe Int
     , ciHidden     :: Bool
     }

instance Default CategoryInfo where
    def = CategoryInfo
        { ciPage = emptyPageTitle
        , ciSize = Nothing
        , ciPageSize = Nothing
        , ciFiles = Nothing
        , ciSubCats = Nothing
        , ciHidden = False
        }

instance FromXml CategoryInfoResponse where
    fromXml e = do
        guard (elName e == nsName "api")
        let es1 = children e
        p  <- pNode "query" es1
        let es = children p
        ps <- (mapMaybe xmlPage . children) <$> pNode "pages" es
        return def {ciPages=ps}
      where
        xmlPage :: Element -> Maybe CategoryInfo
        xmlPage e' = do
            guard (elName e' == nsName "page")
            let ns     = fromMaybe "0" $ pAttr "ns" e'
            let tit    = fromMaybe ""  $ pAttr "title" e'
            let pid    = pAttr "pageid" e'
            let pg     = emptyPageTitle{pgNS=ns,pgTitle=tit,pgMbId=pid}
            let cs = mapMaybe (xmlCategoryInfo pg "categoryinfo") (children e')
            listToMaybe cs

xmlCategoryInfo :: PageTitle -> String -> Element -> Maybe CategoryInfo
xmlCategoryInfo pg tg e = do
   guard (elName e == nsName tg)
   return def
       { ciPage = pg
       , ciSize = pAttr "size" e >>= readMb
       , ciPageSize = pAttr "pagesize" e >>= readMb
       , ciFiles = pAttr "files" e >>= readMb
       , ciSubCats = pAttr "subcats" e >>= readMb
       , ciHidden = isJust (pAttr "hidden" e)
       }

data AllCategoriesRequest
  = AllCategoriesRequest
    { acFrom            :: Maybe PageName
    , acPrefix          :: Maybe PageName
    , acDir             :: Maybe Direction
    , acLimit           :: Maybe Int
    , acProp            :: [String]
    }

instance APIRequest AllCategoriesRequest where
    queryKind _ = QList "allcategories"
    showReq r =
        [ mbOpt "acfrom" id (acFrom r)
        , mbOpt "acprefix" id (acPrefix r)
        , mbOpt "acdir" (\ x -> if x == Up then "ascending" else "descending")
          (acDir r)
        , mbOpt "aclimit" show (acLimit r)
        , opt1   "acprop" (acProp r)
        ]

instance Default AllCategoriesRequest where
    def = AllCategoriesRequest
        { acFrom            = Nothing
        , acPrefix          = Nothing
        , acDir             = Nothing
        , acLimit           = Nothing
        , acProp            = []
        }

data AllCategoriesResponse
  = AllCategoriesResponse
    { acCategories :: [CategoryInfo]
    , acContinue   :: Maybe String
    }

instance Default AllCategoriesResponse where
    def = AllCategoriesResponse
        { acCategories = []
        , acContinue   = Nothing
        }

instance FromXml AllCategoriesResponse where
    fromXml e = do
        guard (elName e == nsName "api")
        let es1 = children e
        p  <- pNode "query" es1
        let es = children p
        ps <- (mapMaybe xmlCII . children) <$> pNode "allcategories" es
        let cont = pNode "query-continue" es1 >>= xmlContinue "allcategories" "acfrom"
        return def {acCategories=ps,acContinue=cont}
      where
        xmlCII :: Element -> Maybe CategoryInfo
        xmlCII e' = do
            c <- xmlCategoryInfo emptyPageTitle "c" e'
            let tit = strContent e'
            return c{ciPage=(ciPage c){pgTitle=tit}}

data ImageInfo
  = ImageInfo
    { iiTimestamp :: Timestamp
    , iiUser      :: UserName
    , iiWidth     :: Maybe Int
    , iiHeight    :: Maybe Int
    , iiSize      :: Maybe Int
    , iiURL       :: Maybe URLString
    , iiComment   :: Maybe String
    , iiSHA1      :: Maybe String
    , iiArchive   :: Maybe String
    , iiBitDepth  :: Maybe Int
    , iiMime      :: Maybe String
    }

instance Default ImageInfo where
    def = ImageInfo
        { iiTimestamp = nullTimestamp
        , iiUser      = nullUser
        , iiWidth     = Nothing
        , iiHeight    = Nothing
        , iiSize      = Nothing
        , iiURL       = Nothing
        , iiComment   = Nothing
        , iiSHA1      = Nothing
        , iiArchive   = Nothing
        , iiBitDepth  = Nothing
        , iiMime      = Nothing
        }

data ImageInfoRequest
  = ImageInfoRequest
    { iiProp      :: [PropKind]
    , iiLimit     :: Maybe Int
    , iiStart     :: Maybe Timestamp
    , iiEnd       :: Maybe Timestamp
    , iiURLSize   :: Maybe (Int,Int)
    }

instance APIRequest ImageInfoRequest where
    queryKind _ = QProp "imageinfo"
    showReq r =
        [ opt1 "iiprop" (map prKind $ iiProp r)
        , mbOpt "iilimit" show (iiLimit r)
        , mbOpt "iistart" id (iiStart r)
        , mbOpt "iiend" id (iiEnd r)
        , mbOpt "iiurlwidth" (show.fst) (iiURLSize r)
        , mbOpt "iiurlheight" (show.snd) (iiURLSize r)
        ]

instance Default ImageInfoRequest where
    def = ImageInfoRequest
        { iiProp      = []
        , iiLimit     = Nothing
        , iiStart     = Nothing
        , iiEnd       = Nothing
        , iiURLSize   = Nothing
        }

data ImageInfoResponse
  = ImageInfoResponse
    { iiPages    :: [(PageTitle,[ImageInfo])]
    , iiContinue :: Maybe String
    }

instance Default ImageInfoResponse where
    def = ImageInfoResponse
         { iiPages = []
         , iiContinue = Nothing
         }

instance FromXml ImageInfoResponse where
    fromXml e = do
        guard (elName e == nsName "api")
        let es1 = children e
        p  <- pNode "query" es1 >>= pNode "pages" . children
        let es = children p
        ps <- (mapMaybe xmlPage . children) <$> pNode "page" es
        let cont = pNode "query-continue" es1 >>= xmlContinue "imageinfo" "iistart"
        return def {iiPages=ps,iiContinue=cont}
      where
        xmlPage :: Element -> Maybe (PageTitle,[ImageInfo])
        xmlPage e' = do
            guard (elName e' == nsName "page")
            let es = children e'
            p <- pNode "imageinfo" es
            let es1 = children p
            cs <- (mapMaybe (xmlImageInfo "ii") . children) <$> pNode "ii" es1
            let ns     = fromMaybe "0" $ pAttr "ns" p
            let tit    = fromMaybe ""  $ pAttr "title" p
            let mbpid  = pAttr "pageid" p
            -- let miss   = isJust (pAttr "missing" p)
            -- let rep    = pAttr "imagerepository" p
            return (emptyPageTitle{pgNS=ns,pgTitle=tit,pgMbId=mbpid}, cs)

xmlImageInfo :: String -> Element -> Maybe ImageInfo
xmlImageInfo tg p = do
   guard (elName p == nsName tg)
   return def
    { iiTimestamp = fromMaybe nullTimestamp (pAttr "timestamp" p)
    , iiUser      = fromMaybe nullUser (pAttr "user" p)
    , iiWidth     = pAttr "width" p >>= readI
    , iiHeight    = pAttr "height" p >>= readI
    , iiSize      = pAttr "size" p >>= readI
    , iiURL       = pAttr "url" p
    , iiComment   = pAttr "comment" p
    , iiSHA1      = pAttr "sha1" p
    , iiArchive   = pAttr "archivename" p
    , iiBitDepth  = pAttr "bitdepth" p >>= readI
    , iiMime      = pAttr "mime" p
    }
  where
    readI :: String -> Maybe Int
    readI s =
        case reads s of
            ((v,_):_) -> Just v
            _ -> Nothing

data AllImagesRequest
 = AllImagesRequest
    { aiFrom            :: Maybe PageName
    , aiPrefix          :: Maybe PageName
    , aiMinSize         :: Maybe Int
    , aiMaxSize         :: Maybe Int
    , aiLimit           :: Maybe Int
    , aiDir             :: Maybe Direction
    , aiSha1            :: Maybe String
    , aiSha1Base36      :: Maybe String
    , aiProp            :: [String]
    }
instance APIRequest AllImagesRequest where
  queryKind _ = QList "allimages"
  showReq r
   = [ mbOpt "aifrom" id (aiFrom r)
     , mbOpt "aiprefix" id (aiPrefix r)
     , mbOpt "aiminsize" show (aiMinSize r)
     , mbOpt "aimaxsize" show (aiMaxSize r)
     , mbOpt "ailimit" show (aiLimit r)
     , mbOpt "aidir"   (\ x -> if x == Up then "ascending" else "descending")
                       (aiDir r)
     , mbOpt "aisha1"  id (aiSha1 r)
     , mbOpt "aisha1base36" id (aiSha1Base36 r)
     , opt1  "aiprop" (aiProp r)
     ]

instance Default AllImagesRequest where
    def = AllImagesRequest
        { aiFrom            = Nothing
        , aiPrefix          = Nothing
        , aiMinSize         = Nothing
        , aiMaxSize         = Nothing
        , aiLimit           = Nothing
        , aiDir             = Nothing
        , aiSha1            = Nothing
        , aiSha1Base36      = Nothing
        , aiProp            = []
        }

data AllImagesResponse
 = AllImagesResponse
    { aiImages   :: [ImageInfo]
    , aiContinue :: Maybe String
    }

instance Default AllImagesResponse where
    def = AllImagesResponse
        { aiImages   = []
        , aiContinue = Nothing
        }

instance FromXml AllImagesResponse where
    fromXml e = do
        guard (elName e == nsName "api")
        let es1 = children e
        p  <- pNode "query" es1
        let es = children p
        ps <- (mapMaybe (xmlImageInfo "img") . children) <$> pNode "allimages" es
        let cont = pNode "query-continue" es1 >>= xmlContinue "allimages" "aifrom"
        return def {aiImages=ps,aiContinue=cont}
