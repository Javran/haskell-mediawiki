module MediaWiki.API.Action
  ( MoveRequest(..)
  , RollbackRequest(..)
  , DeleteRequest(..)
  , UndeleteRequest(..)
  , LoginRequest(..)
  , emptyLogin
  , LoginResponse(..)
  , ParamInfoRequest(..)
  , ParamInfoResponse(..)
  , APIModule(..)
  , ModuleParam(..)
  , ParamType(..)
  , BlockRequest(..)
  , UnblockRequest(..)
  , ParseRequest(..)
  , emptyParseRequest
  , ParseResponse(..)
  , LanguageLink(..)
  , CategoryLink(..)
  , Link(..)
  , TOCSection(..)
  , OpenSearchRequest(..)
  , emptyOpenSearchRequest
  , OpenSearchResponse(..)
  , OpenSearchHit(..)
  , ExpandTemplatesRequest(..)
  , ExpandTemplatesResponse(..)
  , FeedWatchListRequest(..)
  , FeedWatchListResponse(..)
  , FeedItem(..)
  , SitematrixRequest(..)
  , SitematrixResponse(..)
  , SiteSpecialInfo(..)
  , LanguageInfo(..)
  , SiteInfos(..)
  , WatchRequest(..)
  , EmailUserRequest(..)
  , EditRequest(..)
  , ProtectRequest(..)
  ) where

import Data.Default
import Text.XML.Light.Types
import Text.XML.Light.Proc ( strContent )
import Control.Monad
import Data.Maybe

import MediaWiki.API.Types
import MediaWiki.API.Utils

data MoveRequest
  = MoveRequest
    { mvFrom    :: Maybe PageName
    , mvTo      :: Maybe PageName
    , mvToken   :: Token
    , mvReason  :: Maybe String
    , mvMoveTalk :: Bool
    , mvNoRedir  :: Bool
    , mvWatch    :: Bool
    , mvUnwatch  :: Bool
    }

instance APIRequest MoveRequest where
    isPostable _ = True
    showReq r =
        [ mbOpt "from" id (mvFrom r)
        , mbOpt "to"   id (mvTo r)
        , opt   "token" (mvToken r)
        , mbOpt "reason" id (mvReason r)
        , optB "movetalk" (mvMoveTalk r)
        , optB "noredirect"  (mvNoRedir r)
        , optB "watch" (mvWatch r)
        , optB "unwatch" (mvUnwatch r)
        ]

instance Default MoveRequest where
    def = MoveRequest
        { mvFrom    = Nothing
        , mvTo      = Nothing
        , mvToken   = ""
        , mvReason  = Nothing
        , mvMoveTalk = False
        , mvNoRedir  = False
        , mvWatch    = False
        , mvUnwatch  = False
        }

data RollbackRequest
  = RollbackRequest
    { rbTitle :: PageName
    , rbUser  :: UserName
    , rbToken :: Token
    , rbSummary :: Maybe String
    , rbMarkBot :: Maybe Bool
    }

instance APIRequest RollbackRequest where
    isPostable _ = True
    showReq r =
        [ opt "title" (rbTitle r)
        , opt "user"  (rbUser r)
        , opt "token" (rbToken r)
        , mbOpt "summary" id (rbSummary r)
        , mbOpt "markbot" (\ x -> if x then "1" else "0") (rbMarkBot r)
        ]

instance Default RollbackRequest where
    def = RollbackRequest
        { rbTitle = ""
        , rbUser  = ""
        , rbToken = ""
        , rbSummary = Nothing
        , rbMarkBot = Nothing
        }

data DeleteRequest
  = DeleteRequest
    { delTitle  :: PageName
    , delToken  :: Token
    , delReason :: Maybe String
    , delWatch  :: Maybe Bool
    , delUnwatch :: Maybe Bool
    , delOldImage :: Maybe String
    }

instance APIRequest DeleteRequest where
    isPostable _ = True
    showReq r =
        [ opt "title" (delTitle r)
        , opt "token" (delToken r)
        , mbOpt "reason" id (delReason r)
        , optB "watch" (fromMaybe False $ delWatch r)
        , optB "unwatch" (fromMaybe False $ delUnwatch r)
        , mbOpt "oldimage" id (delOldImage r)
        ]

instance Default DeleteRequest where
    def = DeleteRequest
        { delTitle = ""
        , delToken = ""
        , delReason = Nothing
        , delWatch = Nothing
        , delUnwatch = Nothing
        , delOldImage = Nothing
        }

data UndeleteRequest
  = UndeleteRequest
    { udelTitle  :: PageName
    , udelToken  :: Token
    , udelReason :: Maybe String
    , udelTimestamps :: [Timestamp]
    }

instance APIRequest UndeleteRequest where
    isPostable _ = True
    showReq r =
        [ opt "title" (udelTitle r)
        , opt "token" (udelToken r)
        , mbOpt "reason" id (udelReason r)
        , opt1 "timestamps" (udelTimestamps r)
        ]

instance Default UndeleteRequest where
    def = UndeleteRequest
        { udelTitle = ""
        , udelToken = ""
        , udelReason = Nothing
        , udelTimestamps = []
        }

data LoginRequest
  = LoginRequest
    { lgName     :: User
    , lgPassword :: Password
    , lgDomain   :: Maybe String
    }

instance APIRequest LoginRequest where
    isPostable _ = True
    showReq r =
        [ opt "lgname" (lgName r)
        , opt "lgpassword" (lgPassword r)
        , mbOpt "lgdomain" id (lgDomain r)
        ]

emptyLogin :: User -> Password -> LoginRequest
emptyLogin u p = LoginRequest
    { lgName     = u
    , lgPassword = p
    , lgDomain   = Nothing
    }

data LoginResponse
    -- error responses, all together:
  = LoginError
    { lgSuccess  :: Bool
    , lgeError   :: String
      -- NeedToWait/NoName/Illegal/WrongPluginPass/NotExists/WrongPass/EmptyPass/future ones.
      -- (resist the temptation(?) to encode errors via a separate type,
      -- keep it loose for now in a String.
    , lgeDetails :: Maybe String
    , lgeWait    :: String
    }
    -- Successfully logged in, bundle up the result in a separate record so as to ease
    -- the carrying around of a user session.
    --
    -- Notice the use of a shared boolean field betw. the constructors; eases the processing of
    -- responses in code where conds/guards are more convenient:
    --
    --    x <- submitLoginRequest lgReq
    --    when (not $ lgSuccess x) (handleError x)
    --    let sess = lgSession x
    --    ...
    --
 | LoginResponse
    { lgSuccess :: Bool
    , lgSession :: UserSession
    }

instance FromXml LoginResponse where
    fromXml e = do
        guard (elName e == nsName "api")
        let es1 = children e
        p <- pNode "login" es1
        let res = pAttr "result" p
            uid = pAttr "lguserid" p
            unm = pAttr "lgusername" p
            tok = pAttr "lgtoken" p
            coo = pAttr "cookieprefix" p
            ses = pAttr "sessionid" p
        case res of
            Nothing -> fail "missing 'result' api attribute"
            Just "Success" ->
                return LoginResponse{ lgSuccess=True
                                    , lgSession=UserSession
                                         { sessUserId    = fromMaybe "" uid
                                         , sessUserName  = fromMaybe "" unm
                                         , sessPassword  = Nothing
                                         , sessCookiePrefix = coo
                                         , sessSessionId = ses
                                         , sessToken     =  fromMaybe "" tok
                                         }}
            Just x -> do
                let det = pAttr "details" p
                let wai = pAttr "wait" p
                return LoginError
                    { lgSuccess = False
                    , lgeError  = x
                    , lgeDetails = det
                    , lgeWait   = fromMaybe "" wai
                    }

data ParamInfoRequest
  = ParamInfoRequest
    { paModules      :: [String]
    , paQueryModules :: [String]
    }

instance Default ParamInfoRequest where
    def = ParamInfoRequest
        { paModules      = []
        , paQueryModules = []
        }

data ParamInfoResponse
  = ParamInfoResponse
    { parModules :: [APIModule]
    }

data APIModule
 = APIModule
   { modName        :: String
   , modClass       :: String
   , modDescription :: String
   , modParams      :: [ModuleParam]
   }

data ModuleParam
  = ModuleParam
    { modParamName        :: String
    , modParamDefault     :: String
    , modParamDescription :: String
    , modParamPrefix      :: String
    , modParamType        :: ParamType
    }

data ParamType
  = TypeBool
  | TypeString
  | TypeInteger
  | TypeTimestamp
  | TypeName String
  | TypeEnum [String]

data BlockRequest
  = BlockRequest
    { blkUser   :: UserName
    , blkToken  :: Token
    , blkGetToken :: Bool
    , blkExpiry :: Maybe Timestamp
    , blkReason :: Maybe String
    , blkAnonOnly :: Bool
    , blkNoCreate :: Bool
    , blkAutoBlock :: Bool
    , blkNoEmail :: Bool
    , blkHide    :: Bool
    }

instance APIRequest BlockRequest where
    isPostable _ = True
    showReq r =
        [ opt "user" (blkUser r)
        , opt "token" (blkToken r)
        , optB "gettoken" (blkGetToken r)
        , mbOpt "expiry" id (blkExpiry r)
        , mbOpt "reason" id (blkReason r)
        , optB "anononly" (blkAnonOnly r)
        , optB "nocreate" (blkNoCreate r)
        , optB "autoblock" (blkAutoBlock r)
        , optB "noemail"   (blkNoEmail r)
        , optB "hidename"  (blkHide r)
        ]

instance Default BlockRequest where
    def = BlockRequest
        { blkUser   = ""
        , blkToken  = ""
        , blkGetToken = False
        , blkExpiry = Nothing
        , blkReason = Nothing
        , blkAnonOnly = False
        , blkNoCreate = False
        , blkAutoBlock = False
        , blkNoEmail = False
        , blkHide    = False
        }

data UnblockRequest
  = UnblockRequest
    { ublkId    :: Maybe String
    , ublkUser   :: Maybe UserName
    , ublkToken  :: Maybe Token
    , ublkGetToken :: Bool
    , ublkReason :: Maybe String
    }

instance APIRequest UnblockRequest where
    isPostable _ = True
    showReq r =
        [ mbOpt "id" id (ublkId r)
        , mbOpt "user" id (ublkUser r)
        , mbOpt "token" id (ublkToken r)
        , optB "gettoken" (ublkGetToken r)
        , mbOpt "reason" id (ublkReason r)
        ]

instance Default UnblockRequest where
    def = UnblockRequest
        { ublkId = Nothing
        , ublkUser   = Nothing
        , ublkToken  = Nothing
        , ublkGetToken = False
        , ublkReason = Nothing
        }

data ParseRequest
  = ParseRequest
    { paTitle     :: Maybe PageName
    , paText      :: String
    , paPage      :: Maybe PageName
    , paOldId     :: Maybe RevID
    , paProp      :: [String]
    }

emptyParseRequest :: String -> ParseRequest
emptyParseRequest txt = ParseRequest
    { paTitle     = Nothing
    , paText      = txt
    , paPage      = Nothing
    , paOldId     = Nothing
    , paProp      = []
    }

data ParseResponse
  = ParseResponse
    { parText          :: String
    , parRevId         :: Maybe RevID
    , parLangLinks     :: Maybe [LanguageLink]
    , parCategories    :: Maybe [CategoryLink]
    , parLinks         :: Maybe [Link]
    , parTemplates     :: Maybe [Link]
    , parImages        :: Maybe [String]
    , parExternalLinks :: Maybe [URLString]
    , parSections      :: Maybe [TOCSection]
    }

instance Default ParseResponse where
    def = ParseResponse
        { parText          = ""
        , parRevId         = Nothing
        , parLangLinks     = Nothing
        , parCategories    = Nothing
        , parLinks         = Nothing
        , parTemplates     = Nothing
        , parImages        = Nothing
        , parExternalLinks = Nothing
        , parSections      = Nothing
        }


instance FromXml ParseResponse where
    fromXml e = do
          guard (elName e == nsName "api")
          let es1 = children e
          p  <- pNode "parse" es1
          let es = children p
              txt = fromMaybe "" (pNode "text" es >>= return.strContent)
              rev = pAttr "revid" p
              ll  = fmap (mapMaybe xmlLL) (fmap children $ pNode "langlinks" es)
              ca  = fmap (mapMaybe xmlCat) (fmap children $ pNode "categories" es)
              li  = fmap (mapMaybe xmlLi) (fmap children $ pNode "links" es)
              te  = fmap (mapMaybe xmlTe) (fmap children $ pNode "templates" es)
              im  = fmap (mapMaybe xmlIm) (fmap children $ pNode "images" es)
              ex  = fmap (mapMaybe xmlEx) (fmap children $ pNode "externallinks" es)
              se  = fmap (mapMaybe xmlSe) (fmap children $ pNode "sections" es)
          return def
              { parText = txt
              , parRevId = rev
              , parLangLinks = ll
              , parCategories = ca
              , parLinks = li
              , parTemplates = te
              , parImages = im
              , parExternalLinks = ex
              , parSections = se
              }
      where
        xmlLL :: Element -> Maybe LanguageLink
        xmlLL e = do
            guard (elName e == nsName "ll")
            let lng    = fromMaybe "en"  $ pAttr "lang" e
            return LanguageLink{laLang=lng,laLink=strContent e}

        xmlCat :: Element -> Maybe CategoryLink
        xmlCat e = do
            guard (elName e == nsName "cl")
            let sk    = fromMaybe ""  $ pAttr "sortkey" e
            return CategoryLink{caSortKey=sk,caLink=strContent e}

        xmlLi :: Element -> Maybe Link
        xmlLi e = do
            guard (elName e == nsName "pl")
            let ex   = isJust (pAttr "exists" e)
            let ns   = fromMaybe mainNamespace $ pAttr "ns" e
            return Link{liNamespace=ns,liExists=ex,liLink=strContent e}

        xmlTe :: Element -> Maybe Link
        xmlTe e = do
            guard (elName e == nsName "tl")
            let ex   = isJust (pAttr "exists" e)
            let ns   = fromMaybe mainNamespace $ pAttr "ns" e
            return Link{liNamespace=ns,liExists=ex,liLink=strContent e}

        xmlIm :: Element -> Maybe URLString
        xmlIm e = do
            guard (elName e == nsName "img")
            return (strContent e)

        xmlEx :: Element -> Maybe URLString
        xmlEx e = do
            guard (elName e == nsName "el")
            return (strContent e)

        xmlSe :: Element -> Maybe TOCSection
        xmlSe e = do
            guard (elName e == nsName "s")
            let tlev = fromMaybe 0 $ pAttr "toclevel" e >>= readMb
            let lev = fromMaybe 0 $ pAttr "level" e >>= readMb
            let lin = fromMaybe "" $ pAttr "line" e
            let num = fromMaybe "" $ pAttr "number" e
            return TOCSection{tocTocLevel=tlev,tocLevel=lev,tocLine=lin,tocNumber=num}

data LanguageLink
  = LanguageLink
    { laLang  :: String
    , laLink  :: String
    }

data CategoryLink
  = CategoryLink
    { caSortKey :: String
    , caLink    :: String
    }

data Link
  = Link
    { liNamespace :: String
    , liExists    :: Bool
    , liLink      :: String
    }

data TOCSection
  = TOCSection
    { tocTocLevel :: Int
    , tocLevel    :: Int
    , tocLine     :: String
    , tocNumber   :: String
    }

data OpenSearchRequest
  = OpenSearchRequest
    { osSearch     :: String
    , osLimit      :: Maybe Int
    , osNamespaces :: Maybe [Int]
    }

emptyOpenSearchRequest :: String -> OpenSearchRequest
emptyOpenSearchRequest tit
  = OpenSearchRequest
    { osSearch     = tit
    , osLimit      = Nothing
    , osNamespaces = Nothing
    }

data OpenSearchResponse
  = OpenSearchResponse
    { osHits :: [OpenSearchHit]
    }

data OpenSearchHit
  = OpenSearchHit
    { oshTitle   :: String
    , oshMatches :: [String]
    }

data FeedWatchListRequest
  = FeedWatchListRequest
    { feAsAtom    :: Bool  -- False => rss
    , feHours     :: Maybe Int
    , feAllRev    :: Bool
    }

instance Default FeedWatchListRequest where
    def = FeedWatchListRequest
        { feAsAtom    = False
        , feHours     = Nothing
        , feAllRev    = False
        }

data FeedWatchListResponse
  = FeedWatchListResponse
    { fwFeedFormat  :: String
    , fwFeedRaw     :: String
    , fwFeedItems   :: [FeedItem]
    }

data FeedItem
  = FeedItem
    { fiTitle     :: String
    , fiUrl       :: URLString
    , fiComment   :: String
    , fiTimestamp :: String
    , fiUser      :: String
    , fiText      :: String
    }

data ExpandTemplatesRequest
  = ExpandTemplatesRequest
    { etTitle      :: Maybe PageName
    , etText       :: String
    , etGenXml     :: Maybe Bool
    }

instance Default ExpandTemplatesRequest where
    def = ExpandTemplatesRequest
        { etTitle      = Nothing
        , etText       = ""
        , etGenXml     = Nothing
        }

data ExpandTemplatesResponse
  = ExpandTemplatesResponse
    { etExpandedText :: String
    , etExpandedXml  :: Maybe String
    }

instance Default ExpandTemplatesResponse where
    def = ExpandTemplatesResponse
        { etExpandedText  = ""
        , etExpandedXml   = Nothing
        }

instance FromXml ExpandTemplatesResponse where
    fromXml e = do
        guard (elName e == nsName "api")
        let es1 = children e
        p  <- pNode "expandtemplates" es1
        let xm = strContent <$> pNode "parsetree" es1
        return def
            { etExpandedText = strContent p
            , etExpandedXml  = xm
            }

-- 1.12+ and later
data SitematrixRequest
  = SitematrixRequest

instance Default SitematrixRequest where
    def = SitematrixRequest

data SitematrixResponse
  = SitematrixResponse
    { smCount     :: Int
    , smSpecials  :: [SiteSpecialInfo]
    , smLanguages :: [LanguageInfo]
    }

data SiteSpecialInfo
  = SiteSpecialInfo
    { siCode :: String
    , siUrl  :: URLString
    }

data LanguageInfo
  = LanguageInfo
    { liCode  :: String
    , liName  :: String
    , liSites :: [SiteInfos]
    }

data SiteInfos
  = SiteInfos
    { siInfo :: String }

data WatchRequest
 = WatchRequest
    { waTitle  :: PageName
    , waIsUnwatch :: Bool
    }

instance APIRequest WatchRequest where
    isPostable _ = True
    showReq r =
        [ opt "title" (waTitle r)
        , optB "unwatch" (waIsUnwatch r)
        ]

instance Default WatchRequest where
    def = WatchRequest
        { waTitle = ""
        , waIsUnwatch = False
        }

data EmailUserRequest
  = EmailUserRequest
    { emTarget  :: Maybe String
    , emSubject :: Maybe String
    , emText    :: Maybe String
    , emToken   :: Maybe Token
    , emCcMe    :: Bool
    }

instance APIRequest EmailUserRequest where
    isPostable _ = True
    showReq r =
        [ mbOpt "target" id (emTarget r)
        , mbOpt "subject" id (emSubject r)
        , mbOpt "text" id (emText r)
        , mbOpt "token" id (emToken r)
        , optB  "ccme" (emCcMe r)
        ]

instance Default EmailUserRequest where
    def = EmailUserRequest
        { emTarget  = Nothing
        , emSubject = Nothing
        , emText    = Nothing
        , emToken   = Nothing
        , emCcMe    = False
        }

data EditRequest
  = EditRequest
    { edTitle   :: Maybe PageName
    , edSection :: Maybe String
    , edText    :: Maybe String -- ^ Page content
    , edToken   :: Maybe Token  -- ^ Edit token. You can get one of these through prop=info
    , edSummary :: Maybe String
    , edIsMinor   :: Bool
    , edIsNonMinor :: Bool
    , edAsBot   :: Bool
    , edBaseTimestamp :: Maybe Timestamp
    , edRecreate :: Bool
    , edCreateOnly :: Bool
    , edNoCreate :: Bool
    , edCaptchaWord :: Maybe String
    , edCaptchaId   :: Maybe String
    , edWatch :: Bool
    , edUnwatch :: Bool
    , edMD5 :: Maybe String
    , edPrependText :: Maybe String
    , edAppendText :: Maybe String
    }

instance APIRequest EditRequest where
    isPostable _ = True
    showReq r =
        [ mbOpt "title" id (edTitle r)
        , mbOpt "section" id (edSection r)
        , mbOpt "text" id (edText r)
        , mbOpt "token" id (edToken r)
        , mbOpt "summary" id (edSummary r)
        , optB "minor" (edIsMinor r)
        , optB "notminor" (edIsNonMinor r)
        , optB "bot" (edAsBot r)
        , mbOpt "basetimestamp" id (edBaseTimestamp r)
        , optB  "recreate" (edRecreate r)
        , optB  "createonly" (edCreateOnly r)
        , optB  "nocreate" (edNoCreate r)
        , mbOpt "captchaword" id (edCaptchaWord r)
        , mbOpt "captchaid" id (edCaptchaId r)
        , optB  "watch" (edWatch r)
        , optB  "unwatch" (edUnwatch r)
        , mbOpt "md5" id (edMD5 r)
        , mbOpt "prependtext" id (edPrependText r)
        , mbOpt "appendtext" id (edAppendText r)
        ]


instance Default EditRequest where
    def = EditRequest
        { edTitle   = Nothing
        , edSection = Nothing
        , edText    = Nothing
        , edToken   = Nothing
        , edSummary = Nothing
        , edIsMinor   = False
        , edIsNonMinor = True
        , edAsBot   = False
        , edBaseTimestamp = Nothing
        , edRecreate = False
        , edCreateOnly = False
        , edNoCreate = False
        , edCaptchaWord = Nothing
        , edCaptchaId   = Nothing
        , edWatch = False
        , edUnwatch = False
        , edMD5 = Nothing
        , edPrependText = Nothing
        , edAppendText = Nothing
        }

data ProtectRequest
  = ProtectRequest
    { protTitle  :: PageName
    , protToken  :: Token
    , protProtections :: [(String,String)]
    , protExpiry :: Maybe Timestamp
    , protReason :: Maybe String
    , protCascade :: Maybe Bool
    }

instance APIRequest ProtectRequest where
    isPostable _ = True
    showReq r =
        [ opt "title" (protTitle r)
        , opt "token" (protToken r)
        , opt1 "protections" (map (\ (a,b) -> a ++ '=':b) (protProtections r))
        , mbOpt "expiry" id (protExpiry r)
        , mbOpt "reason" id (protReason r)
        , optB "cascade" (fromMaybe False $ protCascade r)
        ]

instance Default ProtectRequest where
    def = ProtectRequest
        { protTitle = ""
        , protToken = ""
        , protProtections = []
        , protExpiry = Nothing
        , protReason = Nothing
        , protCascade = Nothing
        }
