--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Base
-- Description : Collector module of types used by the MediaWiki API
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Collector module of types used by the MediaWiki API
--
--------------------------------------------------------------------
module MediaWiki.API.Base
	( module MediaWiki.API.Base
	, module MediaWiki.API.Query.AllLinks
	, module MediaWiki.API.Query.AllMessages
	, module MediaWiki.API.Query.AllPages
	, module MediaWiki.API.Query.AllUsers
	, module MediaWiki.API.Query.BackLinks
	, module MediaWiki.API.Query.Blocks
	, module MediaWiki.API.Query.Categories
	, module MediaWiki.API.Query.CategoryMembers
	, module MediaWiki.API.Query.DeletedRevisions
	, module MediaWiki.API.Query.EmbeddedIn
	, module MediaWiki.API.Query.ExternalLinks
	, module MediaWiki.API.Query.ExternalURLUsage
	, module MediaWiki.API.Query.Images
	, module MediaWiki.API.Query.ImageUsage
	, module MediaWiki.API.Query.Info
	, module MediaWiki.API.Query.LangLinks
	, module MediaWiki.API.Query.LogEvents
	, module MediaWiki.API.Query.Random
	, module MediaWiki.API.Query.RecentChanges
	, module MediaWiki.API.Query.Revisions
	, module MediaWiki.API.Query.Search
	, module MediaWiki.API.Query.SiteInfo
	, module MediaWiki.API.Query.Templates
	, module MediaWiki.API.Query.UserContribs
	, module MediaWiki.API.Query.UserInfo
	, module MediaWiki.API.Query.Users
	, module MediaWiki.API.Query.WatchList

	, module MediaWiki.API.Query
        , module MediaWiki.API.Action

	) where

import MediaWiki.API.Types

import MediaWiki.API.Query
import MediaWiki.API.Query.AllLinks
import MediaWiki.API.Query.AllMessages
import MediaWiki.API.Query.AllPages
import MediaWiki.API.Query.AllUsers
import MediaWiki.API.Query.BackLinks
import MediaWiki.API.Query.Blocks
import MediaWiki.API.Query.Categories
import MediaWiki.API.Query.CategoryMembers
import MediaWiki.API.Query.DeletedRevisions
import MediaWiki.API.Query.EmbeddedIn
import MediaWiki.API.Query.ExternalLinks
import MediaWiki.API.Query.ExternalURLUsage
import MediaWiki.API.Query.Images
import MediaWiki.API.Query.ImageUsage
import MediaWiki.API.Query.Info
import MediaWiki.API.Query.LangLinks
import MediaWiki.API.Query.Links
import MediaWiki.API.Query.LogEvents
import MediaWiki.API.Query.Random
import MediaWiki.API.Query.RecentChanges
import MediaWiki.API.Query.Revisions
import MediaWiki.API.Query.Search
import MediaWiki.API.Query.SiteInfo
import MediaWiki.API.Query.Templates
import MediaWiki.API.Query.UserContribs
import MediaWiki.API.Query.UserInfo
import MediaWiki.API.Query.Users
import MediaWiki.API.Query.WatchList

import MediaWiki.API.Action
import Data.Default

-- | Type collecting together the main parts of a MediaWiki API request.
data Request
 = Request
    { reqAction :: Action
    , reqFormat :: Format
    , reqMaxLag :: Maybe Int
    }

emptyRequest :: Action -> Format -> Request
emptyRequest a f
 = Request
    { reqAction = a
    , reqFormat = f
    , reqMaxLag = Nothing
    }

emptyXmlRequest :: Action -> Request
emptyXmlRequest a = emptyRequest a xmlFormat

data Action
 = Sitematrix
 | Login           LoginRequest
 | Logout
 | Query           QueryRequest [String]
 | ExpandTemplates ExpandTemplatesRequest
 | Parse           ParseRequest
 | OpenSearch      OpenSearchRequest
 | FeedWatch       FeedWatchListRequest
 | Help            HelpRequest
 | ParamInfo       ParamInfoRequest
 | Unblock         UnblockRequest
 | Watch           WatchRequest
 | EmailUser       EmailUserRequest
 | Edit            EditRequest
 | Move            MoveRequest
 | Block           BlockRequest
 | Protect         ProtectRequest
 | Undelete        UndeleteRequest
 | Delete          DeleteRequest
 | Rollback        RollbackRequest
 | OtherAction     String [ValueName]


infoRequest :: InfoRequest
infoRequest = emptyInfoRequest

revisionRequest :: RevisionRequest
revisionRequest  = emptyRevisionRequest

linksRequest :: LinksRequest
linksRequest = emptyLinksRequest

langLinksRequest :: LangLinksRequest
langLinksRequest  = emptyLangLinksRequest

imagesRequest :: ImagesRequest
imagesRequest = emptyImagesRequest

imageInfoRequest :: ImageInfoRequest
imageInfoRequest = def

templatesRequest :: TemplatesRequest
templatesRequest = emptyTemplatesRequest

categoriesRequest :: CategoriesRequest
categoriesRequest = emptyCategoriesRequest

allCategoriesRequest :: AllCategoriesRequest
allCategoriesRequest = def

allImagesRequest :: AllImagesRequest
allImagesRequest = def

allLinksRequest :: AllLinksRequest
allLinksRequest  = emptyAllLinksRequest

allMessagesRequest :: AllMessagesRequest
allMessagesRequest = emptyAllMessagesRequest

allPagesRequest :: AllPagesRequest
allPagesRequest = emptyAllPagesRequest

allUsersRequest :: AllUsersRequest
allUsersRequest = emptyAllUsersRequest

backLinksRequest :: BackLinksRequest
backLinksRequest = emptyBackLinksRequest

embeddedInRequest :: EmbeddedInRequest
embeddedInRequest = emptyEmbeddedInRequest

imageUsageRequest :: ImageUsageRequest
imageUsageRequest = emptyImageUsageRequest

categoryInfoRequest :: CategoryInfoRequest
categoryInfoRequest = def

categoryMembersRequest :: CategoryMembersRequest
categoryMembersRequest = emptyCategoryMembersRequest

externalLinksRequest :: ExternalLinksRequest
externalLinksRequest = emptyExternalLinksRequest

externalURLUsageRequest :: ExternalURLUsageRequest
externalURLUsageRequest = emptyExternalURLUsageRequest

logEventsRequest :: LogEventsRequest
logEventsRequest = emptyLogEventsRequest

recentChangesRequest :: RecentChangesRequest
recentChangesRequest = emptyRecentChangesRequest

searchRequest :: String -> SearchRequest
searchRequest = emptySearchRequest

siteInfoRequest :: SiteInfoRequest
siteInfoRequest = emptySiteInfoRequest

userContribsRequest :: UserContribsRequest
userContribsRequest = emptyUserContribsRequest

userInfoRequest :: UserInfoRequest
userInfoRequest = emptyUserInfoRequest

watchListRequest :: WatchListRequest
watchListRequest = emptyWatchListRequest

blocksRequest :: BlocksRequest
blocksRequest = emptyBlocksRequest

deletedRevisionsRequest :: DeletedRevisionsRequest
deletedRevisionsRequest = emptyDeletedRevisionsRequest

usersRequest :: UsersRequest
usersRequest = emptyUsersRequest

randomPagesRequest :: RandomPagesRequest
randomPagesRequest = emptyRandomPagesRequest

data QueryRequestKind
 = InfoProp          InfoRequest
 | RevisionsProp     RevisionRequest
 | LinksPropProp     LinksRequest
 | LangLinksProp     LangLinksRequest
 | ImagesProp        ImagesRequest
 | ImageInfoProp     ImageInfoRequest
 | TemplatesProp     TemplatesRequest
 | CategoriesProp    CategoriesRequest
 | AllCategoriesProp AllCategoriesRequest
 | AllImagesProp     AllImagesRequest
 | AllLinksProp      AllLinksRequest
 | AllMessagesProp   AllMessagesRequest
 | AllPagesProp      AllPagesRequest
 | AllUsersProp      AllUsersRequest
 | BacklinksProp     BackLinksRequest
 | EmbeddedInProp    EmbeddedInRequest
 | ImageUsageProp    ImageUsageRequest
 | CategoryInfoProp  CategoryInfoRequest
 | CategoryMembersProp  CategoryMembersRequest
 | ExternalLinksProp    ExternalLinksRequest
 | ExternalURLUsageProp ExternalURLUsageRequest
 | LogEventsProp        LogEventsRequest
 | RecentChangesProp    RecentChangesRequest
 | SearchProp           SearchRequest
 | SiteInfoProp         SiteInfoRequest
 | UserContribsProp     UserContribsRequest
 | UserInfoProp         UserInfoRequest
 | WatchListProp        WatchListRequest
 | BlocksProp           BlocksRequest
 | DeletedRevsProp      DeletedRevisionsRequest
 | UsersProp            UsersRequest
 | RandomProp           RandomPagesRequest

qKind :: QueryRequestKind -> QueryKind
qKind q =
 case q of
   InfoProp k -> queryKind k
   RevisionsProp k -> queryKind k
   LinksPropProp k -> queryKind k
   LangLinksProp k -> queryKind k
   ImagesProp    k -> queryKind k
   ImageInfoProp k -> queryKind k
   TemplatesProp k -> queryKind k
   CategoriesProp k -> queryKind k
   AllCategoriesProp k -> queryKind k
   AllImagesProp     k -> queryKind k
   AllLinksProp      k -> queryKind k
   AllMessagesProp   k -> queryKind k
   AllPagesProp      k -> queryKind k
   AllUsersProp      k -> queryKind k
   BacklinksProp     k -> queryKind k
   EmbeddedInProp    k -> queryKind k
   ImageUsageProp    k -> queryKind k
   CategoryInfoProp  k -> queryKind k
   CategoryMembersProp k -> queryKind k
   ExternalLinksProp   k -> queryKind k
   ExternalURLUsageProp k -> queryKind k
   LogEventsProp        k -> queryKind k
   RecentChangesProp    k -> queryKind k
   SearchProp           k -> queryKind k
   SiteInfoProp         k -> queryKind k
   UserContribsProp     k -> queryKind k
   UserInfoProp         k -> queryKind k
   WatchListProp        k -> queryKind k
   BlocksProp           k -> queryKind k
   DeletedRevsProp      k -> queryKind k
   UsersProp            k -> queryKind k
   RandomProp           k -> queryKind k
