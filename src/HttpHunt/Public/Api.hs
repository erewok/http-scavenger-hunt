{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module HttpHunt.Public.Api where

import           Data.Aeson
import           Data.Text          (Text)
import qualified Data.Text          as Text
import qualified Data.UUID          as UUID
import           RIO                hiding (Handler)
import qualified RIO.HashMap        as HM
import qualified RIO.Vector         as V
import           Servant
import           Servant.HTML.Blaze (HTML)

import           HttpHunt.Config
import           HttpHunt.Redis     as DB
import           HttpHunt.Types


type PublicHuntApi =
    -- list all admin endpoints
     "public" :> "endpoints" :> Header "TeamName" Text
        :> Get '[JSON] Value
    -- list all articles
    :<|> "public" :> "articles" :> Header "TeamName" Text
        :> Get '[JSON] [Article]
    -- describe article JSON shape
    :<|> "public" :> "articles" :> "describe" :> Header "TeamName" Text
        :> Get '[JSON] Value
    -- describe article comment JSON shape
    :<|> "public" :> "articles" :> "comments" :> "describe" :> Header "TeamName" Text
        :> Get '[JSON] Value
    -- get a particular article
    :<|> "public" :> "articles" :> Header "TeamName" Text
        :> Capture "postId" UUID.UUID  :> Get '[JSON] Article

publicHttpHuntApi :: ServerT PublicHuntApi HttpHuntApp
publicHttpHuntApi =
    getEndpointsH
    :<|> listArticlesH
    :<|> desribeArticleH
    :<|> desribeCommentH
    :<|> getArticleDetailH


getEndpointsH :: Maybe Text -> HttpHuntApp Value
getEndpointsH Nothing = do
    conn <- asks _getRedisConn
    let (Object initialResponse) = noTeamNameResponse
    let updatedR = HM.insert "data" (toJSON publicEdpoints) initialResponse
    return $ toJSON updatedR
getEndpointsH (Just teamName) = do
    let method = "GET"
        endpoint = PublicEndpoints
    conn <- asks _getRedisConn
    liftIO $ DB.upsertScoreCard conn teamName endpoint method
    return $ toJSON publicEdpoints

listArticlesH :: Maybe Text -> HttpHuntApp [Article]
listArticlesH Nothing = do
    conn <- asks _getRedisConn
    liftIO $ getAllPosts conn
listArticlesH (Just teamName) = do
    let method = "GET"
        endpoint = PublicArticleList
    conn <- asks _getRedisConn
    articles <- liftIO $ getAllPosts conn
    scorecard <- liftIO $ DB.upsertScoreCard conn teamName endpoint method
    return articles

desribeArticleH :: Maybe Text -> HttpHuntApp Value
desribeArticleH Nothing = pure articleDescriptionValue
desribeArticleH (Just teamName) = do
    let method = "GET"
        endpoint = PublicArticleDescribe
    pure articleDescriptionValue

articleDescriptionValue :: Value
articleDescriptionValue = Object $ HM.fromList [
    ("type", String "object"),
    ("required", Array $ V.fromList [
        "content", "author", "title"
    ]),
    ("properties", Object $ HM.fromList [
        ( "pubdate", Object $ HM.fromList [("type", "String")]),
        ( "content", Object $ HM.fromList [("type", "String")]),
        ( "author", Object $ HM.fromList [("type", "String")])])
    ]

desribeCommentH :: Maybe Text -> HttpHuntApp Value
desribeCommentH Nothing = pure commentDescriptionValue
desribeCommentH (Just teamName) = do
    let method = "GET"
        endpoint = PublicCommentDescribe
    pure commentDescriptionValue

commentDescriptionValue :: Value
commentDescriptionValue = Object $ HM.fromList [
    ("type", String "object"),
    ("required", Array $ V.fromList [
        "comment", "author", "articleId"
    ]),
    ("properties", Object $ HM.fromList [
        ( "articleId", Object $ HM.fromList [("type", "String")]),
        ( "comment", Object $ HM.fromList [("type", "String")]),
        ( "author", Object $ HM.fromList [("type", "String")])])
    ]



getArticleDetailH :: Maybe Text -> UUID.UUID -> HttpHuntApp Article
getArticleDetailH Nothing puid = do
    conn <- asks _getRedisConn
    liftIO $ getPost conn puid
getArticleDetailH (Just teamName) puid = do
    let method = "GET"
        endpoint = PublicArticleDetail
    conn <- asks _getRedisConn
    article <- liftIO $ getPost conn puid
    scorecard <- liftIO $ DB.upsertScoreCard conn teamName endpoint method
    return article


-- helpers, not handlers
noTeamNameResponse :: Value
noTeamNameResponse = Object $ HM.fromList [
    ("status", String "success")
    , ("message", String "missing 'TeamName' header, so you won't get any credit, but this is a public endpoint!")
    ]

publicEdpoints :: Value
publicEdpoints =  Array $ V.fromList [
    Object $ HM.fromList [("endpoint", String "public/endpoints"), ("method", String "GET"), ("payload",  Null), ("content types", String "json")]
    , Object $ HM.fromList [("endpoint", String "public/articles"), ("method", String "GET"), ("payload",  Null), ("content types", String "json")]
    , Object $ HM.fromList [("endpoint", String "public/articles/describe"), ("method", String "GET"), ("payload",  Null), ("content types", String "json")]
    , Object $ HM.fromList [("endpoint", String "public/articles/comments/describe"), ("method", String "GET"), ("payload",  Null), ("content types", String "json")]
    , Object $ HM.fromList [("endpoint", String "public/articles/{ARTICLE_ID}"), ("method", String "GET"), ("payload",  Null), ("content types", String "json")]
    ]

