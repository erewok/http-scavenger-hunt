{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module HttpHunt.Admin.Api where

import           Data.Aeson
import           Data.Maybe      (fromJust)
import           Data.Text       (Text)
import qualified Data.Text       as Text
import           Data.Time.Clock (getCurrentTime)
import qualified Data.UUID       as UUID
import           Data.UUID.V4    (nextRandom)
import           RIO             hiding (Handler)
import qualified RIO.HashMap     as HM
import qualified RIO.Vector      as V
import           Servant

import           HttpHunt.Config
import           HttpHunt.Redis  as DB
import           HttpHunt.Types


type AdminHuntApi =
    -- list all admin endpoints
    "admin" :> "endpoints" :> Header "TeamName" Text
        :> Get '[JSON] Value
    -- create a new article
    :<|> "admin" :> "articles" :> Header "TeamName" Text
        :> ReqBody '[JSON] Article :> Post '[JSON] Value
    -- get a particular article
    :<|> "admin" :> "articles" :> Header "TeamName" Text
        :> Capture "articleId" UUID.UUID :> Get '[JSON] Value
    -- update a particular article
    :<|> "admin" :> "articles" :> Header "TeamName" Text
        :> Capture "articleId" UUID.UUID :> ReqBody '[JSON] Article
        :> Put '[JSON] Value
    -- delete a particular article
    :<|> "admin" :> "articles" :> Header "TeamName" Text
        :> Capture "articleId" UUID.UUID :> Delete '[JSON] Value
    -- Add a comment to a particular article
    :<|> "admin" :> "articles" :> "comments" :> Header "TeamName" Text
        :> Capture "articleId" UUID.UUID  :> ReqBody '[JSON] ArticleComment
        :> Post '[JSON] Value
    -- delete a comment from a particular article
    :<|> "admin" :> "articles" :> "comments" :> Header "TeamName" Text
        :> Capture "articleId" UUID.UUID :> ReqBody '[JSON] ArticleComment
        :> Delete '[JSON] Value

adminHttpHuntApi :: ServerT AdminHuntApi HttpHuntApp
adminHttpHuntApi =
    getAdminEndpointsH
    :<|> postNewArticleH
    :<|> getArticleDetailH
    :<|> putArticleH
    :<|> deleteArticleH
    :<|> createCommentH
    :<|> deleteCommentH

getAdminEndpointsH :: Maybe Text -> HttpHuntApp Value
getAdminEndpointsH Nothing = noTeamNameResponse
getAdminEndpointsH (Just teamName) = do
    let method = "GET"
        endpoint = AdminEndpoints
    conn <- asks _getRedisConn
    liftIO $ DB.upsertScoreCard conn teamName endpoint method
    return $ toJSON adminEdpoints

postNewArticleH :: Maybe Text -> Article -> HttpHuntApp Value
postNewArticleH Nothing _ = noTeamNameResponse
postNewArticleH (Just teamName) article = do
    let method = "POST"
        endpoint = AdminArticleList
    conn <- asks _getRedisConn
    article' <- liftIO $ newArticleHelper article
    result <- liftIO $ updateWholePost conn (fromJust $ article' ^. pid) article'
    scorecard <- liftIO $ DB.upsertScoreCard conn teamName endpoint method
    return $ Object $ HM.fromList [
        ("status", String "success")
        , ("currentScore", toJSON $ scorecard ^. totalScore)
        , ("author", String teamName)
        , ("data", toJSON result)
        ]

newArticleHelper :: Article -> IO Article
newArticleHelper article = do
    aid <- case article ^. pid of
            Nothing        -> Just <$> nextRandom
            Just articleId -> pure $ Just articleId
    dtime <- case article ^. pubdate of
            Nothing     -> Just <$> getCurrentTime
            Just dtime' -> pure $ Just dtime'
    return article{_pid = aid, _pubdate=dtime}


getArticleDetailH :: Maybe Text -> UUID.UUID -> HttpHuntApp Value
getArticleDetailH Nothing _ = noTeamNameResponse
getArticleDetailH (Just teamName) puid = do
    let method = "GET"
        endpoint = AdminArticleDetail
    conn <- asks _getRedisConn
    article <- liftIO $ getPost conn puid
    scorecard <- liftIO $ DB.upsertScoreCard conn teamName endpoint method
    return $ Object $ HM.fromList [
        ("status", String "success")
        , ("currentScore", toJSON $ scorecard ^. totalScore)
        , ("author", String teamName)
        , ("data", toJSON article)
        ]

putArticleH :: Maybe Text -> UUID.UUID -> Article -> HttpHuntApp Value
putArticleH Nothing _ _ = noTeamNameResponse
putArticleH (Just teamName) puid article = do
    let method = "PUT"
        endpoint = AdminArticleDetail
    conn <- asks _getRedisConn
    result <- liftIO $ patchPost conn puid article
    scorecard <- liftIO $ DB.upsertScoreCard conn teamName endpoint method
    return $ Object $ HM.fromList [
        ("status", String "success")
        , ("currentScore", toJSON $ scorecard ^. totalScore)
        , ("author", String teamName)
        , ("data", toJSON article)
        ]

deleteArticleH :: Maybe Text -> UUID.UUID -> HttpHuntApp Value
deleteArticleH Nothing _ = noTeamNameResponse
deleteArticleH (Just teamName) puid = do
    let method = "DELETE"
        endpoint = AdminArticleDetail
    conn <- asks _getRedisConn
    liftIO $ DB.deletePost conn puid
    scorecard <- liftIO $ DB.upsertScoreCard conn teamName endpoint method
    return $ Object $ HM.fromList [
        ("status", String "success")
        , ("currentScore", toJSON $ scorecard ^. totalScore)
        , ("author", String teamName)
        ]

createCommentH :: Maybe Text -> UUID.UUID -> ArticleComment -> HttpHuntApp Value
createCommentH Nothing _ _ = noTeamNameResponse
createCommentH(Just teamName) puid comment = do
    let method = "POST"
        endpoint = AdminArticleComments
    conn <- asks _getRedisConn
    article <- liftIO $ DB.createPostComment conn comment
    scorecard <- liftIO $ DB.upsertScoreCard conn teamName endpoint method
    return $ Object $ HM.fromList [
        ("status", String "success")
        , ("currentScore", toJSON $ scorecard ^. totalScore)
        , ("author", String teamName)
        , ("data", toJSON article)
        ]

deleteCommentH :: Maybe Text -> UUID.UUID -> ArticleComment -> HttpHuntApp Value
deleteCommentH Nothing _ _ = noTeamNameResponse
deleteCommentH(Just teamName) puid comment = do
    let method = "DELETE"
        endpoint = AdminArticleComments
    conn <- asks _getRedisConn
    article <- liftIO $ DB.deletePostComment conn comment
    scorecard <- liftIO $ DB.upsertScoreCard conn teamName endpoint method
    return $ Object $ HM.fromList [
        ("status", String "success")
        , ("currentScore", toJSON $ scorecard ^. totalScore)
        , ("author", String teamName)
        , ("data", toJSON article)
        ]


-- helpers, not handlers
noTeamNameResponse :: HttpHuntApp Value
noTeamNameResponse = return $ Object $ HM.fromList [
    ("status", String "failed")
    , ("error", String "not authorized!")
    , ("message", String "missing 'TeamName' header")
    ]

adminEdpoints :: Value
adminEdpoints =  Array $ V.fromList [
    Object $ HM.fromList [
        ("endpoint", String "admin/endpoints"),
        ("method", String "GET"),
        ("payload",  Null),
        ("content types", String "json")]
    , Object $ HM.fromList [
        ("endpoint", String "admin/articles"),
        ("method", String "POST"),
        ("payload",  String "Article"),
        ("content types", String "json")]
    , Object $ HM.fromList [
        ("endpoint", String "admin/articles/{ARTICLE_ID}"),
        ("method", String "GET"),
        ("payload",  Null),
        ("content types", String "json")]
    , Object $ HM.fromList [
        ("endpoint", String "admin/articles/{ARTICLE_ID}"),
        ("method", String "PUT"),
        ("payload",  String "Article"),
        ("content types", String "json")]
    , Object $ HM.fromList [
        ("endpoint", String "admin/articles/{ARTICLE_ID}"),
        ("method", String "DELETE"),
        ("payload",  Null),
        ("content types", String "json")]
    , Object $ HM.fromList [
        ("endpoint", String "admin/articles/comments/{ARTICLE_ID}"),
        ("method", String "POST"),
        ("payload", String "ArticleComment"),
        ("content types", String "json")]
    , Object $ HM.fromList [
        ("endpoint", String "admin/articles/comments/{ARTICLE_ID}"),
        ("method", String "DELETE"),
        ("payload", Null),
        ("content types", String "json")]
    ]

