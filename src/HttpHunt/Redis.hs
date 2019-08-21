module HttpHunt.Redis where

import           Control.Lens         hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString      as B
import           Data.ByteString.Lazy (toStrict)
import qualified Data.HashMap.Lazy    as HML
import qualified Data.HashSet         as HS
import           Data.List            (delete, nub)
import           Data.Maybe
import qualified Data.Text            as T
import           Data.Text.Encoding
import           Data.Time.Clock      (UTCTime, getCurrentTime)
import qualified Data.UUID            as UUID
import qualified Data.UUID.V4         as UUID4
import qualified Database.Redis       as Redis
import           RIO                  hiding ((^.))
import qualified RIO.HashMap          as HM

import           HttpHunt.Config
import           HttpHunt.Exceptions
import           HttpHunt.Types

type Method = Text
type TeamName = Text

_ARTICLE_KEY :: ByteString
_ARTICLE_KEY = "_ARTICLE_"

getPost :: (MonadIO m, MonadThrow m) => Redis.Connection -> UUID.UUID -> m Article
getPost conn puid = do
    let key = B.append _ARTICLE_KEY (UUID.toASCIIBytes puid)
    result <- liftIO $ Redis.runRedis conn $ Redis.get key
    checkedValue <- checkRedisError result
    case checkedValue of
        Nothing -> throwRedisError "Can't find article"
        Just article -> case eitherDecodeStrict article of
                Right article' -> return article'
                Left decodeErr -> throwRedisError (T.pack decodeErr)

getAllPosts :: (MonadIO m, MonadThrow m) => Redis.Connection -> m [Article]
getAllPosts conn = do
    let keys = B.append _ARTICLE_KEY "*"
    results <- liftIO $ Redis.runRedis conn $ Redis.keys keys
    checkedValue <- checkRedisError results
    case checkedValue of
        []       -> return []
        articles -> return $ mapMaybe decodeStrict articles

updateWholePost :: (MonadIO m, MonadThrow m) => Redis.Connection -> UUID.UUID -> Article -> m UUID.UUID
updateWholePost conn puid article = do
    let key = B.append _ARTICLE_KEY (UUID.toASCIIBytes puid)
        value = encode article
    result <- liftIO $ Redis.runRedis conn $ Redis.set key (toStrict value)
    return puid

patchPost :: (MonadIO m, MonadThrow m) => Redis.Connection -> UUID.UUID -> Article -> m UUID.UUID
patchPost conn puid particle = do
    oldVersion <- getPost conn puid
    -- we're simplifying here: turn them into HashMaps (via JSON) and then merge them together
    let oldArticleHM = toJSON oldVersion ^? _Object
        newArticleHM = toJSON particle ^? _Object
        mergedArticle = HML.union <$> newArticleHM <*> oldArticleHM
    -- now that we have a merged HashMap, we need to make it into an Article
        parsedMergedArticle = fromJSON . Object <$> mergedArticle :: Maybe (Result Article)
    case parsedMergedArticle of
        Just (Success article) -> updateWholePost conn puid article
        _ -> throwM $ OtherError "Failed merging articles!"

deletePost :: (MonadIO m, MonadThrow m) => Redis.Connection -> UUID.UUID -> m UUID.UUID
deletePost conn puid = do
    let key = B.append _ARTICLE_KEY (UUID.toASCIIBytes puid)
    result <- liftIO $ Redis.runRedis conn $ Redis.del [key]
    return puid

createPostComment :: (MonadIO m, MonadThrow m) => Redis.Connection -> ArticleComment -> m Article
createPostComment conn comment = do
    let puid = comment ^. articleId
    article <- getPost conn puid
    -- nub is O(n^2), among other things...
    -- did we mention that this is all just a demonstration
    let article' = article {_comments = nub $ (:) comment (article ^. comments)}
    updateWholePost conn puid article'
    return article'

deletePostComment :: (MonadIO m, MonadThrow m) => Redis.Connection -> ArticleComment -> m Article
deletePostComment conn comment = do
    let puid = comment ^. articleId
    article <- getPost conn puid
    -- nub is O(n^2), among other things...
    -- did we mention that this is all just a demonstration
    let article' =  article {_comments = delete comment (article ^. comments)}
    updateWholePost conn puid article'
    return article'

-- | Scoring related functions
-- Includes type for computing a score
data ScoreVal =
    NewEndpoint
    | NewMethod
    | ExistingEndpointAndMethod

scoreVal :: ScoreVal -> Int
scoreVal NewEndpoint               = 500
scoreVal NewMethod                 = 150
scoreVal ExistingEndpointAndMethod = 5

insertNewCard ::(MonadIO m, MonadThrow m) => Redis.Connection -> TeamName -> Endpoint -> Method -> m ScoreCard
insertNewCard conn name endp meth = do
    let key = encodeUtf8 name
        newcard = newScorecard name
        scoredCard = scoring endp meth newcard
        newValue = encode scoredCard
    result <- liftIO $ Redis.runRedis conn $ Redis.setex key
                                                scoreExpiryDays
                                                (toStrict newValue)
    return scoredCard

upsertScoreCard :: (MonadIO m, MonadThrow m) => Redis.Connection -> TeamName -> Endpoint -> Method -> m ScoreCard
upsertScoreCard conn name endp meth = do
    let key = encodeUtf8 name
    itExists <- liftIO $ Redis.runRedis conn $ Redis.exists key
    case itExists of
        Left _ -> throwRedisError "Failed to check if scorecard exists"
        Right False -> insertNewCard conn name endp meth
        Right True -> do
            storedValue <- liftIO $ Redis.runRedis conn $ Redis.get key
            checkedValue <- checkRedisError storedValue
            case checkedValue of
                Nothing -> throwScoreCardError
                Just scoreCardBS -> do
                    let decodedCard = eitherDecodeStrict scoreCardBS :: Either String ScoreCard
                    case decodedCard of
                        Left decodeErr -> throwRedisError $ T.concat [T.pack decodeErr, " ", decodeUtf8 scoreCardBS]
                        (Right card) -> do
                            let scoredCard' = scoring endp meth card
                                newValue = encode scoredCard'
                            result <- liftIO $ Redis.runRedis conn $ Redis.setex key
                                                                        scoreExpiryDays
                                                                        (toStrict newValue)
                            return card

getScoreValFromRequest :: Endpoint -> Method -> ScoreCard -> ScoreVal
getScoreValFromRequest endp meth scard =
    if HM.member endp (scard ^. endpoints)
        -- check if method is present in these endpoints
        then if (HS.member meth . fromJust . HM.lookup endp) (scard ^. endpoints)
            then ExistingEndpointAndMethod
            else NewMethod
        else NewEndpoint

scoring :: Endpoint -> Method -> ScoreCard -> ScoreCard
scoring endpoint method scorecard =
    let score' = scoreVal $ getScoreValFromRequest endpoint method scorecard
    in scorecard {
        _totalScore = score' + scorecard ^. totalScore
        , _endpoints = updateEndpoints scorecard endpoint method
        }

updateEndpoints ::  ScoreCard -> Endpoint -> Method -> HashMap Endpoint (HashSet Text)
updateEndpoints scorecard endpoint method =
    let endpointHM = scorecard ^. endpoints
        oldSet = endpointHM ^.at endpoint
        newSet = maybe (HS.insert method HS.empty) (HS.insert method) oldSet
    in HM.insert endpoint newSet endpointHM

-- | Helper functions
checkRedisError :: (MonadIO m, MonadThrow m) => Either Redis.Reply a -> m a
checkRedisError (Left redisError) = do
    tstamp <- liftIO getCurrentTime
    throwRedisError (T.pack . show $ redisError)
checkRedisError (Right val) = pure val

throwRedisError :: (MonadThrow m) => Text -> m a
throwRedisError msg = throwM $ DbError msg

throwScoreCardError :: (MonadThrow m) => m a
throwScoreCardError = throwM $ DbError "Can't keep score!"

scoreExpiryDays :: Integer
scoreExpiryDays = _secondsInADay * 10

_secondsInADay :: Integer
_secondsInADay = 60 * 60 * 24
