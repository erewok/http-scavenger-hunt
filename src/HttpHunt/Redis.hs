{-# LANGUAGE ScopedTypeVariables #-}

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
import           RIO                  hiding (over, (^.))
import qualified RIO.HashMap          as HM

import           HttpHunt.Config
import           HttpHunt.Exceptions
import           HttpHunt.Types

type Method = Text
type TeamName = Text

_ARTICLE_KEY :: ByteString
_ARTICLE_KEY = "_ARTICLE_"

_SCORING_KEY :: ByteString
_SCORING_KEY = "_SCORE_"

_TEAM_KEY :: ByteString
_TEAM_KEY = "_TEAM_KEY"

-- | Post related Redis Functions
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
getAllPosts conn = getAllKeysValues conn _ARTICLE_KEY

updateWholePost :: (MonadIO m, MonadThrow m) => Redis.Connection -> UUID.UUID -> Article -> m UUID.UUID
updateWholePost conn puid article = do
    let key = B.append _ARTICLE_KEY (UUID.toASCIIBytes puid)
        value = encode article
    result <- liftIO $ Redis.runRedis conn $ Redis.set key (toStrict value)
    return puid

putPost :: (MonadIO m, MonadThrow m) => Redis.Connection -> UUID.UUID -> Article -> m UUID.UUID
putPost conn puid particle = do
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

-- | Comment related Redis functions
addComments :: ArticleComment -> [ArticleComment] -> [ArticleComment]
addComments comment comments = nub $ (:) comment comments

createPostComment :: (MonadIO m, MonadThrow m) => Redis.Connection -> ArticleComment -> m Article
createPostComment conn comment = do
    let puid = comment ^. articleId
    article <- getPost conn puid
    -- nub is O(n^2), among other things...
    -- did we mention that this is all just a demonstration
    let article' = over comments (addComments comment) article
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

-- | Team-related functions
getAllTeams :: (MonadIO m, MonadThrow m) => Redis.Connection -> m [Team]
getAllTeams conn = getAllKeysValues conn _TEAM_KEY

updateWholeTeam :: (MonadIO m, MonadThrow m) => Redis.Connection -> Text -> Team -> m Team
updateWholeTeam conn name team = do
    let key = B.append _TEAM_KEY $ encodeUtf8 name
        value = encode team
    result <- liftIO $ Redis.runRedis conn $ Redis.set key (toStrict value)
    return team

putTeam :: (MonadIO m, MonadThrow m) => Redis.Connection -> Text -> Team -> m Team
putTeam conn name team = do
    let key = B.append _TEAM_KEY $ encodeUtf8 name
    oldVersion <- getKey conn key :: (MonadIO m, MonadThrow m) => m (Maybe Team)
    -- we're simplifying here: turn them into HashMaps (via JSON) and then merge them together
    let oldTeamHM = toJSON oldVersion ^? _Object
        newTeamHM = toJSON team ^? _Object
        mergedTeam = HML.union <$> newTeamHM <*> oldTeamHM
    -- now that we have a merged HashMap, we need to make it into an Article
        parsedMergedTeam = fromJSON . Object <$> mergedTeam :: Maybe (Result Team)
    case parsedMergedTeam of
        Just (Success team') -> updateWholeTeam conn name team'
        _                    -> throwM $ OtherError "Failed merging teams!"


patchTeam :: (MonadIO m, MonadThrow m) => Redis.Connection -> Text -> Value -> m Team
patchTeam conn name hm = do
    let key = B.append _TEAM_KEY $ encodeUtf8 name
    case hm of
        Object newTeam -> do
            oldVersion <- getKey conn key :: (MonadIO m, MonadThrow m) => m (Maybe Team)
            -- we're simplifying here: turn them into HashMaps (via JSON) and then merge them together
            let oldTeamHM = toJSON oldVersion ^? _Object
                mergedTeam = HML.union <$> pure newTeam <*> oldTeamHM
            -- now that we have a merged HashMap, we need to make it into an Article
                parsedMergedTeam = fromJSON . Object <$> mergedTeam :: Maybe (Result Team)
            case parsedMergedTeam of
                Just (Success team') -> updateWholeTeam conn name team'
                _                    -> throwM $ OtherError "Failed merging teams!"
        _ -> throwM $ OtherError "Object required to patch team!"

deleteTeam :: (MonadIO m, MonadThrow m) => Redis.Connection -> Text -> m Text
deleteTeam conn name = do
    let key = B.append _TEAM_KEY $ encodeUtf8 name
    result <- liftIO $ Redis.runRedis conn $ Redis.del [key]
    return name


-- | Scoring related functions
-- Includes type for computing a score
data ScoreVal =
    NewEndpoint
    | NewMethod
    | ExistingEndpointAndMethod

scoreVal :: ScoreVal -> Int
scoreVal NewEndpoint               = 500
scoreVal NewMethod                 = 250
scoreVal ExistingEndpointAndMethod = 5

getAllScores :: (MonadIO m, MonadThrow m) => Redis.Connection -> m [ScoreCard]
getAllScores conn = getAllKeysValues conn _SCORING_KEY

insertNewCard ::(MonadIO m, MonadThrow m) => Redis.Connection -> TeamName -> Endpoint -> Method -> m ScoreCard
insertNewCard conn name endp meth = do
    let key = B.append _SCORING_KEY $ encodeUtf8 name
        newcard = newScorecard name
        scoredCard = scoring endp meth newcard
        newValue = encode scoredCard
    result <- liftIO $ Redis.runRedis conn $ Redis.setex key
                                                scoreExpiryDays
                                                (toStrict newValue)
    return scoredCard

upsertScoreCard :: (MonadIO m, MonadThrow m) => Redis.Connection -> TeamName -> Endpoint -> Method -> m ScoreCard
upsertScoreCard conn name endp meth = do
    let key =  B.append _SCORING_KEY $ encodeUtf8 name
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
                            return scoredCard'

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
getAllKeysValues :: (MonadIO m, MonadThrow m, FromJSON a) => Redis.Connection -> ByteString -> m [a]
getAllKeysValues conn keyPrefix = do
    let keys = B.append keyPrefix "*"
    results <- liftIO $ Redis.runRedis conn $ Redis.keys keys
    checkedValue <- checkRedisError results
    case checkedValue of
        []     -> return []
        someKeys -> do
            results' <- mapM (getKey conn) someKeys
            pure $ catMaybes results'


getKey :: (MonadIO m, MonadThrow m, FromJSON a) => Redis.Connection -> ByteString -> m (Maybe a)
getKey conn key = do
    result <- liftIO $ Redis.runRedis conn $ Redis.get key
    checkedValue <- checkRedisError result
    case checkedValue of
        Nothing    -> throwRedisError "Can't retrieve key"
        Just value -> pure $ decodeStrict value

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
