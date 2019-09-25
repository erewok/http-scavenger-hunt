{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module HttpHunt.Config where

import           Control.Lens
import           Data.Maybe     (fromJust, isJust)
import           Data.Text
import qualified Database.Redis as Redis
import           GHC.Generics
import           RIO
import           System.Envy


-- | Application context and such
type HttpHuntApp = RIO HttpHuntCtx

data HttpHuntCtx = HttpHuntCtx {
    _getConfig      :: ApiConfig
    , _getRedisConn :: Redis.Connection
}

-- | Configuration values
data Environment = Local
                | Dev
                | Stage
                | Test
                | Prod
                deriving (Eq, Show)

instance Var Environment where
    toVar = show
    fromVar = \case
        "local" -> Just Local
        "dev" -> Just Dev
        "stage" -> Just Stage
        "test" -> Just Test
        "prod" -> Just Prod
        _ -> Nothing

data ApiConfig = ApiConfig {
    environment   :: Environment
    , version     :: Text
    , port        :: Int
    , redisHost   :: Text
    , redisPort   :: Text
    , redisDb     :: Text
    , redisPasswd :: Maybe Text
} deriving (Eq, Show, Generic)

instance DefConfig ApiConfig where
    defConfig = ApiConfig Local "v1" 8000 "localhost" "6379" "3" Nothing

-- All fields will be converted to uppercase
instance FromEnv ApiConfig

redisConn :: ApiConfig -> Either String Redis.ConnectInfo
redisConn conf = if isJust $ redisPasswd conf
    then redisConnWithPasswd (fromJust . redisPasswd $ conf) conf
    else redisConnNoPasswd conf

redisConnWithPasswd :: Text -> ApiConfig -> Either String Redis.ConnectInfo
redisConnWithPasswd passwd conf =
    let
        redisConnStr  = "redis://:" <> passwd <> "@" <> redisConnTail conf
    in  Redis.parseConnectInfo $ unpack redisConnStr

redisConnNoPasswd :: ApiConfig -> Either String Redis.ConnectInfo
redisConnNoPasswd conf =
    let redisConnStr = "redis://" <> redisConnTail conf
    in  Redis.parseConnectInfo $ unpack redisConnStr

redisConnTail :: ApiConfig -> Text
redisConnTail conf =
    redisHost conf <> ":" <> redisPort conf <> "/" <> redisDb conf


makeLenses ''HttpHuntCtx
