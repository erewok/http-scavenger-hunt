{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
import           Data.ByteString.Lazy      as BL
import           Data.ByteString.Lazy.UTF8 as BLU
import           Data.Either
import qualified Data.Text                 as T
import           Data.Typeable
import           Database.Redis
import           Network.HTTP.Types
import qualified Network.Wai               as Wai
import qualified Network.Wai.Handler.Warp  as Warp
import           RIO
import           Servant
import           System.Envy

import           HttpHunt.Api
import           HttpHunt.Config
import           HttpHunt.Exceptions
import           HttpHunt.Types

handleErrors :: SomeException -> Wai.Response
handleErrors exc =
    case (cast exc :: Maybe AppExceptions) of
        Just myExc -> Wai.responseLBS internalServerError500 [(hContentType, "application/json")] $ BL.concat ["{\"status\":", BLU.fromString $ show myExc, "}"]
        Nothing -> Wai.responseLBS internalServerError500 [(hContentType, "application/json")] "{\"status\": \"error\"}"

main :: IO ()
main = do
    conf <- decodeEnv :: IO (Either String ApiConfig)
    let config = fromRight (defConfig :: ApiConfig) conf
    -- print config
    if environment config == Local then print config else pure ()
    let redisC = redisConn config
    case redisC of
        Left  err           -> throwM $ OtherError (T.pack err)
        Right redisConnInfo -> do
            conn <- checkedConnect redisConnInfo
            let ctx = HttpHuntCtx config conn
                warpSettings = Warp.defaultSettings
                portSettings = Warp.setPort (port config) warpSettings
                settings = Warp.setTimeout 55 portSettings

            Warp.runSettings settings $ httpHuntApp ctx

