module Main where

import           Data.Either
import           Database.Redis

import HttpHunt.Api
import HttpHunt.Config
import HttpHunt.Types


main :: IO ()
main = do
    conf <- decodeEnv :: IO (Either String ApiConfig)
    let config = fromRight (defConfig :: ApiConfig) conf
    -- print config
    if lenvironment config == Local then print config else pure ()
    undefined
