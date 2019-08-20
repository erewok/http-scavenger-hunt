{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeOperators    #-}

module HttpHunt.Api where

import Control.Lens
import Data.Aeson
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Database.Redis                as Redis
import RIO hiding (Handler)
import qualified RIO.HashMap              as HM
import Servant

import HttpHunt.Config
import HttpHunt.Types
import HttpHunt.Admin.Api
import HttpHunt.Public.Api

-- | Lendio Offers Api Definition
type HttpHuntApi =
  "health" :> Get '[JSON] Value
  :<|> AdminHuntApi
  :<|> PublicHuntApi

httpHuntApi :: Proxy HttpHuntApi
httpHuntApi = Proxy

httpHuntServer :: ServerT HttpHuntApi HttpHuntApp
httpHuntServer = healthHandler :<|> adminHttpHuntApi :<|> publicHttpHuntApi

-- We need a natural transformation between our app monad and servant's Handler monad
hoister :: HttpHuntCtx -> HttpHuntApp a -> Handler a
hoister = runRIO

httpHuntApp :: HttpHuntCtx -> Application
httpHuntApp ctx =
    serve httpHuntApi $
        hoistServer httpHuntApi (hoister ctx) httpHuntServer

-- | Health handler
healthHandler :: HttpHuntApp Value
healthHandler = pure $ Object $ HM.fromList [( "status", "ok" )]


