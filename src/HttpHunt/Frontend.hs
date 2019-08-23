{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}


module HttpHunt.Frontend where

import           Data.Aeson
import qualified Data.Text                   as T
import           RIO
import           Servant
import           Servant.HTML.Blaze          (HTML)

import           Text.Blaze.Html             (Html)
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import           HttpHunt.Config
import           HttpHunt.Redis              as DB
import           HttpHunt.Types

type FrontendApi =
  "api" :> "scores" :> Get '[JSON] [ScoreCard]
  :<|> "scoreboard" :> Get '[HTML] Html
  :<|> "blog" :> Get '[HTML] Html

frontendApi :: ServerT FrontendApi HttpHuntApp
frontendApi = scoreResultsJSON :<|> scoreBoardHtml :<|> blogHtml

scoreResultsJSON :: HttpHuntApp [ScoreCard]
scoreResultsJSON = do
    conn <- asks _getRedisConn
    liftIO $ DB.getAllScores conn

-- | Html Definitions
scoreBoardHtml :: HttpHuntApp Html
scoreBoardHtml = pure $ pageBase $ do
        H.head scripts
        H.body $
            H.div ! A.class_ "content" $ do
                H.h1 "Scavenger Hunt Scoreboard"
                H.div ! A.class_ "hunt-meta" $
                    H.button ! A.id "hunt-start" $ "Start Scavenger Hunt"
                H.div ! A.id "scoreboard" $ ""
                H.script ! A.src "static/scoreboard.js" $ ""

blogHtml :: HttpHuntApp Html
blogHtml = do
    conn <- asks _getRedisConn
    articles <- liftIO $ DB.getAllPosts conn
    pure $ pageBase $ H.ul $ mapM_  articleListItem articles

articleListItem :: Article -> Html
articleListItem article =
    H.li $ H.toMarkup article

scripts :: Html
scripts = do
    H.script ! A.src "https://cdnjs.cloudflare.com/ajax/libs/ramda/0.26.1/ramda.min.js" $ ""
    H.script ! A.src "https://cdnjs.cloudflare.com/ajax/libs/fetch/3.0.0/fetch.min.js" $ ""
    H.link ! A.href "static/styles.css" ! A.rel "stylesheet" ! A.type_ "text/css"
