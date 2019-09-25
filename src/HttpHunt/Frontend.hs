{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}


module HttpHunt.Frontend where

import           Data.Aeson
import qualified Data.Text                   as T
import           RIO
import qualified RIO.HashMap                 as HM
import qualified RIO.Vector                  as V

import           Servant
import           Servant.HTML.Blaze          (HTML)

import           Text.Blaze.Html             (Html)
import           Text.Blaze.Html5            (ToMarkup (..), (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import           HttpHunt.Config
import           HttpHunt.Redis              as DB
import           HttpHunt.Types

type FrontendApi =
    -- home page: shows how to get endpoints for public and admin
    Get '[JSON] Value
    :<|> "api" :> "scores" :> Get '[JSON] [ScoreCard]
    :<|> "scoreboard" :> Get '[HTML] Html
    :<|> "blog" :> Get '[HTML] Html

frontendApi :: ServerT FrontendApi HttpHuntApp
frontendApi = homePageJSON :<|> scoreResultsJSON :<|> scoreBoardHtml :<|> blogHtml

homePageJSON :: HttpHuntApp Value
homePageJSON = pure $
    Object $ HM.fromList [
        ("status", String "success"),
        ("message", String "Welcome to the HTTP Scavenger Hunt!"),
        ("logo", String (T.pack _LOGO)),
        ("tips", Array $ V.fromList [
            String "If you identify yourself, you will get points for each new endpoint and method you try",
            String "For some endpoints you should provide an auth header (try `teamName`...)",
            String "Admin endpoints can be discovered at /admin/endpoints",
            String "Public endpoints can be discovered at /public/endpoints",
            String "You may also find some poorly documented (or undcoumented!) endpoints as well!"
            ])
        ]


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
                H.h1 "HTTP Scavenger Hunt"
                H.div ! A.class_ "hunt-meta" $
                    H.button ! A.id "hunt-start" $ "Start Scavenger Hunt"
                H.div ! A.class_ "clearfix" $ ""
                H.div ! A.id "scoreboard" $ ""
                H.script ! A.src "static/scoreboard.js" $ ""

blogHtml :: HttpHuntApp Html
blogHtml = do
    conn <- asks _getRedisConn
    articles <- liftIO $ DB.getAllPosts conn
    pure $ pageBase $ do
        H.head $
            H.link ! A.href "static/styles.css" ! A.rel "stylesheet" ! A.type_ "text/css"
        H.div ! A.class_ "content" $ do
            H.h1 "The HTTP Scavenger Hunt Blog"
            H.ul $ toMarkup articles

articleListItem :: Article -> Html
articleListItem article =
    H.li $ H.toMarkup article

scripts :: Html
scripts = do
    H.script ! A.src "https://cdnjs.cloudflare.com/ajax/libs/ramda/0.26.1/ramda.min.js" $ ""
    H.script ! A.src "https://cdnjs.cloudflare.com/ajax/libs/fetch/3.0.0/fetch.min.js" $ ""
    H.link ! A.href "static/styles.css" ! A.rel "stylesheet" ! A.type_ "text/css"
