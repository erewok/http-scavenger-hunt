{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

module HttpHunt.Types where

import           Control.Lens                hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Hashable               (Hashable)
import           Data.HashSet                (HashSet)
import           Data.Maybe
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Data.UUID                   as UUID
import           RIO                         hiding (Handler)
import           RIO.HashMap                 (HashMap)
import qualified RIO.HashMap                 as HM
import           Text.Blaze.Html             (Html)
import           Text.Blaze.Html5            (Attribute, ToMarkup (..), (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import           Data.Time.Clock             (UTCTime)
import           GHC.Generics


-- | Different types of site content: posts and post comments
data Article = Article {
    _pid        :: Maybe UUID.UUID
    , _author   :: Maybe Text
    , _title    :: Text
    , _content  :: Text
    , _pubdate  :: Maybe UTCTime
    , _comments :: [ArticleComment]
} deriving (Eq, Show, Generic)

instance FromJSON Article where
    parseJSON = withObject "article" $ \a -> do
        _pid <- a .:? "id"
        _author <- a .: "author"
        _title <- a .: "title"
        _content <- a .: "content"
        _pubdate <- a .:? "pubdate"
        _comments <- a .:? "comments" .!= []
        return Article{..}

instance ToJSON Article where
    toJSON Article{..} = object [
        "id" .= _pid,
        "author"  .= _author,
        "title"  .= _title,
        "content"  .= _content,
        "pubdate"  .= _pubdate,
        "comments"  .= _comments
        ]

instance ToMarkup Article where
    toMarkup article = do
            H.h2 . toMarkup $ _title article
            if isJust (_pubdate article)
                then H.p . toMarkup . show . fromJust $ _pubdate article
                else ""
            H.p . toMarkup . fromJust $ _author article
            H.p . toMarkup $ _content article
            if null (_comments article)
                then ""
                else do
                    H.h3 "Comments"
                    H.ul $ mapM_ toMarkup $ _comments article


instance ToMarkup [Article] where
    toMarkup = mapM_ (H.li . toMarkup)

data ArticleComment = ArticleComment {
    _articleId       :: UUID.UUID
    , _comment       :: Text
    , _commentAuthor :: Text
} deriving (Eq, Show, Generic)

instance FromJSON ArticleComment where
    parseJSON = withObject "comment" $ \ac -> do
        _articleId <- ac .: "articleId"
        _comment <- ac .: "comment"
        _commentAuthor <- ac .: "author"
        return ArticleComment{..}

instance ToJSON ArticleComment where
    toJSON ArticleComment{..} = object [
        "articleId" .= _articleId,
        "comment"  .= _comment,
        "author"  .= _commentAuthor
        ]

instance ToMarkup ArticleComment where
    toMarkup comment = do
            H.p . toMarkup $ _comment comment
            H.p . toMarkup $ _commentAuthor comment

data Team = Team {
    name            :: Text
    , avatar        :: Maybe Text
    , members       :: [Text]
    , catchphrase   :: Maybe Text
    , combinedScore :: Maybe Integer
} deriving (Eq, Show, Generic)

instance FromJSON Team
instance ToJSON Team

-- | Keeping track of stuff
newScorecard :: Text -> ScoreCard
newScorecard teamName = ScoreCard {
    _teamName = teamName
    , _totalScore = 0
    , _endpoints = HM.empty
}

data ScoreCard = ScoreCard {
    -- this is how we'll identify the team
    _teamName     :: Text
    -- team's total score
    , _totalScore :: Int
    -- map of endpoint to HTTP Methods used
    , _endpoints  :: HashMap Endpoint (HashSet Text)
} deriving (Eq, Show, Generic)

instance FromJSON ScoreCard
instance ToJSON ScoreCard
instance ToMarkup ScoreCard where
    toMarkup scoring = pageBase $ do
            H.h2 . toMarkup $ _teamName scoring
            H.h3 . toMarkup $ _totalScore scoring

data Endpoint =
    -- admin endpoints
    AdminEndpoints
    | AdminArticleList
    | AdminArticleDetail
    | AdminArticleComments
    -- team endpoints
    | AdminTeamList
    | AdminTeamDetail
    -- public endpoints
    | PublicEndpoints
    | PublicArticleList
    | PublicArticleDetail
    | PublicArticleDescribe
    | PublicCommentDescribe
    -- uncategorized endpoints
    | FrontPage
    | ScoreBoard
    deriving (Eq, Show, Generic)

instance FromJSON Endpoint
instance ToJSON Endpoint
instance Hashable Endpoint
instance FromJSONKey Endpoint where
    fromJSONKey = FromJSONKeyText parseEndpoint

parseEndpoint :: Text -> Endpoint
parseEndpoint = \case
    "AdminEndpoints" -> AdminEndpoints
    "AdminArticleList" -> AdminArticleList
    "AdminArticleDetail" -> AdminArticleDetail
    "AdminArticleComments" -> AdminArticleComments
    "AdminTeamList" -> AdminTeamList
    "AdminTeamDetail" -> AdminTeamDetail
    "PublicEndpoints" -> PublicEndpoints
    "PublicArticleList" -> PublicArticleList
    "PublicArticleDetail" -> PublicArticleDetail
    "PublicArticleDescribe" -> PublicArticleDescribe
    "PublicCommentDescribe" -> PublicCommentDescribe
    "FrontPage" -> FrontPage
    "ScoreBoard" -> ScoreBoard

instance ToJSONKey Endpoint where
    toJSONKey = toJSONKeyText (Text.pack . show)

-- | Some basic HTML
pageBase :: Html -> Html
pageBase content = H.docTypeHtml $
    H.body content

_LOGO :: String
_LOGO = unlines [
    "_   _ _____ _____ ____"
    , "| | | |_   _|_   _|  _ \\"
    , "| |_| | | |   | | | |_) |"
    , "|  _  | | |   | | |  __/"
    , "|_| |_| |_|   |_| |_|"
    , "/ ___|  ___ __ ___   _____ _ __   __ _  ___ _ __"
    , "\\___ \\ / __/ _` \\ / / _ \\ '_ \\ / _` |/ _ \\'__|"
    , "___) | (_| (_| |\\ V /  __/ | | | (_| |  __/ |"
    , "|_____ \\___\\__,_|\\_/ \\___|_| |_|\\__, |\\___|_|"
    , "| | | |_   _ _ __ | |_           |___/"
    , "| |_| | | | | '_ \\| __|"
    , "|  _  | |_| | | | | |_"
    , "|_| |_|\\__,_|_| |_|\\__|"
    ]


-- | Lenses at the bottom...
makeLenses ''Article
makeLenses ''ArticleComment
makeLenses ''ScoreCard
