module HttpHunt.Frontend where

import qualified Data.Text                   as T
import           RIO
import           Text.Blaze.Html             ( Html )
import           Text.Blaze.Html5            ( (!) )
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import HttpHunt.Types (pageBase)


scoreBoardHtml :: Html
scoreBoardHtml = pageBase undefined