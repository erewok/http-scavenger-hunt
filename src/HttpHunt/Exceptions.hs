module HttpHunt.Exceptions where

import           Data.Text    (Text)
import           GHC.Generics
import           RIO

data AppExceptions
  = DbError Text
  | OtherError Text
  deriving (Eq, Generic, Show)

instance Exception AppExceptions
