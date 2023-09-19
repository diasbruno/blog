module Types where

import Data.Map (Map)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

type Config = Map Text Text

data Status = Publish | Hidden | Draft
    deriving (Eq, Show)

type Metadata = Map String String

data Post = Post Text Text UTCTime Status Metadata
    deriving (Eq, Show)

publishable :: Post -> Bool
publishable (Post _ _ _ s _) = s /= Hidden
