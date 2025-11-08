{-# LANGUAGE OverloadedStrings #-}

module Feed where

import Configuration (destinationPath)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Text
import qualified Data.Text.IO as TIO
import Data.Text.Lazy (toStrict)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime, parseTimeOrError)
import Data.XML.Types as XML
import Date (iso8601Date)
import qualified Text.Atom.Feed as Atom
import qualified Text.Feed.Export as Export (textFeedWith)
import Text.Feed.Types
import Text.XML (def)
import Types

postURL :: Text -> Post -> Text
postURL b (Post _ slug date _ _) =
    b <> "/articles" <> pack (formatTime defaultTimeLocale "/%Y/%m/%d/" date) <> slug

feedDate :: UTCTime -> Text
feedDate date =
    pack (formatTime defaultTimeLocale "%Y-%m-%d" date)

toEntry :: Config -> Post -> Atom.Entry
toEntry c p@(Post title slug date status meta) =
    let baseurl = fromJust (M.lookup "BASE_URL" c)
        url = postURL baseurl p
     in ( Atom.nullEntry
            url
            (Atom.TextString title)
            (pack (iso8601Date date))
        )
            { Atom.entryAuthors = [Atom.nullPerson{Atom.personName = "Bruno Dias"}]
            , Atom.entryLinks = [Atom.nullLink url]
            , Atom.entryContent = Just (Atom.HTMLContent title)
            }

myFeed :: Config -> [Post] -> UTCTime -> Atom.Feed
myFeed c ps now =
    let baseurl = fromJust (M.lookup "BASE_URL" c)
        feed =
            Atom.nullFeed
                (baseurl <> "/atom.xml")
                (Atom.TextString "diasbruno")
                (feedDate now)
     in feed
            { Atom.feedEntries = fmap (toEntry c) ps
            , Atom.feedLinks = [Atom.nullLink baseurl]
            }

renderFeed c ps =
    let dest = fromJust $ destinationPath c
        fullFeedFilename = dest <> "/atom.xml"
     in do
            now <- getCurrentTime
            TIO.writeFile
                (unpack fullFeedFilename)
                (toStrict (fromJust $ Export.textFeedWith def (AtomFeed (myFeed c ps now))))
            return fullFeedFilename
