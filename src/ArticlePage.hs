{-# LANGUAGE OverloadedStrings #-}

module ArticlePage where

import Configuration (destinationPath)
import qualified Control.Applicative as H
import Control.Monad (forM_, void)
import Data.Functor ((<&>))
import Data.Ini
import Data.Map (fromList)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Text (Text, pack, toLower, unpack)
import qualified Data.Text.IO as TIO
import Data.Time (defaultTimeLocale, formatTime, parseTimeOrError)
import Date
import File
import GHC.IO.Handle (hGetContents)
import PageLayout
import System.Process
import qualified Text.Blaze as A
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Types

page :: Post -> Text -> H.Html
page p@(Post title slug date status meta) content = H.html $ do
    H.head
        ( pageHeader
            ("diasbruno - " <> title)
            ( forM_
                meta
                ( \(name, property, content) -> do
                    H.meta
                        ! A.name (H.stringValue name)
                        ! H.customAttribute "property" (H.stringValue property)
                        ! A.content (H.stringValue content)
                )
            )
        )
    H.body $
        do
            H.main $ do
                pageNavigation
                H.section ! A.class_ "content" $ do
                    H.div ! A.class_ "article-section" $ do
                        H.h1 (H.text title)
                        H.div ! A.class_ "content-info" $ do
                            H.span (H.text "article")
                            H.text " - "
                            H.text (toLower (pack (show status)))
                            H.text " - "
                            H.time (H.text (pack (postDate date)))
                                ! A.class_ "content-datetime"
                                ! A.datetime (H.stringValue (iso8601Date date))
                        H.div (H.preEscapedText content)
            H.script mempty
                ! A.type_ "application/javascript"
                ! A.src "/js/highlight.min.js"

toFile :: Post -> String
toFile (Post title slug date status meta) =
    unpack (pack (fileDate date) <> "-" <> slug <> ".md")


formatDateToString = formatTime defaultTimeLocale

postURL :: Post -> Text
postURL (Post _ slug date _ _) =
    "/articles" <> pack (formatDateToString "/%Y/%m/%d/" date) <> slug

postContentURL :: Post -> Text
postContentURL (Post _ slug date _ _) =
  "/" <> pack (formatDateToString "%Y-%m-%d-" date) <> slug

renderArticle :: Config -> Post -> IO Text
renderArticle c p = do
    let dest = fromJust $ destinationPath c
    mkdir (unpack (dest <> postURL p))
    let filename = dest <> postURL p <> "/index.html"
    content <- compileMd c (toFile p)
    TIO.writeFile (unpack filename) . pack $ renderHtml (page p content)
    return filename

renderArticleMetadataJson :: Config -> Post -> IO Text
renderArticleMetadataJson c p@(Post title slug date status _) = do
    let dest = fromJust $ destinationPath c
        url = postContentURL p
    mkdir $ unpack (dest <> url)
    let filename = dest <> url <> ".json"
    let content = pack $ mconcat ["{",
                   show "title",":", show title , ",",
                   show "slug",":", show slug , ",",
                   show "date",":", show (iso8601Date date), ",",
                   show "status",":", show . Data.Text.toLower . pack $ show status,
                   "}"]
    TIO.writeFile (unpack filename) content
    return filename

renderArticleContent :: Config -> Post -> IO Text
renderArticleContent c p = do
    let dest = fromJust $ destinationPath c
        url = postContentURL p
    let filename = dest <> url <> ".html"
    content <- compileMd c (toFile p)
    TIO.writeFile (unpack filename) content
    return filename
