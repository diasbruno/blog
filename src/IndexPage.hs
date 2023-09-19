{-# LANGUAGE OverloadedStrings #-}

module IndexPage where

import Control.Monad (forM_)
import Control.Monad (void)
import Data.Functor ((<&>))
import Data.Ini
import qualified Data.Map as M
import Data.Text (Text, pack, unpack, toLower)
import qualified Data.Text.IO as TIO
import Data.Time (defaultTimeLocale, formatTime, parseTimeOrError)
import Date
import File
import GHC.IO.Handle (hGetContents)
import PageLayout
import System.Process
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Types
import Data.Maybe (fromJust)
import Configuration (destinationPath)
import qualified Control.Applicative as H

postURL :: Post -> Text
postURL (Post _ slug date _ _) =
    "/articles" <> pack (formatTime defaultTimeLocale "/%Y/%m/%d/" date) <> slug

page :: [Post] -> H.Html
page ps = H.html $ do
    H.head (pageHeader (head ps))
    H.body $ do
        H.main $ do
            pageNavigation
            H.section ! A.class_ "content" $
              forM_ ps (\p@(Post title slug date status meta) -> do
                           H.div
                             ! A.class_ "article-item" $ do
                             H.a  (H.h1 (H.text title))
                               ! A.href (H.textValue (postURL p))
                             H.div $ do
                               H.time (H.text (pack (postDate date)))
                                 ! A.class_ "content-datetime"
                                 ! A.datetime (H.stringValue (iso8601Date date)))
        H.script mempty
            ! A.type_ "application/javascript"
            ! A.src "/js/highlight.min.js"

renderIndex :: Config -> [Post] -> IO Text
renderIndex c p = do
    let dest = fromJust $ destinationPath c
    mkdir (unpack dest)
    let fullArticleFilename = dest <> "/index.html"
    TIO.writeFile (unpack fullArticleFilename) . pack $ renderHtml (page p)
    return fullArticleFilename
