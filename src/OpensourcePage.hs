{-# LANGUAGE OverloadedStrings #-}

module OpensourcePage where

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
import Text.Blaze.Internal (MarkupM, stringValue, textValue)
import Types

page :: H.Html -> H.Html
page content = H.html $ do
    H.head (pageHeader "diasbruno - open-source" mempty)
    H.body $
        do
            H.main $ do
                pageNavigation
                H.section ! A.class_ "content" $ content
            H.script mempty
                ! A.type_ "application/javascript"
                ! A.src "/js/highlight.min.js"

renderProjects :: [(String, [Oss])] -> H.Html
renderProjects oss =
    forM_
        oss
        ( do
            \(cat, projects) -> do
                H.div ! A.class_ "opensource-lang" $ do
                    H.h5 $ do
                        H.span $ H.text (pack cat)
                H.div ! A.class_ "opensource-lang-list" $ do
                    forM_
                        projects
                        ( \(Oss name url _ description) -> do
                            H.div ! A.class_ "opensource-item" $ do
                                H.h3 (H.text name)
                                H.p (H.text description)
                                H.p $ do
                                    H.a (H.text url) ! A.href (textValue url)
                        )
        )

renderOss :: Config -> [(String, [Oss])] -> IO Text
renderOss c os = do
    let dest = fromJust $ destinationPath c
    mkdir (unpack (dest <> "/opensource"))
    let fullArticleFilename = dest <> "/opensource/index.html"
    TIO.writeFile
        (unpack fullArticleFilename)
        (pack (renderHtml (page (renderProjects os))))
    return fullArticleFilename
