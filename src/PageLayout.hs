{-# LANGUAGE OverloadedStrings #-}

module PageLayout where

import Control.Monad (forM_, when)
import Data.Either (isRight)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Data.Text (Text, pack, unpack)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal (MarkupM, stringValue, textValue)
import Types

pageHeader :: Text -> H.Html -> H.Html
pageHeader title metadata = do
    H.title (H.text title)
    H.meta
        ! A.name "viewport"
        ! A.content "width=device-width"
    H.meta
        ! A.name "author"
        ! A.content "Bruno Dias"
    H.meta
        ! A.name "title"
        ! A.content (textValue title)
    metadata
    forM_
        [ "/css/style.css"
        , "/css/highlight.min.css"
        , "/css/milligram.css"
        ]
        ( \style -> do
            H.link
                ! A.rel "stylesheet"
                ! A.type_ "text/css"
                ! A.href style
        )

pageNavigation :: MarkupM ()
pageNavigation = H.nav $ do
    H.div $ do
        H.a (H.h1 "diasbruno") ! A.href "/"
        H.div
            ( do
                H.a "articles"
                    ! A.class_ "nav-link"
                    ! A.href "/"
                H.a "opensource"
                    ! A.class_ "nav-link"
                    ! A.href "/opensource"
                H.a "github"
                    ! A.class_ "nav-link"
                    ! A.href "https://github.com/diasbruno"
                    ! A.target "_blank"
            )
            ! A.class_ "nav-links"
