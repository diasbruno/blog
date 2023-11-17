module Main where

import ArticlePage
import Configuration
import Control.Monad
import Data
import Feed
import IndexPage
import OpensourcePage
import Types

main = do
    c <- loadConfig
    let filtered = filter publishable posts
    mapM_ (renderArticle c >=> print) filtered
    renderIndex c filtered >>= print
    renderFeed c posts
    renderOss c oss
    return ()
