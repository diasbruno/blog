module Main where

import Types
import Data
import Configuration
import Control.Monad
import ArticlePage
import IndexPage
import Feed

main = do
  c <- loadConfig
  let filtered = filter publishable posts
  mapM_ (renderArticle c >=> print) filtered
  renderIndex c filtered >>= print
  renderFeed c posts
