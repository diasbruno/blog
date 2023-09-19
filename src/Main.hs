module Main where

import Types
import Data
import Configuration
import Control.Monad
import ArticlePage
import IndexPage

main = do
  c <- loadConfig
  let filtered = filter publishable posts
  mapM_ (renderArticle c >=> print) filtered
  renderIndex c filtered >>= print
