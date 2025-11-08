{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE CPP             #-}
module Main where

import ArticlePage
import Configuration
import Control.Monad
import Data
import Feed
import IndexPage
import OpensourcePage
import Types
import GHC.Exts
import qualified Data.Text.IO as TIO
import Data.Text (Text, pack, toLower, unpack)
import Data.Maybe (fromJust)
import Data.List (intercalate)
import Date


type Partial = (() :: Constraint)

-- | Apply some operation repeatedly, producing an element of output
--   and the remainder of the list.
--
-- When the empty list is reached it is returned, so the operation
-- is /never/ applied to the empty input.
-- That fact is encoded in the type system with 'repeatedlyNE'
--
-- > \xs -> repeatedly (splitAt 3) xs  == chunksOf 3 xs
-- > \xs -> repeatedly word1 (trim xs) == words xs
-- > \xs -> repeatedly line1 xs == lines xs
repeatedly :: ([a] -> (b, [a])) -> [a] -> [b]
repeatedly f [] = []
repeatedly f as = b : repeatedly f as'
    where (b, as') = f as

-- | Split a list into chunks of a given size. The last chunk may contain
--   fewer than n elements. The chunk size must be positive.
--
-- > chunksOf 3 "my test" == ["my ","tes","t"]
-- > chunksOf 3 "mytest"  == ["myt","est"]
-- > chunksOf 8 ""        == []
-- > chunksOf 0 "test"    == undefined
chunksOf :: Partial => Int -> [a] -> [[a]]
chunksOf i xs | i <= 0 = error $ "chunksOf, number must be positive, got " ++ show i
chunksOf i xs = repeatedly (splitAt i) xs

indexed :: [a] -> [(Int, a)]
indexed xs = go 0 xs
  where
    go i (a:as) = (i, a) : go (i + 1) as
    go _ _      = []

renderArticleJsonPage :: Text -> Int -> (Int, [Post]) -> IO Text
renderArticleJsonPage dest pages (index, chunk) = do
  let
    toJSON (Post title substackLink date status _) =
      mconcat ["{",
                show "title",":", show title , ",",
                show "link",":", show substackLink , ",",
                show "date",":", show (iso8601Date date), ",",
                show "status",":", show . Data.Text.toLower . pack $ show status,
                "}"
              ]
    content = mconcat ["{",
                         show "page", ":", show (index + 1), ",",
                         show "number_of_pages", ":", show pages, ",",
                         show "content", ":", "[",
                         intercalate "," (map toJSON chunk),
                         "]}"
                      ]
    filename = unpack $ dest <> pack ("/posts-" <> show (index + 1) <> ".json")
  TIO.writeFile filename (pack content)
  return . pack $ filename

renderArticleJsons :: Config -> [Post] -> IO ()
renderArticleJsons c ps = do
  let dest = fromJust $ destinationPath c
      pagesContent = [ps]
  mapM_ (renderArticleJsonPage dest (length pagesContent) >=> print) (indexed pagesContent)

main = do
    c <- loadConfig
    let filtered = filter publishable posts
    -- mapM_ (renderArticle c >=> print) filtered
    -- mapM_ (renderArticleContent c >=> print) filtered
    -- mapM_ (renderArticleMetadataJson c >=> print) filtered
    renderArticleJsons c filtered
    -- renderIndex c filtered >>= print
    renderFeed c posts >>= print
    renderOss c oss >>= print
    return ()
