{-# LANGUAGE OverloadedStrings  #-}
module File where

import Control.Monad (void)
import Data.Text
import GHC.IO.Handle (hGetContents)
import System.Process
import Types
import Configuration (contentPath)
import qualified Data.Map as M

compileMd :: Config -> String -> IO Text
compileMd c file = do
    let spec = proc "comrak" ["--unsafe", file]
    (_, Just hout, _, _) <-
        createProcess
            spec
                { std_out = CreatePipe
                , cwd = unpack <$> contentPath c
                }
    fmap pack (hGetContents hout)

mkdir :: String -> IO ()
mkdir file =
    let spec = proc "mkdir" ["-p", file]
     in void (createProcess spec)
