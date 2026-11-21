{-# LANGUAGE OverloadedStrings #-}

module Configuration where

import Data.Functor ((<&>))
import Data.Ini (Ini (iniGlobals), readIniFile)
import qualified Data.Map as M
import Data.Text (Text)
import Types (Config)

loadConfig :: IO Config
loadConfig =
    readIniFile "./config.ini" <&> either (const M.empty) (M.fromList . iniGlobals)

contentPath :: Config -> Maybe Text
contentPath =
    M.lookup "CONTENT_PATH"

destinationPath :: Config -> Maybe Text
destinationPath = M.lookup "DESTINATION_PATH"
