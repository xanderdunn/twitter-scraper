{-# LANGUAGE TemplateHaskell #-}

module TweetJSON (
scrapeJSONSearchURL,
TweetJSON,
_itemsHTML
) where

-- System
import Data.Text
import Control.Monad

-- Third Party
import Network.Wreq
import Data.Aeson
import Control.Lens hiding (element) -- Consider using microlens or fclabels

data TweetJSON = TweetJSON {__itemsHTML :: Text} deriving Show
makeLenses ''TweetJSON

instance FromJSON TweetJSON where
    parseJSON (Object v) = TweetJSON <$> v .: pack "items_html"
    parseJSON _ = mzero

scrapeJSONSearchURL :: String -> IO (Maybe TweetJSON)
scrapeJSONSearchURL url = do
    r <- get url
    let body = r ^. responseBody
    let jsonParsed = decode body :: Maybe TweetJSON
    return jsonParsed
