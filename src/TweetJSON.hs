module TweetJSON (
scrapeJSONSearchURL
) where

-- System
import Data.Text
import Control.Monad

-- Third Party
import Network.Wreq
import Data.Aeson
import Control.Lens hiding (element) -- Consider using microlens or fclabels

data TweetJSON = TweetJSON {_itemsHTML :: Text} deriving Show

instance FromJSON TweetJSON where
    parseJSON (Object v) = TweetJSON <$> v .: pack "items_html"
    parseJSON _ = mzero

scrapeJSONSearchURL :: String -> IO (Maybe TweetJSON)
scrapeJSONSearchURL url = do
    r <- get url
    let body = r ^. responseBody
    let jsonParsed = decode body :: Maybe TweetJSON
    return jsonParsed
