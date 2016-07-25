{-# LANGUAGE TemplateHaskell #-}

-- TODO: Doing too much in this file.  Separate into IO, CSV, JSON

module TwitterScraper (
outputFilePath,
csvContents,
getByteString,
startDay,
getStartDay,
twitterSearchURL,
twitterJSONURL,
scrapeSearchURL,
tweetMinMax,
completeFile,
scrapeJSONSearchURL,
scrapeTweetJSON
) where

-- System
import System.Directory
import System.FilePath
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Control.Monad

-- Third Party
import Text.HTML.Scalpel
import Data.Time.Calendar
import Data.Time.Format
import Data.Csv
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Vector as V
import Control.Lens hiding (element) -- Consider using microlens or fclabels

-- First Party
import TweetJSON (scrapeJSONSearchURL, TweetJSON)

-- |The Twitter search URL for a given search term and day
twitterSearchURL :: String -> Day -> String
twitterSearchURL searchTerm day = "https://twitter.com/search?q=" ++ searchTerm ++ "%20lang%3Aen%20since%3A" ++ showGregorian day ++"%20until%3A" ++ showGregorian(addDays 1 day) ++ "&src=typd"

-- |The Twitter JSON response URLs for all search results beyond the first page
twitterJSONURL :: String -> Day -> Int -> Int -> String
twitterJSONURL searchTerm day tweetMax tweetMin = "https://twitter.com/i/search/timeline?vertical=news&q=" ++ searchTerm ++ "%20lang%3Aen%20since%3A" ++ showGregorian day ++ "%20until%3A" ++ showGregorian(addDays 1 day) ++ "&src=typd&include_available_features=1&include_entities=1&max_position=TWEET-" ++ show tweetMin ++ "-" ++ show tweetMax ++ "-BD1UO2FFu9QAAAAAAAAETAAAAAcAAAASAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA&reset_error_state=false"

data Tweet = Tweet { __unique :: Int, __author :: T.Text, __location :: T.Text, __retweets :: Int, __likes :: Int, __body :: T.Text, __cardURL :: T.Text, __date :: T.Text } deriving (Show)
makeLenses ''Tweet

instance ToRecord Tweet where
    toRecord (Tweet uniqueF authorF locationF retweetsF likesF bodyF cardURLF dateF) = record [toField uniqueF, toField authorF, toField locationF, toField retweetsF, toField likesF, toField bodyF, toField cardURLF, toField dateF]

instance FromRecord Tweet where
    parseRecord v
      | length v == 8 = Tweet <$>
                          v .! 0 <*>
                          v .! 1 <*>
                          v .! 2 <*>
                          v .! 3 <*>
                          v .! 4 <*>
                          v .! 5 <*>
                          v .! 6 <*>
                          v .! 7
      | otherwise     = mzero

tweetMinMax :: V.Vector Tweet -> (Int, Int)
tweetMinMax tweets = (minID, maxID)
    where headTweet = V.head tweets
          maxID = view _unique headTweet
          minID = view _unique (V.last tweets)

scrapeSearchURL :: String -> IO (Maybe [Tweet])
scrapeSearchURL url = scrapeURL url tweetScraper

textToInt :: T.Text -> Int
textToInt t
    | T.length t == 0 = 0
    | otherwise = read (T.unpack t) :: Int

-- | Scraper defined for the Scalpel library to scrape the HTML
tweetScraper :: Scraper T.Text [Tweet]
tweetScraper = tweets
   where
       tweets :: Scraper T.Text [Tweet]
       tweets = chroots ("div" @: [hasClass "js-stream-tweet"]) infos

       infos :: Scraper T.Text Tweet
       infos = do
           authorT <- attr "data-screen-name" Any
           uniqueT <- attr "data-tweet-id" Any
           bodyT <- text $ "div"  @: [hasClass "js-tweet-text-container"]
           counters <- texts $ "span" @: [hasClass "ProfileTweet-actionCountForPresentation"]
           let retweetsT = head counters
           let likesT = counters !! 2
           -- TODO: Fix the location and card_url items
           -- location <- text $ "span" @: [hasClass "Tweet-geo"]
           -- card_url <- attr "data-card-url" ("div"  @: [hasClass "js-macaw-cards-iframe-container"])
           return Tweet {__unique = textToInt uniqueT, __author = authorT, __location = T.pack "", __retweets = textToInt retweetsT, __likes = textToInt likesT, __body = T.strip bodyT, __cardURL = T.pack "", __date = T.pack ""}


-- |The day on which to start scraping for a given term is the last day already recorded in that file.
startDay :: V.Vector Tweet -> Day
startDay tweets
    | null tweets = fromGregorian 2013 01 01
    | otherwise = day
    where day = fromJust $ parseTimeM True defaultTimeLocale (iso8601DateFormat Nothing) dateString
          dateText = view _date (V.last tweets)
          dateString = T.unpack dateText

-- |This should instead take a Vector 
getStartDay :: ByteString.ByteString -> IO Day
getStartDay csvByteString = case csvContents csvByteString of
        Left msg -> error $ "Could not parse CSV with error: " ++ msg
        Right tweets -> return (startDay tweets)

csvContents :: ByteString.ByteString -> Either String (V.Vector Tweet)
csvContents = decode NoHeader

-- |A given search term is complete when the output CSV file is moved to _complete.csv
completeFile :: FilePath -> IO ()
completeFile path = renameFile path (dropExtension path ++ "_complete.csv")

outputFilePath :: String -> FilePath -> FilePath
outputFilePath searchTerm currentDirectory = currentDirectory </> "output" </> searchTerm ++ ".csv"

-- |Get contents of file as a Lazy ByteString, return empty Lazy ByteString if the file does not exist
getByteString :: FilePath -> IO ByteString.ByteString
getByteString path = do
    fileExists <- doesFileExist path
    if fileExists
        then ByteString.readFile path
        else return ByteString.empty

-- Prevent duplicates by checking a set of tweet IDs
-- Map across a list of companies, where each company has a list of search terms.  Prevent duplicate tweets across all files for a given company
-- Name the output files tesla.csv, @TeslaMotors.csv, #tesla.csv, etc.

-- TODO: I'd rather not create lenses for this data type twice
makeLenses ''TweetJSON

scrapeTweetJSON :: TweetJSON -> Maybe [Tweet]
scrapeTweetJSON json
    | htmlText == T.pack "" = Just []
    | otherwise = scrapeStringLike htmlText tweetScraper
    where htmlText = view _itemsHTML json
