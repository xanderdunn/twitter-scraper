module TwitterScraper (
outputFilePath,
csvContents,
getByteString,
startDay,
getStartDay,
twitterSearchURL,
scrapeSearchURL
) where

-- System
import System.Directory
import System.FilePath
import System.IO
import System.Environment
import Data.Maybe (fromJust)
import Control.Applicative
import qualified Data.Text as T

-- Third Party
import Text.HTML.Scalpel
import qualified Text.JSON as JSON
import Data.Time.Calendar
import Data.Time.Format
import qualified Data.Csv as CSV
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Vector as Vec

-- |The Twitter search URL for a given search term and day
twitterSearchURL :: String -> Day -> String
twitterSearchURL searchTerm day = "https://twitter.com/search?q=" ++ searchTerm ++ "%20lang%3Aen%20since%3A" ++ showGregorian day ++"%20until%3A" ++ showGregorian(addDays 1 day) ++ "&src=typd"

-- |The Twitter JSON response URLs for all search results beyond the first page
twitterJSONURL :: String -> Day -> Integer -> Integer -> String
twitterJSONURL searchTerm day tweetMin tweetMax = "https://twitter.com/i/search/timeline?vertical=news&q=" ++ searchTerm ++ "%20lang%3Aen%20since%3A" ++ showGregorian day ++ "%20until%3A" ++ showGregorian(addDays 1 day) ++ "&src=typd&include_available_features=1&include_entities=1&max_position=TWEET-" ++ show tweetMin ++ "-" ++ show tweetMax ++ "-BD1UO2FFu9QAAAAAAAAETAAAAAcAAAASAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA&reset_error_state=false"

type ScrapeReturn = (T.Text, T.Text, T.Text, T.Text, T.Text, T.Text, T.Text)

scrapeSearchURL :: String -> IO (Maybe [ScrapeReturn])
scrapeSearchURL url = scrapeURL url tweetScraper

tweetScraper :: Scraper T.Text [ScrapeReturn]
tweetScraper = tweets
   where
       tweets :: Scraper T.Text [ScrapeReturn]
       tweets = chroots ("div" @: [hasClass "js-stream-tweet"]) infos

       infos :: Scraper T.Text ScrapeReturn
       infos = do
           author <- attr "data-screen-name" Any
           id <- attr "data-tweet-id" Any
           body <- text $ "div"  @: [hasClass "js-tweet-text-container"]
           counters <- texts $ "span" @: [hasClass "ProfileTweet-actionCountForPresentation"]
           let retweets = head counters
           let likes = counters !! 2
           -- TODO: Fix the location and card_url items
           -- location <- text $ "span" @: [hasClass "Tweet-geo"]
           -- card_url <- attr "data-card-url" ("div"  @: [hasClass "js-macaw-cards-iframe-container"])
           return (id, author, T.pack "", retweets, likes, T.strip body, T.pack "")

-- TODO: Use a lens instead of this ugly, long tuple
type Tweet = (Int, T.Text, T.Text, Int, Int, T.Text, T.Text, T.Text, T.Text)

-- |The day on which to start scraping for a given term is the last day already recorded in that file.
startDay :: Vec.Vector Tweet -> Day
startDay tweets
    | null tweets = fromGregorian 2013 01 01
    | otherwise = day
    where day = fromJust $ parseTimeM True defaultTimeLocale (iso8601DateFormat Nothing) dateString
          (_, _, _, _, _, _, _, _, dateText) = Vec.last tweets
          dateString = T.unpack dateText

getStartDay :: ByteString.ByteString -> IO Day
getStartDay csvByteString = do
    let csv = csvContents csvByteString
    case csvContents csvByteString of
        Left msg -> error $ "Could not parse CSV with error: " ++ msg
        Right tweets -> return (startDay tweets)

csvContents :: ByteString.ByteString -> Either String (Vec.Vector Tweet)
csvContents = CSV.decode CSV.NoHeader

-- Prevent duplicates by checking a set of tweet IDs

-- A given search term is complete when the output CSV file is moved to _complete.csv
completeFile :: FilePath -> IO ()
completeFile path = renameFile path (dropExtension path ++ "_complete.csv")

outputFilePath :: String -> FilePath -> FilePath
outputFilePath searchTerm currentDirectory = currentDirectory </> "output" </> searchTerm ++ ".csv"

-- Get contents of file as a Lazy ByteString, return empty Lazy ByteString if the file does not exist
getByteString :: FilePath -> IO ByteString.ByteString
getByteString path = do
    fileExists <- doesFileExist path
    if fileExists
        then ByteString.readFile path
        else return ByteString.empty

-- Map across a list of companies, where each company has a list of search terms.  Prevent duplicate tweets across all files for a given company
-- Name the output files tesla.csv, @TeslaMotors.csv, #tesla.csv, etc.
