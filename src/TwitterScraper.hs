{-# LANGUAGE TemplateHaskell #-}

-- TODO: Doing too much in this file.  Separate into IO, CSV, JSON

module TwitterScraper (
outputFilePath,
getByteString,
startDay,
getStartValues,
twitterSearchURL,
twitterJSONURL,
saveDayTweets,
saveYearTweets,
completeFile,
getExistingTweets
) where

-- System
import System.Directory
import System.FilePath
import qualified Data.Text as T
import Control.Monad
import qualified Data.Set as Set

-- Third Party
import Text.HTML.Scalpel
import Data.Time.Calendar
import Data.Time.Clock.POSIX
import Data.Time
import Data.Csv
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector as V
import Control.Lens hiding (element) -- Consider using microlens or fclabels

-- First Party
import TweetJSON (TweetJSON, scrapeJSONSearchURL, _itemsHTML)

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

textToInteger :: T.Text -> Integer
textToInteger t
    | T.length t == 0 = 0
    | otherwise = read (T.unpack t) :: Integer

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
           date <- attr "data-time-ms" ("span"  @: [hasClass "_timestamp"])
           counters <- texts $ "span" @: [hasClass "ProfileTweet-actionCountForPresentation"]
           let retweetsT = head counters
           let likesT = counters !! 2
           -- TODO: Fix the location and card_url items https://github.com/fimad/scalpel/issues/39
           -- location <- text $ "span" @: [hasClass "Tweet-geo"]
           -- card_url <- attr "data-card-url" ("div"  @: [hasClass "js-macaw-cards-iframe-container"])
           return Tweet {__unique = textToInt uniqueT, __author = authorT, __location = T.pack "", __retweets = textToInt retweetsT, __likes = textToInt likesT, __body = T.strip bodyT, __cardURL = T.pack "", __date = date}


-- |The day on which to start scraping for a given term is the last day already recorded in that file.
startDay :: V.Vector Tweet -> Day
startDay tweets
    | null tweets = fromGregorian 2013 01 01
    | otherwise = day
    where day = utctDay $ posixSecondsToUTCTime $ fromInteger milliseconds / 1000
          milliseconds = textToInteger $ view _date (V.last tweets)

getExistingTweets :: LBS.ByteString -> IO (V.Vector Tweet)
getExistingTweets csvByteString = case csvContents csvByteString of
        Left msg -> error $ "Could not parse CSV with error: " ++ msg
        Right tweets -> return tweets

-- |Start day and all existing unique IDs
getStartValues :: V.Vector Tweet -> (Day, Set.Set Int)
getStartValues tweets = (startDay tweets, uniques)
    where uniques = Set.fromList $ map (view _unique) (V.toList tweets)

csvContents :: LBS.ByteString -> Either String (V.Vector Tweet)
csvContents = decode NoHeader

-- |Save a V.Vector of Tweet objects to CSV
saveTweets :: FilePath -> String -> Set.Set Int -> V.Vector Tweet -> Day -> IO ()
saveTweets path searchTerm existingIDs tweets day = do
    let newTweets = uniqueTweets existingIDs tweets
    LBS.appendFile path (encode (V.toList newTweets))
    print $ show (V.length newTweets) ++ " " ++ showGregorian day ++ " " ++ searchTerm ++ " tweets saved to " ++ path

-- TODO: This can be made more elegant and efficient
-- |Given a Set of already saved unique Tweet IDs and new Tweet objects, return a Vector of Tweet objects that are not in the Set of unique IDs
uniqueTweets :: Set.Set Int -> V.Vector Tweet -> V.Vector Tweet
uniqueTweets existingIDs newTweets
    | V.length newTweets == 0 = V.empty
    | otherwise = currentTweet V.++ uniqueTweets existingIDs xs
        where (x, xs) = (V.head newTweets, V.tail newTweets)
              currentTweet = if view _unique x `Set.member` existingIDs then V.empty else V.fromList [x]

-- |A given search term is complete when the output CSV file is moved to _complete.csv
completeFile :: FilePath -> IO ()
completeFile path = renameFile path (dropExtension path ++ "_complete.csv")

outputFilePath :: String -> FilePath -> FilePath
outputFilePath searchTerm currentDirectory = currentDirectory </> "output" </> searchTerm ++ ".csv"

-- |Get contents of file as a Lazy ByteString, return empty Lazy ByteString if the file does not exist
getByteString :: FilePath -> IO LBS.ByteString
getByteString path = do
    fileExists <- doesFileExist path
    if fileExists
        then LBS.readFile path
        else return LBS.empty

-- |Parse JSON into a list of Tweet objects
scrapeTweetJSON :: TweetJSON -> Maybe [Tweet]
scrapeTweetJSON json
    | T.strip htmlText == T.pack "" = Just []
    | otherwise = scrapeStringLike htmlText tweetScraper
    where htmlText = view _itemsHTML json

-- TODO: Clean this up
-- |Take a list of tweets and recursively gather JSON tweet results until all results have been collected
allJSONTweetsOnDay :: String -> Day -> V.Vector Tweet -> IO (V.Vector Tweet)
allJSONTweetsOnDay searchTerm day tweets = do
    let (tweetMin, tweetMax) = tweetMinMax tweets
    let jsonURL = twitterJSONURL searchTerm day tweetMax tweetMin
    maybeScrapedJSON <- scrapeJSONSearchURL jsonURL
    case maybeScrapedJSON of
        Nothing -> error "Couldn't scrape Twitter JSON"
        Just scrapedJSON -> do
            let maybeScrapedResults = scrapeTweetJSON scrapedJSON
            case maybeScrapedResults of
                Nothing -> error "Couldn't scrape from JSON to Tweet"
                Just scrapedResults ->
                    if null scrapedResults
                       then return V.empty
                       else do
                           let vectorResults = V.fromList scrapedResults
                           let combinedResults = tweets V.++ vectorResults
                           nextResults <- allJSONTweetsOnDay searchTerm day combinedResults
                           return $ vectorResults V.++ nextResults

-- |Gather first page and all JSON tweet results
allTweetsOnDay :: String -> V.Vector Tweet -> Day -> IO (V.Vector Tweet)
allTweetsOnDay searchTerm tweets day
    | V.length tweets == 0 = do
        let searchURL = twitterSearchURL searchTerm day
        print searchURL
        maybeScraped <- scrapeSearchURL searchURL
        case maybeScraped of
            Nothing -> error "Scraped nothing"
            Just scraped -> allTweetsOnDay searchTerm (V.fromList scraped) day
    | otherwise = allJSONTweetsOnDay searchTerm day tweets

-- |Get a day of tweets and save them to CSV
saveDayTweets :: String -> FilePath -> Set.Set Int -> Day -> IO ()
saveDayTweets searchTerm outputPath uniqueIDs day = do
    oneDayTweets <- allTweetsOnDay searchTerm V.empty day
    saveTweets outputPath searchTerm uniqueIDs oneDayTweets day

-- TODO: Use an accumulator for the uniqueIDs to add to it the newly collected Tweets at runtime
saveYearTweets :: String -> FilePath -> Set.Set Int -> Day -> IO ()
saveYearTweets searchTerm outputPath uniqueIDs day = mapM_ (saveDayTweets searchTerm outputPath uniqueIDs) [day..(fromGregorian 2014 01 01)]
