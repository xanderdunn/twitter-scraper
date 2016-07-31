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
import Control.Applicative
import Control.Monad
import Data.Char
import qualified Data.Text as T
import qualified Data.Set as Set
import System.Directory
import System.FilePath

-- Third Party
import Control.Exception.Extra
import Control.Lens hiding (element) -- Consider using microlens or fclabels
import qualified Data.ByteString.Lazy as LBS
import Data.Csv
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.Vector as V
import Text.HTML.Scalpel

-- First Party
import TweetJSON (TweetJSON, scrapeJSONSearchURL, _itemsHTML)

-- |The Twitter search URL for a given search term and day
twitterSearchURL :: String -> Day -> String
twitterSearchURL searchTerm day = "https://twitter.com/search?q=" ++ searchTerm ++ "%20lang%3Aen%20since%3A" ++ showGregorian day ++"%20until%3A" ++ showGregorian(addDays 1 day) ++ "&src=typd"

-- |The Twitter JSON response URLs for all search results beyond the first page
twitterJSONURL :: String -> Day -> Integer -> Integer -> String
twitterJSONURL searchTerm day tweetMax tweetMin = "https://twitter.com/i/search/timeline?vertical=news&q=" ++ searchTerm ++ "%20lang%3Aen%20since%3A" ++ showGregorian day ++ "%20until%3A" ++ showGregorian(addDays 1 day) ++ "&src=typd&include_available_features=1&include_entities=1&max_position=TWEET-" ++ show tweetMin ++ "-" ++ show tweetMax ++ "-BD1UO2FFu9QAAAAAAAAETAAAAAcAAAASAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA&reset_error_state=false"

data Tweet = Tweet { __unique :: Integer, __author :: T.Text, __location :: T.Text, __retweets :: Int, __likes :: Int, __body :: T.Text, __date :: T.Text } deriving (Show)
makeLenses ''Tweet

instance ToRecord Tweet where
    toRecord (Tweet uniqueF authorF locationF retweetsF likesF bodyF dateF) = record [toField uniqueF, toField authorF, toField locationF, toField retweetsF, toField likesF, toField bodyF, toField dateF]

instance FromRecord Tweet where
    parseRecord v
      | length v == 7 = Tweet <$>
                          v .! 0 <*>
                          v .! 1 <*>
                          v .! 2 <*>
                          v .! 3 <*>
                          v .! 4 <*>
                          v .! 5 <*>
                          v .! 6
      | otherwise     = mzero

tweetMinMax :: V.Vector Tweet -> (Integer, Integer)
tweetMinMax tweets = (minID, maxID)
    where headTweet = V.head tweets
          maxID = view _unique headTweet
          minID = view _unique (V.last tweets)

scrapeSearchURL :: String -> IO (Maybe [Tweet])
scrapeSearchURL url = scrapeURL url tweetScraper

-- TODO: Surely these can be generalized rather than duplication for Int, Integer?
readSafeInt :: T.Text -> Int
readSafeInt t
    | null string = 0
    | last string == 'K' = round ((read (init string):: Double) * 1000) :: Int
    | otherwise = case reads string of
                    [(x, "")] -> x
                    _ -> error $ "Attempted to convert \"" ++ string ++ "\" to an Int, but can't be done."
    where string = T.unpack t

textToInteger :: T.Text -> Integer
textToInteger t
    | all isDigit string = read string :: Integer
    | null string = 0
    | otherwise = error $ "Attempted to convert " ++ string ++ " to an Int, but can't be done."
    where string = T.unpack t

-- | Scraper defined for the Scalpel library to scrape the HTML
tweetScraper :: Scraper T.Text [Tweet]
tweetScraper = tweets
   where
       tweets :: Scraper T.Text [Tweet]
       tweets = chroots ("div" @: [hasClass "js-stream-tweet"]) infos

       infos:: Scraper T.Text Tweet
       infos = do
           uniqueT <- attr "data-tweet-id" Any
           authorT <- attr "data-screen-name" Any
           date <- attr "data-time-ms" ("span"  @: [hasClass "_timestamp"])
           location <- text ("span" @: [hasClass "Tweet-geo"]) <|> attr "nothing" Any
           bodyT <- text $ "div"  @: [hasClass "js-tweet-text-container"]
           counters <- texts $ "span" @: [hasClass "ProfileTweet-actionCountForPresentation"]
           let retweetsT = head counters
           let likesT = counters !! 2
           return Tweet {__unique = textToInteger uniqueT, __author = authorT, __location = T.strip location, __retweets = readSafeInt retweetsT, __likes = readSafeInt likesT, __body = T.strip bodyT, __date = date}


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
getStartValues :: V.Vector Tweet -> (Day, Set.Set Integer)
getStartValues tweets = (startDay tweets, uniques)
    where uniques = Set.fromList $ map (view _unique) (V.toList tweets)

csvContents :: LBS.ByteString -> Either String (V.Vector Tweet)
csvContents = decode NoHeader

-- |Save a V.Vector of Tweet objects to CSV
saveTweets :: FilePath -> String -> Set.Set Integer -> V.Vector Tweet -> Day -> IO ()
saveTweets path searchTerm existingIDs tweets day = do
    let newTweets = uniqueTweets existingIDs tweets
    LBS.appendFile path (encode (V.toList newTweets))
    print $ show (V.length newTweets) ++ " " ++ showGregorian day ++ " " ++ searchTerm ++ " tweets saved to " ++ path

-- TODO: This can be made more elegant and efficient
-- |Given a Set of already saved unique Tweet IDs and new Tweet objects, return a Vector of Tweet objects that are not in the Set of unique IDs
uniqueTweets :: Set.Set Integer -> V.Vector Tweet -> V.Vector Tweet
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
    maybeScrapedJSON <- retry 3 (scrapeJSONSearchURL jsonURL)
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
            Just scraped -> do
                jsonTweets <- allTweetsOnDay searchTerm scrapedVector day
                return $ scrapedVector  V.++ jsonTweets
                where scrapedVector = V.fromList scraped
    | otherwise = allJSONTweetsOnDay searchTerm day tweets

-- |Get a day of tweets and save them to CSV
saveDayTweets :: String -> FilePath -> Set.Set Integer -> Day -> IO ()
saveDayTweets searchTerm outputPath uniqueIDs day = do
    oneDayTweets <- allTweetsOnDay searchTerm V.empty day
    saveTweets outputPath searchTerm uniqueIDs oneDayTweets day

-- TODO: Use an accumulator for the uniqueIDs to add to it the newly collected Tweets at runtime
saveYearTweets :: String -> FilePath -> Set.Set Integer -> Day -> IO ()
saveYearTweets searchTerm outputPath uniqueIDs day = mapM_ (saveDayTweets searchTerm outputPath uniqueIDs) [day..(fromGregorian 2014 01 01)]
