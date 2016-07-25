module Main where

-- System
import System.Directory
import System.FilePath

-- Third Party
import qualified Data.Vector as Vec

-- First Party
import TwitterScraper

-- TODO: Clean up this mess
main :: IO ()
main = do
    cd <- getCurrentDirectory
    createDirectoryIfMissing True (cd </> "output/")
    let ofp = outputFilePath "tesla" cd
    csvByteString <- getByteString ofp
    day <- getStartDay csvByteString
    -- TODO: Map across [startDay..(fromGregorian 2014 01 01)]
    let searchTerm = "tesla"
    let searchURL = twitterSearchURL searchTerm day
    print searchURL
    maybeScraped <- scrapeSearchURL searchURL
    case maybeScraped of
        Nothing -> error "Scraped nothing"
        Just scraped -> do
            print scraped
            let (tweetMin, tweetMax) = tweetMinMax (Vec.fromList scraped)
            let jsonURL = twitterJSONURL searchTerm day tweetMax tweetMin
            maybeScrapedJSON <- scrapeJSONSearchURL jsonURL
            case maybeScrapedJSON of
                Nothing -> error "Couldn't scrape Twitter JSON"
                Just scrapedJSON -> do
                    let maybeScrapedResults = scrapeTweetJSON scrapedJSON
                    case maybeScrapedResults of
                        Nothing -> error "Couldn't scrape from JSON to Tweet"
                        Just scrapedResults -> print scrapedResults
