module Main where

-- System
import System.Directory
import System.FilePath

-- Third Party
import qualified Data.Vector as Vec

-- First Party
import TwitterScraper

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
    scraped <- scrapeSearchURL searchURL
    case scraped of
        Nothing -> error "Scraped nothing"
        Just results -> do
            print results
            let (tweetMin, tweetMax) = tweetMinMax (Vec.fromList results)
            let jsonURL = twitterJSONURL searchTerm day tweetMax tweetMin
            print jsonURL
