module Main where

-- System
import System.Directory
import System.FilePath

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
    let searchURL = twitterSearchURL "tesla" day
    print searchURL
    scraped <- scrapeSearchURL searchURL
    case scraped of
        Nothing -> error "Scraped nothing"
        Just results -> print results
