module Main where

-- System
import System.Directory
import System.FilePath

-- Third Party
import qualified Data.Vector as V

-- First Party
import TwitterScraper

main :: IO ()
main = do
    cd <- getCurrentDirectory
    createDirectoryIfMissing True (cd </> "output/")
    let ofp = outputFilePath "tesla" cd
    csvByteString <- getByteString ofp
    let searchTerm = "tesla"
    -- TODO: Map across [startDay..(fromGregorian 2014 01 01)]
    day <- getStartDay csvByteString
    allTweets <- allTweetsOnDay searchTerm V.empty day
    saveTweets ofp searchTerm allTweets day
