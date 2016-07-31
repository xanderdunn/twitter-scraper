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
    let searchTerm = "tesla"
    -- TODO: Map across a list of companies, where each company has a list of search terms.  Prevent duplicate tweets across all files for a given company
    -- TODO: Check for existing _complete.csv and skip
    existingTweets <- getExistingTweets csvByteString
    let (day, uniques) = getStartValues existingTweets
    print $ show (length existingTweets) ++ " tweets already collected"
    print $ show (length uniques) ++ " unique IDs already collected"
