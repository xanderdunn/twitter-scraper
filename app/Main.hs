module Main where

-- System
import System.Directory
import System.FilePath

-- Third Party
import qualified Data.Vector as V
import Data.Time.Calendar

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
    allTweets <- allTweetsOnDay searchTerm day V.empty
    print $ show (V.length allTweets) ++ " " ++ searchTerm ++ " tweets collected on " ++ showGregorian day
