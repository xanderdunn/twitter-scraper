-- System
import System.Directory
import System.FilePath

-- First Party
import TwitterScraper

main :: IO ()
main = do
    cd <- getCurrentDirectory
    createDirectoryIfMissing True (cd </> "output/")
    let searchTerm = "tesla"
    let ofp = outputFilePath searchTerm cd
    csvByteString <- getByteString ofp
    existingTweets <- getExistingTweets csvByteString
    let (day, uniques) = getStartValues existingTweets
    saveDayTweets searchTerm ofp uniques day
