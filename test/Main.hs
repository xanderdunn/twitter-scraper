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
    let ofp = cd </> "output" </> searchTerm ++ ".csv"
    csvByteString <- getByteString ofp
    existingTweets <- getExistingTweets csvByteString
    let day = startDay existingTweets
    let uniques = uniqueTweetIDs existingTweets
    saveDayTweets searchTerm ofp uniques day
