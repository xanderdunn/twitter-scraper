module Main where

-- System
import System.Directory
import System.FilePath

-- First Party
import TwitterScraper

main :: IO ()
main = do
    cd <- getCurrentDirectory
    let companiesPath = cd </> "output/" </> "companies.csv"
    csvBytes <- getByteString companiesPath
    companies <- getExistingCompanies csvBytes
    mapM_ saveCompanyTweets companies
