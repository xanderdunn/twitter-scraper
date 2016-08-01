{-# LANGUAGE TemplateHaskell #-}

module Company (
Company,
_hashtag,
_name,
_ticker,
_website,
_handle,
getExistingCompanies,
) where

-- System
import Control.Monad
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector as V

-- Third Party
import Control.Lens hiding (element) -- Consider using microlens or fclabels
import Data.Csv

data Company = Company {__ticker :: String, __name :: String, __website :: String, __handle :: String, __hashtag :: String} deriving Show
makeLenses ''Company

csvToCompanies :: LBS.ByteString -> Either String (V.Vector Company)
csvToCompanies = decode HasHeader

-- TODO: Reduce duplication with getExistingTweets
getExistingCompanies :: LBS.ByteString -> IO (V.Vector Company)
getExistingCompanies csvByteString = case csvToCompanies csvByteString of
        Left msg -> error $ "Could not parse CSV with error: " ++ msg
        Right companies -> return companies

instance FromRecord Company where
    parseRecord v
      | length v == 5 = Company <$>
                          v .! 0 <*>
                          v .! 1 <*>
                          v .! 2 <*>
                          v .! 3 <*>
                          v .! 4
      | otherwise     = mzero
