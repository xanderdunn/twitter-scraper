{-# LANGUAGE TemplateHaskell #-}

module Company (
Company,
_hashtag,
_name,
_ticker,
_website,
_handle
) where

-- Third Party
import Control.Lens hiding (element) -- Consider using microlens or fclabels

data Company = Compant {__ticker :: String, __name :: String, __website :: String, __handle :: String, __hashtag :: String} deriving Show
makeLenses ''Company

-- TODO: Add CSV import
