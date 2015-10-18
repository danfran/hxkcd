{-# LANGUAGE DeriveGeneric #-}
module Xkcd where

import Data.Aeson
import Data.Text (Text, unpack)
import Data.Time
import GHC.Generics

data Xkcd = Xkcd {
    month :: !Text,
    num :: Int,
    link :: !Text,
    year :: !Text,
    news :: !Text,
    safe_title :: !Text,
    transcript :: !Text,
    alt :: !Text,
    img :: !Text,
    title :: !Text,
    day :: !Text
} deriving (Show,Generic)

instance FromJSON Xkcd
instance ToJSON Xkcd

getDate :: Xkcd -> String
getDate xkcd = formatTime defaultTimeLocale "%B %d %Y" (fromGregorian (toInt year) (toInt month) (toInt day))
    where
      toInt :: (Read a, Num a) => (Xkcd -> Text) -> a
      toInt = read . unpack . ($ xkcd)

getUri :: Xkcd -> String
getUri = unpack . img

getTitle :: Xkcd -> String
getTitle = unpack . title

getAlt :: Xkcd -> String
getAlt = unpack . alt

getNum :: Xkcd -> Int
getNum = num
