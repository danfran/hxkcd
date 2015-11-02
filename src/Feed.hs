{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
module Feed where

import Data.Aeson
import Data.Text (Text, unpack)
import Data.Time
import GHC.Generics
#if ! MIN_VERSION_time(1,5,0)
import System.Locale
#endif

data Feed = Feed {
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

instance FromJSON Feed
instance ToJSON Feed

getDate :: Feed -> String
getDate xkcd = formatTime defaultTimeLocale "%B %d %Y" (fromGregorian (toInt year) (toInt month) (toInt day))
    where
      toInt :: (Read a, Num a) => (Feed -> Text) -> a
      toInt = read . unpack . ($ xkcd)

getUri :: Feed -> String
getUri = unpack . img

getTitle :: Feed -> String
getTitle = unpack . title

getAlt :: Feed -> String
getAlt = unpack . alt

getNum :: Feed -> Int
getNum = num
