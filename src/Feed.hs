{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
module Feed where

import Data.Aeson
import Network.URI (parseURI, URI(..))
import Data.Maybe (fromJust)
import Data.List (intercalate)
import Data.Text (Text, unpack)
import Control.Monad.State
import Data.IORef
import Network.HTTP
import GHC.Generics
import Control.Applicative
import Network.HTTP.Conduit (simpleHttp)
import System.Locale
import System.Random
import Data.Time
import Data.Time.Format
import qualified Data.ByteString.Lazy as B

data FeedIndex = FeedIndex {
    lastIndex :: Int,
    index :: Int
} deriving (Show)

initCursor = FeedIndex { lastIndex = 0, index = 0 }

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
getDate xkcd = formatTime defaultTimeLocale "%B %d %Y" date
               where date = readTime defaultTimeLocale "%Y/%-m/%-d" feedDate :: UTCTime
                     feedDate = intercalate "/" $ map (unpack . ($ xkcd)) [year, month, day]

getUrl :: Int -> String
getUrl n
    | n <= 0 = "http://xkcd.com/info.0.json"
    | otherwise = "http://xkcd.com/" ++ show n ++ "/info.0.json"

getJSON :: String -> IO B.ByteString
getJSON = simpleHttp

getFinalUrlPart :: URI -> String
getFinalUrlPart = reverse . takeWhile (/='/') . reverse . uriPath

getUri :: Xkcd -> URI
getUri feed = fromJust $ parseURI $ unpack $ img feed

getTitle :: Xkcd -> String
getTitle xkcd = unpack $ title xkcd

getAlt :: Xkcd -> String
getAlt xkcd = unpack $ alt xkcd

getNum :: Xkcd -> Int
getNum = num

getRandomNum :: Int -> IO Int
getRandomNum upperLimit = getStdRandom (randomR (1,upperLimit))

getFeed :: String -> IO (Maybe Xkcd)
getFeed url = (decode <$> getJSON url) :: IO (Maybe Xkcd)
