{-# LANGUAGE DeriveGeneric #-}
module FeedService where

import Data.Aeson
import Network.URI (parseURI, URI(..))
import Data.Maybe (fromJust)
import Data.List (intercalate)
import Data.Text (Text, unpack)
import GHC.Generics
import Control.Applicative
import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy as B


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
getDate xkcd = intercalate "/" $ map (unpack . ($ xkcd)) [year, month, day]

getUrl :: Int -> String
getUrl n
    | n <= 0 = "http://xkcd.com/info.0.json"
    | otherwise = "http://xkcd.com/" ++ show n ++ "/info.0.json"

getJSON :: String -> IO B.ByteString
getJSON url = simpleHttp url

getFinalUrlPart :: URI -> String
getFinalUrlPart = reverse . takeWhile (/='/') . reverse . uriPath

getUri :: Xkcd -> URI
getUri feed = fromJust $ parseURI $ unpack $ img feed

getTitle :: Xkcd -> String
getTitle xkcd = unpack $ title xkcd

getAlt :: Xkcd -> String
getAlt xkcd = unpack $ alt xkcd

getNum :: Xkcd -> Int
getNum xkcd = num xkcd

getFeed :: String -> IO (Either String Xkcd)
getFeed url = (eitherDecode <$> getJSON url) :: IO (Either String Xkcd)