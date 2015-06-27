{-# LANGUAGE DeriveGeneric #-}
module Feed where

import Data.Aeson
import Network.URI (parseURI, URI(..))
import Data.Maybe (fromJust)
import Data.List (intercalate)
import Data.Text (Text, unpack)
import Control.Monad.State
import Network.HTTP
import GHC.Generics
import Control.Applicative
import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy as B

type Feed a = StateT FeedIndex IO a

data FeedIndex = FeedIndex {
    lastIndex :: Int,
    index :: Int
} deriving (Show)

startIndex = FeedIndex { lastIndex = 0, index = 0 }

data Navigation = First | Previous | Random | Next | Last

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

getFeed :: String -> IO (Maybe Xkcd)
getFeed url = (decode <$> getJSON url) :: IO (Maybe Xkcd)

getIndex :: Navigation -> Feed FeedIndex
getIndex navigation = do
  cursor <- get
  newCursor <- case navigation of
                    First -> return cursor { index = 1 }

                    Previous -> if index cursor > 1
                                    then return cursor { index = index cursor - 1 }
                                    else return cursor

                    Next -> if index cursor < lastIndex cursor
                                then return cursor { index = index cursor + 1 }
                                else return cursor

                    Last -> do feed <- liftIO $ getFeed $ getUrl 0
                               let i = getNum $ fromJust feed
                               return $ cursor { lastIndex = i, index = i }

                    otherwise -> return cursor
  put newCursor
  return newCursor
