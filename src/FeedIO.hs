module FeedIO (loadFeed, downloadFeed) where

import Data.Aeson
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import System.IO.Error

import Feed

loadFeed :: String -> Int -> IO (Maybe Feed)
loadFeed baseDir currentId = do
  let fileName = getFeedPath baseDir currentId
  putStrLn $ "Loading the feed file " ++ fileName
  r <- tryIOError $ B.readFile fileName
  case r of
    Left _  -> downloadFeed baseDir currentId
    Right f -> return $ decode f

downloadFeed :: String -> Int -> IO (Maybe Feed)
downloadFeed baseDir currentId = do
  let url = getUrl currentId
  putStrLn $ "Fetching the feed from the url " ++ url
  r <- tryIOError $ simpleHttp url
  case r of
    Left _  -> return Nothing
    Right f -> saveFeed baseDir f

saveFeed :: String -> B.ByteString -> IO (Maybe Feed)
saveFeed baseDir feed = do
  let f = decode feed
  let fileName = getFeedPath baseDir (getNum $ fromJust f)
  putStrLn $ "Saving the feed file " ++ fileName
  r <- tryIOError $ B.writeFile fileName feed
  case r of
    Left _  -> return Nothing
    Right _ -> return f

getUrl :: Int -> String
getUrl n
    | n <= 0 = "http://xkcd.com/info.0.json"
    | otherwise = "http://xkcd.com/" ++ show n ++ "/info.0.json"

getFeedPath :: String -> Int -> String
getFeedPath baseDir currentId = baseDir ++ show currentId ++ ".metadata.json"
