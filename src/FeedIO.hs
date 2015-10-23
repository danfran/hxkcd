{-# LANGUAGE NamedFieldPuns #-}
module FeedIO where

import Control.Monad.Trans
import Data.Aeson
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import System.Directory
import System.IO.Error
import System.Random

import Feed

downloadFeed :: String -> IO (Maybe Feed)
downloadFeed url = do
  r <- liftIO $ tryIOError $ simpleHttp url
  case r of
    Left _  -> return Nothing
    Right f -> return $ decode f

loadFeed :: String -> IO (Maybe Feed)
loadFeed path = do
  r <- liftIO $ tryIOError $ B.readFile path
  case r of
    Left _  -> return Nothing
    Right f -> return $ decode f

saveFeed :: String -> Feed -> IO (Maybe Feed)
saveFeed path feed = do
  r <- liftIO $ tryIOError $ B.writeFile path $ encode feed
  case r of
    Left _  -> return Nothing
    Right _ -> return $ Just feed

-- bad - should not reuse the lastIndex call but the current AppState
updateAsRandom :: String -> IO Int
updateAsRandom baseDir = updateAsLast baseDir >>= \lastIndex -> getStdRandom (randomR (1, lastIndex))

updateAsLast :: String -> IO Int
updateAsLast baseDir
    = do r <- downloadFeed $ getUrl 0
         let f = fromJust r
         let num = getNum f
         _ <- saveFeed (getFeedPath baseDir num) f
         return num

fetchFeed :: String -> Int -> IO (Maybe Feed)
fetchFeed baseDir currentId
     = do let fileName = getFeedPath baseDir currentId
          putStrLn $ "Executing the fetch with id " ++ show currentId ++ " to get the feed file " ++ fileName
          exists <- doesFileExist fileName
          if exists then loadFeed fileName
                    else downloadFeed (getUrl currentId) >>= \f -> saveFeed fileName $ fromJust f

getUrl :: Int -> String
getUrl n
    | n <= 0 = "http://xkcd.com/info.0.json"
    | otherwise = "http://xkcd.com/" ++ show n ++ "/info.0.json"

getFeedPath :: String -> Int -> String
getFeedPath baseDir currentId = baseDir ++ show currentId ++ ".metadata.json"
