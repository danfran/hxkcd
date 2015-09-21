{-# LANGUAGE NamedFieldPuns #-}
module Feed where

import Control.Monad.Trans
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import System.IO.Error

import Xkcd

getUrl :: Int -> String
getUrl n
    | n <= 0 = "http://xkcd.com/info.0.json"
    | otherwise = "http://xkcd.com/" ++ show n ++ "/info.0.json"

getFinalUrlPart :: String -> String
getFinalUrlPart = reverse . takeWhile (/='/') . reverse

downloadFeed :: String -> IO (Maybe Xkcd)
downloadFeed url = do
  r <- liftIO $ tryIOError $ simpleHttp url
  case r of
    Left _  -> return Nothing
    Right f -> return $ decode f

loadFeed :: String -> IO (Maybe Xkcd)
loadFeed path = do
  r <- liftIO $ tryIOError $ B.readFile path
  case r of
    Left _  -> return Nothing
    Right f -> return $ decode f

saveFeed :: String -> Xkcd -> IO (Maybe Xkcd)
saveFeed path feed = do
  r <- liftIO $ tryIOError $ B.writeFile path $ encode feed
  case r of
    Left _  -> return Nothing
    Right _ -> return $ Just feed
