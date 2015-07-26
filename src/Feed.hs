{-# LANGUAGE NamedFieldPuns #-}
module Feed where

import Control.Monad.Reader
import Control.Monad.Trans
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.IORef
import Data.Maybe (fromJust)
import Network.HTTP
import Network.HTTP.Conduit (simpleHttp)
import System.Directory
import System.IO.Error
import Graphics.UI.WXCore

import Xkcd

data FeedIndex = FeedIndex {
    lastIndex :: Int,
    index :: Int
} deriving (Show)


initCursor = FeedIndex { lastIndex = 0, index = 0 }

getUrl :: Int -> String
getUrl n
    | n <= 0 = "http://xkcd.com/info.0.json"
    | otherwise = "http://xkcd.com/" ++ show n ++ "/info.0.json"

getFinalUrlPart :: String -> String
getFinalUrlPart = reverse . takeWhile (/='/') . reverse

-- image

downloadImage :: String -> IO (Maybe B.ByteString)
downloadImage uri = do
  r <- liftIO $ tryIOError $ simpleHttp uri
  case r of
    Left e  -> return Nothing
    Right f -> return $ Just f

loadImage :: String -> IO (Maybe (Bitmap ()))
loadImage path = do
  r <- liftIO $ tryIOError $ bitmapCreateFromFile path
  case r of
    Left e  -> return Nothing
    Right f -> return $ Just f

saveImage :: String -> B.ByteString -> IO (Maybe B.ByteString)
saveImage path image = do
  r <- liftIO $ tryIOError $ B.writeFile path image
  case r of
    Left e  -> return Nothing
    Right f -> return $ Just image


-- feed

downloadFeed :: String -> IO (Maybe Xkcd)
downloadFeed url = do
  r <- liftIO $ tryIOError $ simpleHttp url
  case r of
    Left e  -> return Nothing
    Right f -> return $ decode f

loadFeed :: String -> IO (Maybe Xkcd)
loadFeed path = do
  r <- liftIO $ tryIOError $ B.readFile path
  case r of
    Left e  -> return Nothing
    Right f -> return $ decode f

saveFeed :: String -> Xkcd -> IO (Maybe Xkcd)
saveFeed path feed = do
  r <- liftIO $ tryIOError $ B.writeFile path $ encode feed
  case r of
    Left e  -> return Nothing
    Right f -> return $ Just feed
