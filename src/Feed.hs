{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Feed where

import Control.Exception
import Control.Monad.Trans
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.IORef
import Data.Maybe (fromJust)
import Data.Typeable
import Network.HTTP
import Network.HTTP.Conduit (simpleHttp)
import System.Directory
import System.IO.Error
import System.Random
import Graphics.UI.WXCore

import Xkcd

data DownloadFeed = DownloadFeed deriving (Show, Typeable)
data LoadFeed = LoadFeed deriving (Show, Typeable)
data SaveFeed = SaveFeed deriving (Show, Typeable)

data DownloadImage = DownloadImage deriving (Show, Typeable)
data LoadImage = LoadImage deriving (Show, Typeable)
data SaveImage = SaveImage deriving (Show, Typeable)

instance Exception DownloadFeed
instance Exception LoadFeed
instance Exception SaveFeed

instance Exception DownloadImage
instance Exception LoadImage
instance Exception SaveImage

data Env = Env {
    baseDir :: FilePath
}

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

getRandomNum :: Int -> IO Int
getRandomNum upperLimit = getStdRandom (randomR (1, upperLimit))

getFeedPath :: Env -> Int -> String
getFeedPath env id = baseDir env ++ show id ++ ".metadata.json"

getImagePath :: Env -> Int -> String -> String
getImagePath env id uri = baseDir env ++ show id ++ "-" ++ getFinalUrlPart uri

-- image

downloadImage :: String -> IO (B.ByteString)
downloadImage uri = do
  r <- liftIO $ tryIOError $ simpleHttp uri
  case r of
    Left e  -> throw DownloadImage
    Right f -> return f

loadImage :: String -> IO (Bitmap ())
loadImage path = do
  r <- liftIO $ tryIOError $ bitmapCreateFromFile path
  case r of
    Left e  -> throw LoadImage
    Right f -> return f

saveImage :: String -> B.ByteString -> IO (B.ByteString)
saveImage path image = do
  r <- liftIO $ tryIOError $ B.writeFile path image
  case r of
    Left e  -> throw SaveImage
    Right f -> return image


-- feed

downloadFeed :: String -> IO (Xkcd)
downloadFeed url = do
  r <- liftIO $ tryIOError $ simpleHttp url
  case r of
    Left e  -> throw DownloadFeed
    Right f -> return $ fromJust $ decode f

loadFeed :: String -> IO (Xkcd)
loadFeed path = do
  r <- liftIO $ tryIOError $ B.readFile path
  case r of
    Left e  -> throw LoadFeed
    Right f -> return $ fromJust $ decode f

saveFeed :: String -> Xkcd -> IO (Xkcd)
saveFeed path feed = do
  r <- liftIO $ tryIOError $ B.writeFile path $ encode feed
  case r of
    Left e  -> throw SaveFeed
    Right f -> return feed
