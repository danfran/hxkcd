{-# LANGUAGE NamedFieldPuns #-}
module ImageIO where

import Control.Monad.Reader
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromJust)
import Graphics.UI.WXCore
import Network.HTTP.Conduit (simpleHttp)
import System.Directory
import System.IO.Error

import Feed -- to be removed & export module functions to set

downloadImage :: String -> IO (Maybe B.ByteString)
downloadImage uri = do
  r <- liftIO $ tryIOError $ simpleHttp uri
  case r of
    Left _  -> return Nothing
    Right f -> return $ Just f

loadImage :: String -> IO (Maybe (Bitmap ()))
loadImage path = do
  r <- liftIO $ tryIOError $ bitmapCreateFromFile path
  case r of
    Left _  -> return Nothing
    Right f -> return $ Just f

saveImage :: String -> B.ByteString -> IO (Maybe B.ByteString)
saveImage path image = do
  r <- liftIO $ tryIOError $ B.writeFile path image
  case r of
    Left _  -> return Nothing
    Right _ -> return $ Just image

fetchImage :: String -> Feed -> IO (Maybe (Bitmap ()))
fetchImage baseDir f
     = do let uri = getUri f
          let fileName = getImagePath baseDir uri (getNum f)
          putStrLn $ "Image saved in " ++ fileName
          exists <- doesFileExist fileName
          unless exists $ downloadImage uri >>= \i -> void (saveImage fileName $ fromJust i)
          loadImage fileName

-- here and in FeedIO to be moved in Feed
getImagePath :: String -> String -> Int -> String
getImagePath baseDir uri currentId = baseDir ++ show currentId ++ "-" ++ getFinalUrlPart uri

getFinalUrlPart :: String -> String
getFinalUrlPart = reverse . takeWhile (/='/') . reverse
