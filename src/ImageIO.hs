module ImageIO (loadImage) where

import Control.Monad
import qualified Data.ByteString.Lazy as B
import Graphics.UI.WXCore
import Network.HTTP.Conduit (simpleHttp)
import System.Directory
import System.FilePath
import System.IO.Error

import Feed

loadImage :: String -> Feed -> IO (Maybe (Bitmap ()))
loadImage baseDir f = do
  let uri = getUri f
  let fileName = getImagePath baseDir uri (getNum f)
  putStrLn $ "Loading the image file " ++ fileName
  exists <- doesFileExist fileName
  -- I don't know how to convert ByteString to Bitmap
  -- Maybe I should consider VBitmap -> WxObject?
  unless exists $ void(downloadImage fileName uri)
  -- Using unless as tryIOError doesn't switch if file not found
  r <- tryIOError $ bitmapCreateFromFile fileName
  case r of
    Left _  -> return Nothing
    Right i -> return $ Just i

downloadImage :: String -> String -> IO (Maybe B.ByteString)
downloadImage fileName uri = do
  putStrLn $ "Downloading the image file " ++ fileName
  r <- tryIOError $ simpleHttp uri
  case r of
    Left _  -> return Nothing
    Right i -> saveImage fileName i

saveImage :: String -> B.ByteString -> IO (Maybe B.ByteString)
saveImage fileName image = do
  putStrLn $ "Saving the image file " ++ fileName
  r <- tryIOError $ B.writeFile fileName image
  case r of
    Left _  -> return Nothing
    Right _ -> return $ Just image

getImagePath :: String -> String -> Int -> FilePath
getImagePath baseDir uri currentId = baseDir </> show currentId ++ "-" ++ getFinalUrlPart uri

getFinalUrlPart :: String -> String
getFinalUrlPart = reverse . takeWhile (/='/') . reverse
