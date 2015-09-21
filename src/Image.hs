{-# LANGUAGE NamedFieldPuns #-}
module Image where

import Control.Monad.Reader
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import System.IO.Error
import Graphics.UI.WXCore

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
