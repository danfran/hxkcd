{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Control.Monad.State as S
import qualified Data.Foldable as F (forM_)
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Data.Maybe (fromJust)
import Data.IORef
import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Control.Exception as E
import Network.HTTP
import Network.URI (parseURI, URI(..))
import Graphics.UI.WX
import Graphics.UI.WXCore
import System.Directory
import Feed

data Env = Env {
    baseDir :: FilePath
}

data Navigation = First | Previous | Random | Next | Last deriving (Eq)

data Components = Components {
    f :: Frame(),
    titleContainer :: StaticText(),
    dateContainer :: StaticText(),
    altContainer :: TextCtrl(),
    sw :: ScrolledWindow()
}

main :: IO ()
main = start hxkcd

hxkcd :: IO ()
hxkcd
  = do
      hd <- getHomeDirectory
      let env = Env (hd ++ "/.hxkcd/")
      createDirectoryIfMissing False $ baseDir env

      cursor <- newIORef initCursor

      f <- frame [ text := "HXKCD" ]

      p <- panel f []

      file     <- menuPane      [ text := "&Menu" ]
      first    <- menuItem file [ text := "&Start\tCtrl+S", help := "Load first image" ]
      previous <- menuItem file [ text := "&Left\tCtrl+L", help := "Load previous image" ]
      random   <- menuItem file [ text := "Ra&ndom\tCtrl+N", help := "Load random image" ]
      next     <- menuItem file [ text := "&Right\tCtrl+R", help := "Load next image" ]
      last     <- menuItem file [ text := "Las&t\tCtrl+T", help := "Load last image" ]

      tbar <- toolBar f []
      toolMenu tbar first    "First" "icons/start_left16.png" []
      toolMenu tbar previous "Previous" "icons/left16.png"    []
      toolMenu tbar random   "Random" "icons/random16.png"    []
      toolMenu tbar next     "Next" "icons/right16.png"       []
      toolMenu tbar last     "Last" "icons/end_right16.png"   []

      titleContainer <- staticText p []
      dateContainer <- staticText p []
      altContainer <- textCtrl p [ enabled := False, wrap := WrapNone ]

      let swSize = Size 750 480

      sw <- scrolledWindow p [ bgcolor := white, scrollRate := sz 10 10, virtualSize := swSize, fullRepaintOnResize := False ]

      set f [ layout := container p $ margin 10 $ grid 1 4 [
                                                            [ hfill (widget dateContainer) ],
                                                            [ hfill (widget titleContainer) ],
                                                            [ fill (widget altContainer) ],
                                                            [ fill $ minsize swSize $ widget sw ]
                                                           ]
              , clientSize := sz 800 640
            ]

      -- set actions

      let components = Components { f, titleContainer, dateContainer, altContainer, sw }

      vbitmap <- variable [ value := Nothing ]

      set f [ on (menu first)      := getImage components vbitmap cursor First env
              , on (menu previous) := getImage components vbitmap cursor Previous env
              , on (menu random)   := getImage components vbitmap cursor Random env
              , on (menu next)     := getImage components vbitmap cursor Next env
              , on (menu last)     := getImage components vbitmap cursor Last env
              , on closing :~ \previous -> do { closeImage vbitmap; previous } ]

      set sw [ on paint := onPaint vbitmap ]

      getImage components vbitmap cursor Last env

      return ()
  where
    getImage components vbitmap cursor nav env
          = do id <- fmap index (getIndex nav cursor)
               feed <- fmap fromJust (getFeedFromUrl $ getUrl id) -- lack of check

               let imageFileName = baseDir env ++ show id ++ "-" ++ getFinalUrlPart (getUri feed)
               let metadataFileName = baseDir env ++ show id ++ ".metadata.json"

               doesFileExist imageFileName >>= \exists -> unless exists (saveImage imageFileName feed) -- lack of check
               doesFileExist metadataFileName >>= \exists -> unless exists (saveMetadata metadataFileName feed) -- lack of check

               displayImage (sw components) vbitmap imageFileName
               displayMetadata components metadataFileName

    saveImage imageFileName feed
          = do imageData <- simpleHTTP (defaultGETRequest_ $ getUri feed) >>= getResponseBody -- lack of check
               B.writeFile imageFileName imageData

    saveMetadata metadataFileName feed = B.writeFile metadataFileName (encode feed)

    getIndex navigation ref
          = do
              cursor@FeedIndex { index, lastIndex } <- readIORef ref

              newCursor <- case navigation of
                                First -> return cursor { index = 1 }

                                Previous -> return $ if index > 1
                                                        then cursor { index = index - 1 }
                                                        else cursor

                                Random -> do rn <- getRandomNum lastIndex
                                             return cursor { index = rn }

                                Next -> return $ if index < lastIndex
                                                    then cursor { index = index + 1 }
                                                    else cursor

                                Last -> do feed <- S.liftIO $ getFeedFromUrl $ getUrl 0
                                           let i = getNum $ fromJust feed
                                           return $ cursor { lastIndex = i, index = i }

              print $ show newCursor

              writeIORef ref newCursor
              return newCursor

    onPaint vbitmap dc viewArea
          = do logNullCreate -- to prevent iCCP warning dialog
               mbBitmap <- get vbitmap value
               case mbBitmap of
                 Nothing -> return ()
                 Just bm -> drawBitmap dc bm pointZero False []

    closeImage vbitmap
          = do mbBitmap <- swap vbitmap value Nothing
               F.forM_ mbBitmap objectDelete

    displayMetadata components fname
          = do feed <- fmap fromJust (getFeedFromFile fname)
               set (titleContainer components) [ text := getTitle feed ]
               set (dateContainer components)  [ text := getDate feed ]
               set (altContainer components)   [ text := getAlt feed ]

    displayImage sw vbitmap fname
          = do bm <- bitmapCreateFromFile fname  -- can fail with exception
               closeImage vbitmap
               set vbitmap [ value := Just bm ]
               -- resize
               bmsize <- get bm size
               set sw [ virtualSize := bmsize ]
               repaint sw
