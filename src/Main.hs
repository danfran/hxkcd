{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson
import qualified Data.Foldable as F (forM_)
import qualified Data.ByteString.Lazy as B
import Data.IORef
import Control.Applicative
import Control.Monad
import Network.HTTP
import Network.URI (parseURI, URI(..))
import Graphics.UI.WX
import Graphics.UI.WXCore
import FeedService


main :: IO ()
main = start hxkcd

hxkcd :: IO ()
hxkcd
  = do
      ref <- newIORef (0)

      f <- frame [ text := "HXKCD" ]

      p <- panel f []

      vbitmap <- variable [ value := Nothing ]

      file     <- menuPane      [ text := "&Menu" ]
      first    <- menuItem file [ text := "&Start\tCtrl+S", help := "Load first image" ]
      left     <- menuItem file [ text := "&Left\tCtrl+L", help := "Load previous image" ]
      random   <- menuItem file [ text := "Ra&ndom\tCtrl+N", help := "Load random image" ]
      right    <- menuItem file [ text := "&Right\tCtrl+R", help := "Load next image" ]
      last     <- menuItem file [ text := "Las&t\tCtrl+T", help := "Load last image" ]

      tbar <- toolBar f []
      toolMenu tbar first  "First" "icons/start_left16.png"  []
      toolMenu tbar left  "Previous" "icons/left16.png"  []
      toolMenu tbar random  "Random" "icons/random16.png"  []
      toolMenu tbar right "Next" "icons/right16.png" []
      toolMenu tbar last "Last" "icons/end_right16.png" []

      titleContainer <- staticText p []
      dateContainer <- staticText p []
      altContainer <- staticText p []

      sw <- panel p [ on paint := onPaint vbitmap, bgcolor := white, fullRepaintOnResize := False ]

      set f [ layout := container p $ grid 1 4 [
                                                [ hfill (widget titleContainer) ],
                                                [ hfill (widget dateContainer) ],
                                                [ hfill (widget altContainer) ],
                                                [ fill (widget sw) ]
                                               ]
              , clientSize := sz 300 200
              , on (menu first) := do { writeIORef ref 1; updateImage titleContainer dateContainer altContainer sw f vbitmap ref }
              , on (menu left) := do { modifyIORef' ref (subtract 1); updateImage titleContainer dateContainer altContainer sw f vbitmap ref }
              , on (menu right) := do { modifyIORef' ref (+1); updateImage titleContainer dateContainer altContainer sw f vbitmap ref }
              , on (menu last) := do { writeIORef ref 0; updateImage titleContainer dateContainer altContainer sw f vbitmap ref }
              , on closing :~ \previous -> do { closeImage vbitmap; previous } ]

      updateImage titleContainer dateContainer altContainer sw f vbitmap ref

      return ()
  where
    updateImage titleContainer dateContainer altContainer sw f vbitmap ref
          = do ref' <- readIORef ref
               xkcdFeed <- getFeed $ getUrl ref'

               case xkcdFeed of
                      Left err -> return ()
                      Right feed -> do let uri = getUri feed

                                       imageData <- simpleHTTP (defaultGETRequest_ uri) >>= getResponseBody

                                       set titleContainer [ text := getTitle feed ]
                                       set dateContainer [ text := getDate feed ]
                                       set altContainer [ text := getAlt feed ]

                                       let fileName = getFinalUrlPart uri
                                       B.writeFile fileName imageData
                                       openImage sw vbitmap f fileName

                                       writeIORef ref $ getNum feed

                                       resizeWindow titleContainer dateContainer altContainer sw f

    resizeWindow title date alt sw f
          = do sws <- get sw size
               tcs <- get title size
               dcs <- get date size
               acs <- get alt size

               -- let maxW = foldl (max . sizeW) 100 [ sws, tcs, dcs, acs ]
               let maxW = maximum $ map sizeW [ sws, tcs, dcs, acs ]
               let maxH = sum $ map sizeH [ sws, tcs, dcs, acs ]

               set f [ clientSize := Size maxW maxH ]

    onPaint vbitmap dc viewArea
          = do mbBitmap <- get vbitmap value
               case mbBitmap of
                 Nothing -> return ()
                 Just bm -> drawBitmap dc bm pointZero False []

    closeImage vbitmap
          = do mbBitmap <- swap vbitmap value Nothing
               F.forM_ mbBitmap objectDelete

    openImage sw vbitmap container fname
          = do bm <- bitmapCreateFromFile fname  -- can fail with exception
               closeImage vbitmap
               set vbitmap [ value := Just bm ]
               -- resize
               bmsize <- get bm size
               set sw [ clientSize := bmsize ]
               repaint sw
--           `catch` \err -> repaint sw
