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
      toolMenu tbar first  "First" "icons/start_left16.png"  []
      toolMenu tbar previous  "Previous" "icons/left16.png"  []
      toolMenu tbar random  "Random" "icons/random16.png"  []
      toolMenu tbar next "Next" "icons/right16.png" []
      toolMenu tbar last "Last" "icons/end_right16.png" []

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

      set f [ on (menu first)      := updateImage components vbitmap cursor First env
              , on (menu previous) := updateImage components vbitmap cursor Previous env
              , on (menu random)   := updateImage components vbitmap cursor Random env
              , on (menu next)     := updateImage components vbitmap cursor Next env
              , on (menu last)     := updateImage components vbitmap cursor Last env
              , on closing :~ \previous -> do { closeImage vbitmap; previous } ]

      set sw [ on paint := onPaint vbitmap ]

      updateImage components vbitmap cursor Last env

      return ()
  where
    updateImage components vbitmap cursor nav env
          = do feedIndex <- getIndex nav cursor
               xkcdFeed <- getFeed $ getUrl $ index feedIndex

               let feed = fromJust xkcdFeed

               let uri = getUri feed

               imageData <- simpleHTTP (defaultGETRequest_ uri) >>= getResponseBody
               set (titleContainer components) [ text := getTitle feed ]
               set (dateContainer components) [ text := getDate feed ]
               set (altContainer components) [ text := getAlt feed ]

               hd <- getHomeDirectory
               let fileName =  baseDir env ++ getFinalUrlPart uri
               B.writeFile fileName imageData
               openImage (sw components) vbitmap f fileName

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

                                Last -> do feed <- S.liftIO $ getFeed $ getUrl 0
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

    openImage sw vbitmap container fname
          = do bm <- bitmapCreateFromFile fname  -- can fail with exception
               closeImage vbitmap
               set vbitmap [ value := Just bm ]
               -- resize
               bmsize <- get bm size
               set sw [ virtualSize := bmsize ]
               repaint sw
