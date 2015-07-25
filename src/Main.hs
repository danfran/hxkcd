{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Control.Monad.State as S
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as B
import qualified Data.Foldable as F (forM_)
import Data.IORef
import Graphics.UI.WX hiding (when)
import Graphics.UI.WXCore hiding (when)
import Network.HTTP
import System.Directory

import Feed
import Xkcd

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

      -- start

      getImage components vbitmap cursor Last env

      return ()
  where
    getImage components vbitmap ref navigation env
          = do cursor@FeedIndex { index, lastIndex } <- readIORef ref

               cursor@FeedIndex { index, lastIndex }
                <- case navigation of
                     First -> return cursor { index = 1 }

                     Previous -> return $ if index > 1 then cursor { index = index - 1 } else cursor

                     Random -> getRandomNum lastIndex >>= \rn -> return $ cursor { index = rn }

                     Next -> return $ if index < lastIndex then cursor { index = index + 1 } else cursor

                     Last -> catch updateAsLast (\(_ :: SomeException) -> return cursor)
                             where updateAsLast = do r <- downloadFeed $ getUrl 0
                                                     let id = getNum r
                                                     let fileName = getFeedPath env id
                                                     saveFeed fileName r
                                                     displayContent env id components vbitmap r
                                                     return $ cursor { lastIndex = id, index = id }

               print $ show cursor
               writeIORef ref cursor
               fetchFeed env index components vbitmap navigation >>= unless (navigation == Last)
               return ()

    fetchFeed env id components vbitmap navigation
          = do let fileName = getFeedPath env id
               exists <- doesFileExist fileName
               return $ if exists then catch (loadFeed fileName >>= displayContent env id components vbitmap)
                                             (\(e :: SomeException) -> putStrLn $ "Error: " ++ show e)
                                  else catch (downloadFeed (getUrl id) >>= saveFeed fileName >>= displayContent env id components vbitmap)
                                             (\(e :: SomeException) -> putStrLn $ "Error: " ++ show e)

    displayContent env id components vbitmap feed =
         fetchImage env id components vbitmap (getUri feed) >> displayMetadata components feed

    fetchImage env id components vbitmap uri
         = do let fileName = getImagePath env id uri
              exists <- doesFileExist fileName
              unless exists $ catch (downloadImage uri >>= saveImage fileName >> return ())
                                    (\(e :: SomeException) -> putStrLn $ "Error: " ++ show e)
              catch (loadImage fileName >>= displayImage (sw components) vbitmap)
                    (\(e :: SomeException) -> putStrLn $ "Error: " ++ show e)

    onPaint vbitmap dc viewArea
          = do logNullCreate -- to prevent iCCP warning dialog
               mbBitmap <- get vbitmap value
               case mbBitmap of
                 Nothing -> return ()
                 Just bm -> drawBitmap dc bm pointZero False []

    closeImage vbitmap
          = do mbBitmap <- swap vbitmap value Nothing
               F.forM_ mbBitmap objectDelete

    displayMetadata components feed
          = do set (titleContainer components) [ text := getTitle feed ]
               set (dateContainer components)  [ text := getDate feed ]
               set (altContainer components)   [ text := getAlt feed ]

    displayImage sw vbitmap bm
          = do closeImage vbitmap
               set vbitmap [ value := Just bm ]
               -- resize
               bmsize <- get bm size
               set sw [ virtualSize := bmsize ]
               repaint sw
