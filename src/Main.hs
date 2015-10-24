module Main where

import Graphics.UI.WX hiding (Event, newEvent)
import Graphics.UI.WXCore hiding (Event)

import AppNetwork

main :: IO ()
main = start hxkcd

hxkcd :: IO ()
hxkcd
  = do f <- frame [ text := "HXKCD" ]
       p <- panel f []

       tbMenu     <- menuPane        [ text := "&Menu" ]
       tbFirst    <- menuItem tbMenu [ text := "&Start\tCtrl+S", help := "Load first image" ]
       tbPrevious <- menuItem tbMenu [ text := "&Left\tCtrl+L", help := "Load previous image" ]
       tbRandom   <- menuItem tbMenu [ text := "Ra&ndom\tCtrl+N", help := "Load random image" ]
       tbNext     <- menuItem tbMenu [ text := "&Right\tCtrl+R", help := "Load next image" ]
       tbLast     <- menuItem tbMenu [ text := "Las&t\tCtrl+T", help := "Load last image" ]

       tbar <- toolBar f []
       _ <- toolMenu tbar tbFirst    "First" "icons/start_left16.png" []
       _ <- toolMenu tbar tbPrevious "Previous" "icons/left16.png"    []
       _ <- toolMenu tbar tbRandom   "Random" "icons/random16.png"    []
       _ <- toolMenu tbar tbNext     "Next" "icons/right16.png"       []
       _ <- toolMenu tbar tbLast     "Last" "icons/end_right16.png"   []

       titleContainer <- staticText p []
       dateContainer <- staticText p []
       altContainer <- textCtrl p [ enabled := False, wrap := WrapNone ]

       let swSize = Size 750 480
       sw <- scrolledWindow p [ bgcolor := white, scrollRate := sz 10 10, virtualSize := swSize, fullRepaintOnResize := False ]
       vbitmap <- variable [ value := Nothing ]

       set f [ layout := container p $ margin 10 $ grid 1 4 [ [ hfill (widget dateContainer) ],
                                                              [ hfill (widget titleContainer) ],
                                                              [ fill (widget altContainer) ],
                                                              [ fill $ minsize swSize $ widget sw ] ]
               , clientSize := sz 800 640
               , on closing :~ \previous -> do { closeImage vbitmap; previous } ]

       set sw [ on paint := onPaint vbitmap ]

       appNetwork tbFirst tbPrevious tbRandom tbNext tbLast f sw vbitmap titleContainer dateContainer altContainer
  where
    onPaint :: VBitmap -> DC () -> Rect -> IO ()
    onPaint vbitmap dc _ -- viewArea
         = do _ <- logNullCreate -- to prevent iCCP warning dialog
              mbBitmap <- get vbitmap value
              case mbBitmap of
                Nothing -> return ()
                Just bm -> drawBitmap dc bm pointZero False []
