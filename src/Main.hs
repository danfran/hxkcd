{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import qualified Data.Foldable as F (forM_)
import Data.Maybe (fromJust)
import Graphics.UI.WX hiding (Event, newEvent)
import Graphics.UI.WXCore hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX
import System.Directory

import Feed
import FeedIO
import ImageIO
import AppState

data Navigation = First | Previous | Random | Next | Last deriving (Eq)

type VBitmap = Var (Maybe (WxObject (CGDIObject (CBitmap ()))))

main :: IO ()
main = start hxkcd

hxkcd :: IO ()
hxkcd
  = do -- start to build the window
       f <- frame [ text := "HXKCD" ]

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

       -- init

       hd <- getHomeDirectory
       let baseDir = hd ++ "/.hxkcd/"

       -- start frp

       let networkDescription :: forall t. Frameworks t => AddHandler Navigation -> Moment t ()
           networkDescription initHandler = do

               stateInitializer <- fromAddHandler initHandler

               firstButton    <- event0 f (menu tbFirst)
               previousButton <- event0 f (menu tbPrevious)
               randomButton   <- event0 f (menu tbRandom)
               nextButton     <- event0 f (menu tbNext)
               lastButton     <- event0 f (menu tbLast)

               fetchLastIndex <- fromPoll $ updateAsLast baseDir -- review this - expensive?
               fetchRandomIndex <- fromPoll $ updateAsRandom baseDir -- review this - expensive?

               let menuSelection :: Event t AppState
                   menuSelection = accumE initialState $
                                       unions [
                                           doFirst <$ firstButton
                                           , doPrevious <$ previousButton
                                           , doRandom <$> fetchRandomIndex <@ randomButton
                                           , doNext <$ nextButton
                                           , doLast <$> fetchLastIndex <@ lastButton
                                           , doLast <$> fetchLastIndex <@ stateInitializer
                                       ]

                   mapIO' :: (a -> IO b) -> Event t a -> Moment t (Event t b)
                   mapIO' ioFunc e1
                       = do (e2, handler) <- newEvent
                            reactimate $ (ioFunc >=> handler) <$> e1
                            return e2

               fetchFeed2E <- mapIO' (fetchFeed baseDir . index) menuSelection
               let fetchFeed2B = stepper Nothing fetchFeed2E

               fetchImage2E <- mapIO' (fetchImage baseDir . fromJust) fetchFeed2E

               sink dateContainer  [ text :== maybe "error" getDate  <$> fetchFeed2B ]
               sink titleContainer [ text :== maybe "error" getTitle <$> fetchFeed2B ]
               sink altContainer   [ text :== maybe "error" getAlt   <$> fetchFeed2B ]

               reactimate $ displayImage sw vbitmap <$> fromJust <$> fetchImage2E

       (addInitHandler, fireInit) <- newAddHandler
       network <- compile $ networkDescription addInitHandler
       actuate network
       fireInit Last
  where
    onPaint :: VBitmap -> DC () -> Rect -> IO ()
    onPaint vbitmap dc _ -- viewArea
         = do _ <- logNullCreate -- to prevent iCCP warning dialog
              mbBitmap <- get vbitmap value
              case mbBitmap of
                Nothing -> return ()
                Just bm -> drawBitmap dc bm pointZero False []

    closeImage :: VBitmap -> IO ()
    closeImage vbitmap
         = do mbBitmap <- swap vbitmap value Nothing
              F.forM_ mbBitmap objectDelete

    displayImage :: ScrolledWindow() -> VBitmap -> Bitmap () -> IO ()
    displayImage sw vbitmap bm
         = do closeImage vbitmap
              set vbitmap [ value := Just bm ]
              -- resize
              bmsize <- get bm size
              set sw [ virtualSize := bmsize ]
              repaint sw
