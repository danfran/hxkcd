{-# LANGUAGE ScopedTypeVariables #-}
module AppNetwork where

import Control.Monad
import qualified Data.Foldable as F (forM_)
import Data.Maybe (fromJust)
import Graphics.UI.WX hiding (Event, newEvent)
import Graphics.UI.WXCore hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX
import System.Directory
import System.Random

import Feed
import AppState
import FeedIO
import ImageIO

type VBitmap = Var (Maybe (WxObject (CGDIObject (CBitmap ()))))

data Navigation = First | Previous | Random | Next | Last deriving (Eq)

appNetwork :: MenuItem() -> MenuItem() -> MenuItem() -> MenuItem() -> MenuItem()
                      -> Frame() -> ScrolledWindow() -> VBitmap -> StaticText() -> StaticText() -> TextCtrl() -> IO ()
appNetwork tbFirst tbPrevious tbRandom tbNext tbLast f sw vbitmap titleContainer dateContainer altContainer = do
  hd <- getHomeDirectory
  let baseDir = hd ++ "/.hxkcd/"
  lastId <- getNum . fromJust <$> downloadFeed baseDir 0
  let initialState = lastId

  let networkDescription :: forall t. Frameworks t => AddHandler Navigation -> Moment t ()
      networkDescription initHandler = do

          firstButton    <- event0 f (menu tbFirst)
          previousButton <- event0 f (menu tbPrevious)
          randomButton   <- event0 f (menu tbRandom)
          nextButton     <- event0 f (menu tbNext)
          lastButton     <- event0 f (menu tbLast)

          stateInitializer <- fromAddHandler initHandler

          fetchRandomIndex <- fromPoll $ getStdRandom (randomR (1, lastId))

          let menuSelection :: Event t Int
              menuSelection = accumE initialState $
                                  unions [
                                      doFirst <$ firstButton
                                      , doPrevious <$ previousButton
                                      , doRandom <$> fetchRandomIndex <@ randomButton
                                      , doNext lastId <$ nextButton
                                      , doLast lastId <$ lastButton
                                      , doLast lastId <$ stateInitializer
                                  ]

              mapIO' :: (a -> IO b) -> Event t a -> Moment t (Event t b)
              mapIO' ioFunc e1
                  = do (e2, handler) <- newEvent
                       reactimate $ (ioFunc >=> handler) <$> e1
                       return e2

          fetchFeed2E <- mapIO' (loadFeed baseDir) menuSelection
          fetchImage2E <- mapIO' (loadImage baseDir . fromJust) fetchFeed2E

          reactimate $ displayMetadata <$> fromJust <$> fetchFeed2E
          reactimate $ displayImage <$> fromJust <$> fetchImage2E

  (addInitHandler, fireInit) <- newAddHandler
  network <- compile $ networkDescription addInitHandler
  actuate network
  fireInit Last
  where
    displayMetadata :: Feed -> IO ()
    displayMetadata loadedFeed
         = do set titleContainer [ text := getTitle loadedFeed ]
              set dateContainer  [ text := getDate loadedFeed ]
              set altContainer   [ text := getAlt loadedFeed ]

    displayImage :: Bitmap () -> IO ()
    displayImage bm
         = do closeImage vbitmap
              set vbitmap [ value := Just bm ]
              -- resize
              bmsize <- get bm size
              set sw [ virtualSize := bmsize ]
              repaint sw

closeImage :: VBitmap -> IO ()
closeImage vbitmap
     = do mbBitmap <- swap vbitmap value Nothing
          F.forM_ mbBitmap objectDelete
