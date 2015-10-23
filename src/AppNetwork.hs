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

          reactimate $ displayImage <$> fromJust <$> fetchImage2E

  (addInitHandler, fireInit) <- newAddHandler
  network <- compile $ networkDescription addInitHandler
  actuate network
  fireInit Last
  where
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