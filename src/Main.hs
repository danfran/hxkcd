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
import System.Random

import Feed
import Xkcd
import Image


data Navigation = First | Previous | Random | Next | Last deriving (Eq)

type VBitmap = Var (Maybe (WxObject (CGDIObject (CBitmap ()))))

data AppState = AppState {
    lastIndex :: Int,
    index :: Int
} deriving (Show)


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

       lastId <- updateAsLast baseDir
       let initialState = AppState { lastIndex = lastId, index = lastId }

       -- start frp

       let networkDescription :: forall t. Frameworks t => Moment t ()
           networkDescription = do

               firstButton    <- event0 f (menu tbFirst)
               previousButton <- event0 f (menu tbPrevious)
               randomButton   <- event0 f (menu tbRandom)
               nextButton     <- event0 f (menu tbNext)
               lastButton     <- event0 f (menu tbLast)

               fetchLastIndex <- fromPoll $ updateAsLast baseDir -- review this - expensive?
               fetchRandomIndex <- fromPoll $ updateAsRandom baseDir -- review this - expensive?

               let doFirst :: AppState -> AppState
                   doFirst state = state { index = 1, lastIndex = lastIndex state }

                   doPrevious :: AppState -> AppState
                   doPrevious state = if index state > 1 then (state { index = index state - 1, lastIndex = lastIndex state }) else state

                   doRandom :: Int -> AppState -> AppState
                   doRandom randomIndex state = state { index = randomIndex, lastIndex = lastIndex state }

                   doNext :: AppState -> AppState
                   doNext state = if index state < lastIndex state then (state { index = index state + 1, lastIndex = lastIndex state }) else state

                   doLast :: Int -> AppState -> AppState
                   doLast newIndex state = state { index = newIndex, lastIndex = newIndex }


                   menuSelection :: Event t AppState
                   menuSelection = accumE initialState $
                                       unions [
                                           doFirst <$ firstButton
                                           , doPrevious <$ previousButton
                                           , doRandom <$> fetchRandomIndex <@ randomButton
                                           , doNext <$ nextButton
                                           , doLast <$> fetchLastIndex <@ lastButton
                                       ]

                   mapIO' :: (a -> IO b) -> Event t a -> Moment t (Event t b)
                   mapIO' ioFunc e1
                       = do (e2, handler) <- newEvent
                            reactimate $ (ioFunc >=> handler) <$> e1
                            return e2

               fetchFeed2E <- mapIO' (fetchFeed baseDir) menuSelection
               let fetchFeed2B = stepper Nothing fetchFeed2E

               fetchImage2E <- mapIO' (fetchImage baseDir) fetchFeed2E

               sink dateContainer  [ text :== maybe "error" getDate  <$> fetchFeed2B ]
               sink titleContainer [ text :== maybe "error" getTitle <$> fetchFeed2B ]
               sink altContainer   [ text :== maybe "error" getAlt   <$> fetchFeed2B ]

               reactimate $ displayImage sw vbitmap <$> fromJust <$> fetchImage2E

       network <- compile networkDescription
       actuate network

       return ()
  where
    -- bad - should not reuse the lastIndex call but the current AppState
    updateAsRandom :: String -> IO Int
    updateAsRandom baseDir = updateAsLast baseDir >>= \lastIndex -> getStdRandom (randomR (1, lastIndex))

    updateAsLast :: String -> IO Int
    updateAsLast baseDir
        = do r <- downloadFeed $ getUrl 0
             let f = fromJust r
             let num = getNum f
             _ <- saveFeed (getFeedPath baseDir num) f
             return num

    fetchFeed :: String -> AppState -> IO (Maybe Xkcd)
    fetchFeed baseDir state
         = do let currentId = index state
              let fileName = getFeedPath baseDir currentId
              putStrLn $ "Executed fetch with id " ++ show currentId
              putStrLn $ "Get Feed file " ++ fileName
              exists <- doesFileExist fileName
              if exists then loadFeed fileName
                        else downloadFeed (getUrl currentId) >>= \f -> saveFeed fileName $ fromJust f

    getFeedPath :: String -> Int -> String
    getFeedPath baseDir currentId = baseDir ++ show currentId ++ ".metadata.json"

    fetchImage :: String -> Maybe Xkcd -> IO (Maybe (Bitmap ()))
    fetchImage baseDir xkcdFeed
         = do let f = fromJust xkcdFeed
              let uri = getUri f
              let fileName = getImagePath baseDir (getNum f) uri
              putStrLn ("Image saved here: " ++ fileName)
              exists <- doesFileExist fileName
              unless exists $ downloadImage uri >>= \i -> void (saveImage fileName $ fromJust i)
              loadImage fileName

    getImagePath :: String -> Int -> String -> String
    getImagePath baseDir currentId uri = baseDir ++ show currentId ++ "-" ++ getFinalUrlPart uri

    onPaint vbitmap dc _ -- viewArea
         = do _ <- logNullCreate -- to prevent iCCP warning dialog
              mbBitmap <- get vbitmap value
              case mbBitmap of
                Nothing -> return ()
                Just bm -> drawBitmap dc bm pointZero False []

    closeImage vbitmap
         = do mbBitmap <- swap vbitmap value Nothing
              F.forM_ mbBitmap objectDelete

    displayImage sw vbitmap bm
         = do closeImage vbitmap
              set vbitmap [ value := Just bm ]
              -- resize
              bmsize <- get bm size
              set sw [ virtualSize := bmsize ]
              repaint sw
