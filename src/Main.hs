{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import qualified Control.Monad.State as S
import Control.Monad
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as B
import qualified Data.Foldable as F (forM_)
import Data.Maybe (fromJust)
import Graphics.UI.WX hiding (when, Event)
import Graphics.UI.WXCore hiding (when, Event)
import Network.HTTP
import Reactive.Banana
import Reactive.Banana.WX
import System.Directory
import System.Random

import Feed
import Xkcd
import Image


data Navigation = First | Previous | Random | Next | Last deriving (Eq)

type VBitmap = Var (Maybe (WxObject (CGDIObject (CBitmap ()))))

data AppEnv = AppEnv {
    baseDir :: FilePath
}

data AppState = AppState {
    lastIndex :: Int,
    index :: Int
} deriving (Show)

type HXkcd = ReaderT AppEnv (S.StateT AppState IO)

newtype HXkcdApp a = HXkcdApp {
    runHXkcd :: HXkcd a
} deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv, S.MonadState AppState)

data Components = Components {
    f :: Frame(),
    titleContainer :: StaticText(),
    dateContainer :: StaticText(),
    altContainer :: TextCtrl(),
    sw :: ScrolledWindow(),
    vbitmap :: VBitmap
}

data MenuItems = MenuItems {
    frs :: MenuItem(),
    prv :: MenuItem(),
    rnd :: MenuItem(),
    nxt :: MenuItem(),
    lst :: MenuItem()
}

runAll :: HXkcdApp a -> AppEnv -> AppState -> IO (a, AppState)
runAll k env = S.runStateT (runReaderT (runHXkcd k) env)

main :: IO ()
main = start hxkcd

hxkcd :: IO ()
hxkcd
  = do f <- frame [ text := "HXKCD" ]

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
       vbitmap <- variable [ value := Nothing ]

       set f [ layout := container p $ margin 10 $ grid 1 4 [ [ hfill (widget dateContainer) ],
                                                              [ hfill (widget titleContainer) ],
                                                              [ fill (widget altContainer) ],
                                                              [ fill $ minsize swSize $ widget sw ] ]
               , clientSize := sz 800 640
               , on closing :~ \previous -> do { closeImage vbitmap; previous } ]

       set sw [ on paint := onPaint vbitmap ]

       -- start

       hd <- getHomeDirectory
       let env = AppEnv $ hd ++ "/.hxkcd/"
       let state = AppState { lastIndex = 0, index = 0 }
       let components = Components { f, titleContainer, dateContainer, altContainer, sw, vbitmap }
       let menuItems = MenuItems { frs = first, prv = previous, rnd = random, nxt = next, lst = last }

       runAll (start components menuItems) env state
       return ()
  where
    start :: Components -> MenuItems -> HXkcdApp ()
    start components m = do
        env <- ask
        state <- S.get
        liftIO $ createDirectoryIfMissing False $ baseDir env
        lastId <- liftIO updateAsLast
        liftIO $ runReactiveNetwork components m AppState { index = lastId, lastIndex = lastId }

    runReactiveNetwork :: Components -> MenuItems -> AppState -> IO ()
    runReactiveNetwork components m initialState = do

        let networkDescription :: forall t. Frameworks t => Moment t ()
            networkDescription = do

                firstButton    <- event0 (menu $ frs m) command
                previousButton <- event0 (menu $ prv m) command
                randomButton   <- event0 (menu $ rnd m) command
                nextButton     <- event0 (menu $ nxt m) command
                lastButton     <- event0 (menu $ lst m) command

                fetchLastIndex <- fromPoll updateAsLast

                let
                    doFirst :: AppState -> AppState
                    doFirst state = state { index = 1, lastIndex = lastIndex state }

                    doPrevious :: AppState -> AppState
                    doPrevious state = if index state > 1 then (state { index = index state - 1, lastIndex = lastIndex state }) else state

                    doRandom :: AppState -> AppState
                    doRandom state = state { index = getRandomNum $ lastIndex state , lastIndex = lastIndex state }

                    doNext :: AppState -> AppState
                    doNext state = if index state < lastIndex state then (state { index = index state + 1, lastIndex = lastIndex state }) else state

                    doLast :: Int -> AppState -> AppState
                    doLast newIndex state = state { index = newIndex, lastIndex = newIndex }

                    menuSelection :: Behavior t AppState
                    menuSelection = accumB initialState $
                                        unions [
                                            doFirst <$ firstButton
                                            , doPrevious <$ previousButton
                                            , doRandom <$ randomButton
                                            , doNext <$ nextButton
                                            , doLast <$> fetchLastIndex <@ lastButton
                                        ]

                    mapIO' :: (a -> IO b) -> Event t a -> Moment t (Event t b)
                    mapIO' ioFunc e1 = do
                        (e2, handler) <- newEvent
--                         reactimate $ (\state -> ioFunc state >>= handler) <$> e1
                        reactimate $ (ioFunc >=> handler) <$> e1
                        return e2

                fetchFeed2E <- mapIO' fetchFeed appStateE
                let fetchFeed2B = stepper Nothing $ Just <$> fetchFeed2E

                fetchImage2E <- mapIO' fetchImage fetchFeed2E
                let fetchImage2B = stepper Nothing $ Just <$> fetchImage2E

                sink (titleContainer components) [ text :== show <$> fetchFeed2B ]
                sink (dateContainer components)  [ text :== show <$> fetchFeed2B ]
                sink (altContainer components)   [ text :== show <$> fetchFeed2B ]

        network <- compile networkDescription
        actuate network

    getRandomNum :: Int -> Int
    getRandomNum upperLimit = getStdRandom (randomR (1, upperLimit))

    updateAsLast :: IO Int
    updateAsLast
        = do r <- downloadFeed $ getUrl 0
             let f = fromJust r
             let num = getNum f
             getFeedPath num >>= \path -> saveFeed path f
             return num

    fetchFeed :: AppState -> IO (Maybe Xkcd)
    fetchFeed state
         = do let id = index state
              fileName <- getFeedPath id
              exists <- doesFileExist fileName
              if exists then loadFeed fileName
                        else downloadFeed (getUrl id) >>= \f -> saveFeed fileName $ fromJust f

--     getFeedPath :: Int -> HXkcdApp String
--     getFeedPath id = do env <- ask
--                         return $ baseDir env ++ show id ++ ".metadata.json"

    getFeedPath :: Int -> String
    getFeedPath id = ".hxkcd/" ++ show id ++ ".metadata.json"

    displayContent :: Components -> Maybe Xkcd -> IO (Maybe (Bitmap ()))
    displayContent components feed
         = do let f = fromJust feed
              let uri = getUri f
              fileName <- getImagePath (getNum f) uri
              exists <- doesFileExist fileName
              unless exists $ downloadImage uri >>= \i -> void (saveImage fileName $ fromJust i)
              loadImage fileName -- >>= \i -> displayImage (sw components) (vbitmap components) (fromJust i)
--               displayCachedContent components fileName uri
--               displayMetadata components f

--     getImagePath :: Int -> String -> HXkcdApp String
--     getImagePath id uri = do env <- ask
--                              return $ baseDir env ++ show id ++ "-" ++ getFinalUrlPart uri

    getImagePath :: Int -> String -> IO String
    getImagePath id uri = ".hxkcd/" ++ show id ++ "-" ++ getFinalUrlPart uri

    onPaint vbitmap dc viewArea
         = do logNullCreate -- to prevent iCCP warning dialog
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
