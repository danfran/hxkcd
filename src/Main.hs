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

                sink (titleContainer components) [ text :== show <$> reactimate $ fetchFeed <@> menuSelection  ]
                sink (dateContainer components)  [ text :== getDate $ fetchFeed <$> menuSelection ]
                sink (altContainer components)   [ text :== getAlt $ fetchFeed <$> menuSelection ]

--                 reactimate $ displayContent components (index <$> menuSelection)

        network <- compile networkDescription
        actuate network

--     getImage :: Components -> Navigation -> IO ()
--     getImage components navigation
--          = do cursor@AppState { index, lastIndex } <- S.get
--
--               liftIO $ print $ show cursor
--
--               feed <- if navigation == Last then
--                         do r <- updateAsLast
--                            displayContent components r
--                            let id = getNum $ fromJust r
--                            S.put $ cursor { index = id, lastIndex = id }
--                            return r
--                       else
--                         do case navigation of
--                              First -> S.put cursor { index = 1, lastIndex = lastIndex }
--                              Previous -> S.put $ if index > 1 then cursor { index = index - 1, lastIndex = lastIndex } else cursor
--                              Random -> getRandomNum lastIndex >>= \rn -> S.put $ cursor { index = rn, lastIndex = lastIndex }
--                              Next -> S.put $ if index < lastIndex then cursor { index = index + 1, lastIndex = lastIndex } else cursor
--                            fetchFeed index
--
--               displayContent components feed
--
--               S.get >>= \s -> liftIO $ print $ show s
--
--               return ()

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

    displayContent :: Components -> Int -> IO ()
    displayContent components feedId
         = do feed <- fetchFeed feedId
              let f = fromJust feed
              let uri = getUri f
              fileName <- getImagePath (getNum f) uri
              displayCachedContent components fileName uri
--               displayMetadata components f

    displayCachedContent :: Components -> String -> String -> IO ()
    displayCachedContent components fileName uri
         = do exists <- doesFileExist fileName
              unless exists $ downloadImage uri >>= \i -> void (saveImage fileName $ fromJust i)
              loadImage fileName >>= \i -> displayImage (sw components) (vbitmap components) (fromJust i)

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

--     displayMetadata components feed
--          = do set (titleContainer components) [ text := getTitle feed ]
--               set (dateContainer components)  [ text := getDate feed ]
--               set (altContainer components)   [ text := getAlt feed ]

    displayImage sw vbitmap bm
         = do closeImage vbitmap
              set vbitmap [ value := Just bm ]
              -- resize
              bmsize <- get bm size
              set sw [ virtualSize := bmsize ]
              repaint sw
