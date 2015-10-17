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
import Graphics.UI.WX hiding (Event, newEvent)
import Graphics.UI.WXCore hiding (Event)
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

--data MenuItems = MenuItems {
--    frs :: MenuItem(),
--    prv :: MenuItem(),
--    rnd :: MenuItem(),
--    nxt :: MenuItem(),
--    lst :: MenuItem()
--}

runAll :: HXkcdApp a -> AppEnv -> AppState -> IO (a, AppState)
runAll k env = S.runStateT (runReaderT (runHXkcd k) env)

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
       toolMenu tbar tbFirst    "First" "icons/start_left16.png" []
       toolMenu tbar tbPrevious "Previous" "icons/left16.png"    []
       toolMenu tbar tbRandom   "Random" "icons/random16.png"    []
       toolMenu tbar tbNext     "Next" "icons/right16.png"       []
       toolMenu tbar tbLast     "Last" "icons/end_right16.png"   []

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
--       let menuItems = MenuItems { frs = first, prv = previous, rnd = random, nxt = next, lst = last }

       lastId <- updateAsLast


--       runAll (start components menuItems) env state
--  where
--    start :: Components -> MenuItems -> HXkcdApp ()
--    start components m = do
--        env <- ask
--        state <- S.get
--        liftIO $ createDirectoryIfMissing False $ baseDir env
--        lastId <- liftIO updateAsLast
--        liftIO $ runReactiveNetwork components m AppState { index = lastId, lastIndex = lastId }
--
--    runReactiveNetwork :: Components -> MenuItems -> AppState -> IO ()
--    runReactiveNetwork components m initialState = do

       let networkDescription :: forall t. Frameworks t => Moment t ()
           networkDescription = do

               firstButton    <- event0 f (menu tbFirst)
               previousButton <- event0 f (menu tbPrevious)
               randomButton   <- event0 f (menu tbRandom)
               nextButton     <- event0 f (menu tbNext)
               lastButton     <- event0 f (menu tbLast)

               fetchLastIndex <- fromPoll updateAsLast -- review this - expensive?
               fetchRandomIndex <- fromPoll updateAsRandom -- review this - expensive?

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
                   menuSelection = accumE AppState { index = lastId, lastIndex = lastId } $
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

               fetchFeed2E <- mapIO' fetchFeed menuSelection
               let fetchFeed2B = stepper Nothing $ fetchFeed2E

               fetchImage2E <- mapIO' (fetchImage components) fetchFeed2E

               sink dateContainer  [ text :== show <$> (maybe "error" getDate)  <$> fetchFeed2B ]
               sink titleContainer [ text :== show <$> (maybe "error" getTitle) <$> fetchFeed2B ]
               sink altContainer   [ text :== show <$> (maybe "error" getAlt)   <$> fetchFeed2B ]

               reactimate $ (displayImage sw vbitmap) <$> fromJust <$> fetchImage2E

       network <- compile networkDescription
       actuate network

       return ()
  where
    getRandomNum :: Int -> IO Int
    getRandomNum upperLimit = getStdRandom (randomR (1, upperLimit))

    -- bad - should not reuse the lastIndex call but the current AppState
    updateAsRandom :: IO Int
    updateAsRandom = updateAsLast >>= \lastIndex -> getStdRandom (randomR (1, lastIndex))

    updateAsLast :: IO Int
    updateAsLast
        = do r <- downloadFeed $ getUrl 0
             let f = fromJust r
             let num = getNum f
--             getFeedPath num >>= \path -> saveFeed path f
             saveFeed (getFeedPath num) f
             return num

    fetchFeed :: AppState -> IO (Maybe Xkcd)
    fetchFeed state
         = do let id = index state
              putStrLn $ "Executed fetch with id " ++ show id

--              fileName <- getFeedPath id
              let fileName = getFeedPath id
              putStrLn $ "Get Feed file " ++ fileName

              exists <- doesFileExist fileName
              if exists then loadFeed fileName
                        else downloadFeed (getUrl id) >>= \f -> saveFeed fileName $ fromJust f

--     getFeedPath :: Int -> HXkcdApp String
--     getFeedPath id = do env <- ask
--                         return $ baseDir env ++ show id ++ ".metadata.json"

    getFeedPath :: Int -> String
    getFeedPath id = "/home/daniele/.hxkcd/" ++ show id ++ ".metadata.json"

    fetchImage :: Components -> Maybe Xkcd -> IO (Maybe (Bitmap ()))
    fetchImage components feed
         = do let f = fromJust feed
              let uri = getUri f
--              fileName <- getImagePath (getNum f) uri
              let fileName = getImagePath (getNum f) uri

              putStrLn ("Imnage saved here: " ++ fileName)

              exists <- doesFileExist fileName
              unless exists $ downloadImage uri >>= \i -> void (saveImage fileName $ fromJust i)
              loadImage fileName -- >>= \i -> displayImage (sw components) (vbitmap components) (fromJust i)
--               displayCachedContent components fileName uri
--               displayMetadata components f

--     getImagePath :: Int -> String -> HXkcdApp String
--     getImagePath id uri = do env <- ask
--                              return $ baseDir env ++ show id ++ "-" ++ getFinalUrlPart uri

    getImagePath :: Int -> String -> String
    getImagePath id uri = "/home/daniele/.hxkcd/" ++ show id ++ "-" ++ getFinalUrlPart uri

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
