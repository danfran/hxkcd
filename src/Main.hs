{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Control.Monad.State as S
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as B
import qualified Data.Foldable as F (forM_)
import Data.IORef
import Data.Maybe (fromJust)
import Graphics.UI.WX hiding (when)
import Graphics.UI.WXCore hiding (when)
import Network.HTTP
import System.Directory
import System.Random

import Feed
import Xkcd

data Navigation = First | Previous | Random | Next | Last deriving (Eq)

type VBitmap = Var (Maybe (WxObject (CGDIObject (CBitmap ()))))

type HXkcd = ReaderT Env IO

data Env = Env {
    baseDir :: FilePath
}

data Components = Components {
    f :: Frame(),
    titleContainer :: StaticText(),
    dateContainer :: StaticText(),
    altContainer :: TextCtrl(),
    sw :: ScrolledWindow(),
    vbitmap :: VBitmap
}

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

       set f [ layout := container p $ margin 10 $ grid 1 4 [
                                                             [ hfill (widget dateContainer) ],
                                                             [ hfill (widget titleContainer) ],
                                                             [ fill (widget altContainer) ],
                                                             [ fill $ minsize swSize $ widget sw ]
                                                            ]
               , clientSize := sz 800 640
             ]

       -- set actions

       cursor <- newIORef initCursor

       vbitmap <- variable [ value := Nothing ]

       set sw [ on paint := onPaint vbitmap ]

       let components = Components { f, titleContainer, dateContainer, altContainer, sw, vbitmap }

       hd <- getHomeDirectory
       let env = Env $ hd ++ "/.hxkcd/"

       set f [ on (menu first)      := runReaderT (getImage components cursor First) env
               , on (menu previous) := runReaderT (getImage components cursor Previous) env
               , on (menu random)   := runReaderT (getImage components cursor Random) env
               , on (menu next)     := runReaderT (getImage components cursor Next) env
               , on (menu last)     := runReaderT (getImage components cursor Last) env
               , on closing :~ \previous -> do { closeImage vbitmap; previous } ]

       runReaderT (start components cursor) env
       return ()
  where
    start :: Components -> IORef FeedIndex -> HXkcd ()
    start components cursor = do
        env <- ask
        liftIO $ createDirectoryIfMissing False $ baseDir env
        getImage components cursor Last

    getImage :: Components -> IORef FeedIndex -> Navigation -> HXkcd ()
    getImage components ref navigation
          = do cursor@FeedIndex { index, lastIndex } <- liftIO $ readIORef ref

               cursor@FeedIndex { index, lastIndex }
                <- case navigation of
                     First -> return cursor { index = 1 }

                     Previous -> return $ if index > 1 then cursor { index = index - 1 } else cursor

                     Random -> getRandomNum lastIndex >>= \rn -> return $ cursor { index = rn }

                     Next -> return $ if index < lastIndex then cursor { index = index + 1 } else cursor

                     Last -> do r <- updateAsLast cursor components
                                displayContent components r
                                let id = getNum $ fromJust r
                                return $ cursor { lastIndex = id, index = id }

               liftIO $ print $ show cursor
               liftIO $ writeIORef ref cursor

               unless (navigation == Last) $ fetchFeed index components navigation >>= displayContent components
               return ()

    getRandomNum :: Int -> HXkcd Int
    getRandomNum upperLimit = liftIO $ getStdRandom (randomR (1, upperLimit))

    updateAsLast :: FeedIndex -> Components -> HXkcd (Maybe Xkcd)
    updateAsLast cursor components = do r <- liftIO $ downloadFeed $ getUrl 0
                                        let f = fromJust r
                                        getFeedPath (getNum f) >>= \path -> liftIO $ saveFeed path f
                                        return r

    fetchFeed :: Int -> Components -> Navigation -> HXkcd (Maybe Xkcd)
    fetchFeed id components navigation
          = do fileName <- getFeedPath id
               exists <- liftIO $ doesFileExist fileName
               if exists then liftIO $ loadFeed fileName
                         else liftIO $ downloadFeed (getUrl id) >>= \f -> liftIO $ saveFeed fileName $ fromJust f

    getFeedPath :: Int -> HXkcd String
    getFeedPath id = do env <- ask
                        return $ baseDir env ++ show id ++ ".metadata.json"

    displayContent :: Components -> Maybe Xkcd -> HXkcd ()
    displayContent components feed
         = do env <- ask
              let f = fromJust feed
              let uri = getUri f
              fileName <- getImagePath (getNum f) uri
              exists <- liftIO $ doesFileExist fileName
              unless exists $ liftIO $ downloadImage uri >>= \i -> void (liftIO $ saveImage fileName $ fromJust i)
              liftIO $ loadImage fileName >>= \i -> liftIO $ displayImage (sw components) (vbitmap components) (fromJust i)
              liftIO $ displayMetadata components f

    getImagePath :: Int -> String -> HXkcd String
    getImagePath id uri = do env <- ask
                             return $ baseDir env ++ show id ++ "-" ++ getFinalUrlPart uri

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
