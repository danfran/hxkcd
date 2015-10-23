module AppState where

data AppState = AppState {
    lastIndex :: Int,
    index :: Int
} deriving (Show)


initialState :: AppState
initialState = AppState { lastIndex = 0, index = 0 }

doFirst :: AppState -> AppState
doFirst state = state { index = 1, lastIndex = lastIndex state }

doPrevious :: AppState -> AppState
doPrevious state = if index state > 1 then (state { index = index state - 1, lastIndex = lastIndex state })
                   else state

doRandom :: Int -> AppState -> AppState
doRandom randomIndex state = state { index = randomIndex, lastIndex = lastIndex state }

doNext :: AppState -> AppState
doNext state = if index state < lastIndex state then (state { index = index state + 1, lastIndex = lastIndex state })
               else state

doLast :: Int -> AppState -> AppState
doLast newIndex state = state { index = newIndex, lastIndex = newIndex }
