module AppState where

doFirst :: Int -> Int
doFirst _ = 1

doPrevious :: Int -> Int
doPrevious currentId = if currentId > 1 then currentId - 1
                       else currentId

doRandom :: Int -> Int -> Int
doRandom randomId _ = randomId

doNext :: Int -> Int -> Int
doNext lastId currentId = if currentId < lastId then currentId + 1
                          else currentId

doLast :: Int -> Int -> Int
doLast lastId _ = lastId
