module Main where

import Data.Time

main :: IO ()
main = do
    now <- getZonedTime
    print $ getTime now

getTime :: ZonedTime -> String
getTime now = do
    formatTime defaultTimeLocale "%Y%m%d-%H%M%S" now
