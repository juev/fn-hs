module Main where

import Data.Time ( getZonedTime, formatTime, defaultTimeLocale )

main :: IO ()
main = do
    now <- getZonedTime
    print $ formatTime defaultTimeLocale "%Y%m%d-%H%M%S" now