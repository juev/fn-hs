{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Time                 (ZonedTime, defaultTimeLocale,
                                            formatTime, getZonedTime)
-- import System.Process (system)
import           Data.ByteString.Lazy.UTF8 as BSU (fromString)
import           Data.Digest.Pure.SHA      (sha256, showDigest)
import           System.Directory          (getCurrentDirectory)
import           Text.Printf               (printf)
shaDigestLength = 8

type Mill = Bool

main :: IO ()
main = do
    currentDirectory <- getCurrentDirectory
    putStrLn $ "Current directory: " ++ currentDirectory
    now <- getZonedTime
    let dataTime = getTime now False in
        printf "Result: %s--%s" dataTime . shaDigest $ dataTime

getTime :: ZonedTime -> Mill -> String
getTime now True  = formatTime defaultTimeLocale "%Y%m%d-%H%M%S_%-q" now
getTime now False = formatTime defaultTimeLocale "%Y%m%d-%H%M%S" now

shaDigest :: String -> String
shaDigest = take shaDigestLength . showDigest . sha256 . BSU.fromString
