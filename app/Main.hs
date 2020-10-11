{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.ByteString.Lazy.UTF8 as BSU (fromString)
import           Data.Digest.Pure.SHA      (sha256, showDigest)
import           Data.Time                 (ZonedTime, defaultTimeLocale,
                                            formatTime, getZonedTime)
import           System.Directory          (getCurrentDirectory)
-- import           System.Exit
import           System.Process
import           Text.Printf               (printf)

shaDigestLength = 8
gitSHAsize = 8

type Mill = Bool

main :: IO ()
main = do
    currentDirectory <- getCurrentDirectory
    putStrLn $ "Current directory: " ++ currentDirectory
    gitSHADigest <- getGitSHADigest
    now <- getZonedTime
    let dataTime = getTime now False in
        printf "Result: %s-%s-%s" dataTime gitSHADigest . shaDigest $ dataTime

getTime :: ZonedTime -> Mill -> String
getTime now True  = formatTime defaultTimeLocale "%Y%m%d-%H%M%S_%-q" now
getTime now False = formatTime defaultTimeLocale "%Y%m%d-%H%M%S" now

shaDigest :: String -> String
shaDigest = take shaDigestLength . showDigest . sha256 . BSU.fromString

getGitSHADigest :: IO String
getGitSHADigest = do
    ( _, output, _ ) <- readProcessWithExitCode "git" ["rev-parse", "--short=8", "HEAD"] []
    return $ removeEndOfLine output

removeEndOfLine :: String -> String
removeEndOfLine = filter (/= '\n')
