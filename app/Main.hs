{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module Main where

import           Data.ByteString.Lazy.UTF8 as BSU (fromString)
import           Data.Digest.Pure.SHA      (sha256, showDigest)
import           Data.Time                 (ZonedTime, defaultTimeLocale,
                                            formatTime, getZonedTime)
import           System.Directory          (getCurrentDirectory)
-- import           System.Exit
import           System.Console.CmdArgs    (Data, Default (def), Typeable,
                                            cmdArgs, help, summary, (&=))
import           System.Process            (readProcessWithExitCode)
import           Text.Printf               (printf)

shaDigestLength = 8
gitSHAsize = 8

type Mill = Bool

main :: IO ()
main = do
    Fn {..} <- cmdArgs fn
    currentDirectory <- getCurrentDirectory
    putStrLn $ "Current directory: " ++ currentDirectory
    gitSHADigest <- getGitSHADigest
    now <- getZonedTime
    let dataTime = getTime now milli in
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

data Fn = Fn {milli :: Bool} deriving (Show, Data, Typeable)

fn = Fn
    {milli = def &= help "Use milliseconds"
    } &=
    help "`fn` is a tool for generating, and parsing, file names based on\ncurrent date, time, process id and gitsha." &=
    summary "Fn v0.1.0.0, (C) Denis Evsyukov"
