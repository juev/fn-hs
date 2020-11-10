{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module Main where

import           Data.ByteString.Lazy.UTF8 as BSU (fromString)
import           Data.Digest.Pure.SHA      (sha256, showDigest)
import           Data.Time                 (ZonedTime, defaultTimeLocale,
                                            formatTime, getZonedTime)
import           System.Process            (readProcessWithExitCode)
import           Text.Printf               (printf)
import           Options.Applicative
import           Data.Semigroup ((<>))

shaDigestLength :: Int
shaDigestLength = 8

gitSHAsize :: Int
gitSHAsize = 8

type Mill = Bool

data Options = Options
  { timestamp  :: Bool
  , milli      :: Bool
  , prochash   :: Bool
  , gitsha     :: Bool }

options :: Parser Options
options = Options
      <$> switch
          ( short 't'
         <> help "return timestamp only." )
      <*> switch
          ( short 'm'
         <> help "include milliseconds." )
      <*> switch
          ( short 'p'
         <> help "return a prochash." )
      <*> switch
          ( short 'g'
         <> help "return current git sha." )

main :: IO ()
main = parse =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
        <> header "fn-hs - a tool for generating, and parsing file names" )

parse :: Options -> IO ()
parse (Options True milli _ _) = do
  now <- getZonedTime
  let dataTime = getTime now milli in
    printf "%s" dataTime
parse (Options _ _ True _) = do
  gitSHADigest <- getGitSHADigest
  printf "%s" gitSHADigest
parse (Options _ _ _ True) = do
  now <- getZonedTime
  let dataTime = getTime now False in
    printf "%s" $ shaDigest dataTime
parse (Options _ _ _ _) = do
  now <- getZonedTime
  gitSHADigest <- getGitSHADigest
  let dataTime = getTime now False in
    printf "%s-%s-%s" dataTime gitSHADigest . shaDigest $ dataTime

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
