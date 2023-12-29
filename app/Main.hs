{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Monad (forM_, void, when)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Options.Applicative
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Process (readProcess)

data Opts = Opts
  { origin :: String,
    destination :: String,
    depth :: Int,
    verbose :: Bool
  }
  deriving (Show)

optsParser :: Parser Opts
optsParser =
  Opts
    <$> strOption (long "origin" <> short 'i' <> help "directory of origin")
    <*> strOption (long "destination" <> short 'o' <> help "directory of destination")
    <*> option auto (long "depth" <> short 'd' <> help "directory depth" <> value 1)
    <*> switch (long "verbose" <> short 'v' <> help "verbose output")

opts :: ParserInfo Opts
opts = info (optsParser <**> helper) (header "img2cbr - converts a folder containing images to a cbr file")

findDirectories :: Opts -> IO [String]
findDirectories options = do
  output <- readProcess "find" [origin options, "-type", "d", "-mindepth", show $ depth options, "-maxdepth", show $ depth options] []
  pure $ lines output

img2cbr :: String -> Opts -> IO ()
img2cbr dir options = do
  let cbr = T.replace (T.pack $ origin options) (T.pack $ destination options) (T.pack $ dir ++ ".cbr")
  exists <- doesFileExist $ T.unpack cbr
  if exists
    then when (verbose options) $ do
      TIO.putStrLn $ "File already exists, skipping -- " <> cbr
    else do
      createDirectoryIfMissing True (destination options)
      when (verbose options) $ do
        TIO.putStrLn $ "packaging -- " <> cbr
      void $ readProcess "zip" ["-r", T.unpack cbr, dir] []

main :: IO ()
main = do
  options <- execParser opts
  dirs <- findDirectories options
  forM_ dirs (`img2cbr` options)
