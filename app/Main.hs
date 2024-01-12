{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Concurrent
import Control.Monad
import Data.Text qualified as T
import Options.Applicative
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Process (readProcess)
import Text.Printf (printf)

data Opts = Opts
  { origin :: String,
    destination :: String,
    depth :: Int,
    pool :: Int,
    verbose :: Bool
  }
  deriving (Show)

data WorkerDirectories = WorkerDirectories {pending :: MVar Int, total :: Int, channel :: Chan String}

optsParser :: Parser Opts
optsParser =
  Opts
    <$> strOption (long "origin" <> short 'i' <> help "directory of origin")
    <*> strOption (long "destination" <> short 'o' <> help "directory of destination")
    <*> option auto (long "depth" <> short 'd' <> help "directory depth" <> value 1)
    <*> option auto (long "pool" <> short 'p' <> help "number of parallel convertions" <> value 1)
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
      putStrLn $ "File already exists, skipping -- " <> T.unpack cbr
    else do
      createDirectoryIfMissing True (destination options)
      when (verbose options) $ do
        putStrLn $ "packaging -- " <> T.unpack cbr
      void $ readProcess "zip" ["-r", T.unpack cbr, dir] []

printProgress :: Int -> Int -> IO ()
printProgress pending total = do
  let current = total - pending
      currentProgress = current * 100 `div` total
  printf "(%d / %d) %d%s\n" current total currentProgress "%"

runWorker :: WorkerDirectories -> Opts -> MVar () -> IO ()
runWorker dirs options await = do
  totalPending <- readMVar dirs.pending
  -- checking total pending before readChan, otherwise it will block when empty
  if totalPending == 0 then takeMVar await else runWorker' totalPending
  where
    runWorker' totalPending = do
      dir <- readChan dirs.channel
      void $ swapMVar dirs.pending (totalPending - 1)
      img2cbr dir options
      printProgress (totalPending - 1) dirs.total
      runWorker dirs options await

main :: IO ()
main = do
  options <- execParser opts
  dirs <- findDirectories options
  let total = length dirs
  mDirsTotal <- newMVar total
  dirsChannel <- newChan
  writeList2Chan dirsChannel dirs
  let wDirs = WorkerDirectories {pending = mDirsTotal, total = total, channel = dirsChannel}
  awaiting <- replicateM options.pool $ do
    await <- newMVar ()
    void . forkIO $ void (runWorker wDirs options await)
    pure $ \() -> putMVar await ()
  mapM_ (\wait -> wait ()) awaiting
