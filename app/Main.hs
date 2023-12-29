module Main where

import Options.Applicative
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
    <$> strOption
      ( long "origin"
          <> short 'i'
          <> help "directory of origin"
      )
    <*> strOption
      ( long "destination"
          <> short 'o'
          <> help "directory of destination"
      )
    <*> option
      auto
      ( long "depth"
          <> short 'd'
          <> help "directory depth"
          <> value 1
      )
    <*> switch
      ( long "verbose"
          <> short 'v'
          <> help "verbose output"
      )

opts :: ParserInfo Opts
opts =
  info
    (optsParser <**> helper)
    (header "img2cbr - converts a folder containing images to a cbr file")

findDirectories :: Opts -> IO [String]
findDirectories options = do
  output <- readProcess "find" [origin options, "-type", "d", "-mindepth", show $ depth options, "-maxdepth", show $ depth options] []
  pure $ lines output

main :: IO ()
main = do
  options <- execParser opts
  dirs <- findDirectories options
  print dirs
