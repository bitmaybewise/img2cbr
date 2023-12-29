module Main where

import Options.Applicative

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

main :: IO ()
main = do
  options <- execParser opts
  print options
