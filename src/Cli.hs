module Cli (args, Options(..)) where

import Options.Applicative

newtype Options = Options
  { devMode :: Bool
  }

parseOptions :: Parser Options
parseOptions =
  Options
    <$> switch (long "dev" <> short 'd' <> help "Run in development mode")

opts :: ParserInfo Options
opts =
  info
    (parseOptions <**> helper)
    ( fullDesc
        <> progDesc "Run the server in development or production mode"
        <> header "A Servant-based Haskell web server"
    )

args :: IO Options
args = execParser opts
