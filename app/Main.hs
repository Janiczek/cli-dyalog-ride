module Main where

import Control.Applicative ((<**>))
import Data.Semigroup ((<>))
import Lib (Flags (Flags), run)
import Options.Applicative
  ( Parser,
    auto,
    execParser,
    footer,
    fullDesc,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    progDesc,
    short,
    showDefault,
    str,
    strOption,
    switch,
    value,
  )

parser :: Parser Flags
parser =
  Flags
    <$> option
      auto
      ( long "port"
          <> short 'p'
          <> metavar "APLINE_PORT"
          <> help "Port to run the RIDE server for communication with Dyalog on"
          <> showDefault
          <> value 8000
      )
    <*> option
      str
      ( long "dyalog-path"
          <> metavar "APLINE_DYALOG_PATH"
          <> help "Path to the Dyalog interpreter binary"
          <> showDefault
          <> value "dyalog"
      )
    <*> switch
      ( long "verbose"
          <> short 'v'
          <> help "Should log RIDE communication?"
      )

main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (parser <**> helper)
        ( fullDesc
            <> progDesc "A backtick-friendly CLI Dyalog APL client"
            <> footer "Note: this program expects to be given the actual `dyalog` binary, not the `mapl` wrapper. If you use `mapl`, this program will work but the Dyalog binary won't for some reason be stopped when this program stops."
        )
