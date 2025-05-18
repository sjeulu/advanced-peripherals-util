module Main where

import qualified Options.Applicative as Options
import qualified Commands.GenerateTextures as GenerateTextures

import Options.Applicative
  ( ParserInfo, info, progDesc
  , helper, option, long, short, metavar, help, value
  , hsubparser
  , auto
  )


data SubCmd
  = GenerateTextures GenerateTextures.CmdOpts
  deriving Show

data CmdOpts = MkCmdOpts
  { logLevel :: Int
  , subCmd :: SubCmd
  }
  deriving Show

parseCmd :: ParserInfo CmdOpts
parseCmd = info
  ( (helper <*>) $ MkCmdOpts
  <$> option auto
      ( long "log-level"
      <> short 'l'
      <> metavar "LOG_LEVEL"
      <> help "The log level to use"
      <> value 0
      )
  <*> hsubparser
    ( GenerateTextures.parseCmd GenerateTextures
    )
  )
  ( progDesc "idris2-cctweaked maintenance tools")


main :: IO ()
main = do
  opts <- Options.execParser parseCmd

  case subCmd opts of
    GenerateTextures opts -> GenerateTextures.run opts
