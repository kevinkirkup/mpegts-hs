module Main where

import Lib
import Options.Applicative

{-
Data type to hold the command line optio
-}
data Options = Options { input :: String
                       , verbose :: Bool
                       } deriving Show

{-
Option Parser
-}
parseOptions :: Parser Options
parseOptions = Options
  <$> strArgument 
      ( metavar "INPUT"
     <> help "The input file" )
  <*> switch
      ( long "verbose"
     <> short 'v'
     <> help "Enable verbose mode" )

-- Actual program logic
run :: Options -> IO ()
run opts = do
  putStrLn (input opts)

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> parseOptions)
      ( fullDesc
     <> progDesc "Parse the specified MpegTS file"
     <> header "mpegts-hs - a parser for MpegTS streams" )
