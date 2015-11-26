module Main where

import Lib
import System.Console.ArgParser
import Control.Applicative

{-
Data type to hold the command line optio
-}
data MyConfig = MyConfig String Bool
  deriving Show

parser :: ParserSpec MyConfig
parser = MyConfig
  `parsedBy` reqPos "input" `Descr` "Input MpegTs file"
  `andBy` boolFlag "verbose" `Descr` "Verbose output"

commandLineInterface :: IO (CmdLnInterface MyConfig)
commandLineInterface = 
  (`setAppDescr` "Parser for printing out information about an MpegTS file")
  <$> (`setAppEpilog` "Kevin S Kirkup")
  <$> mkApp parser


main :: IO ()
main = do
  interface <- commandLineInterface
  runApp interface print
  someFunc

