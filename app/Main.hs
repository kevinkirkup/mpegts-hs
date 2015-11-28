module Main where

import Lib
import System.Console.ArgParser
import Text.Format
import Control.Applicative
import Data.Typeable

{-
Data type to hold the command line optio
-}
data MyConfig = MyConfig {
                           input :: String,
                           verbose :: Bool
                          } deriving Show

{-
Define the command line arguments
-}
parser :: ParserSpec MyConfig
parser = MyConfig
  `parsedBy` reqPos "input" `Descr` "Input MpegTs file"
  `andBy` boolFlag "verbose" `Descr` "Verbose output"

{-
Define the Interface to gather the Configuration Parameters
-}
commandLineInterface :: IO (CmdLnInterface MyConfig)
commandLineInterface = 
  (`setAppDescr` "Parser for printing out information about an MpegTS file")
  <$> (`setAppEpilog` "Kevin S Kirkup")
  <$> mkApp parser


printCommandLineArgs :: MyConfig -> IO ()
printCommandLineArgs = putStrLn . input

main :: IO ()
main = do
  interface <- commandLineInterface
  runApp interface printCommandLineArgs
  someFunc

