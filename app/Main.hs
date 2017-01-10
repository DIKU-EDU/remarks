module Main where

import Parser
import Validator

import System.Environment ( getArgs )
import System.Exit ( exitWith, ExitCode ( ExitFailure ) )
import System.IO ( hPutStrLn, stderr )

import Text.PrettyPrint.GenericPretty

report :: String -> IO ()
report = hPutStrLn stderr

noFile :: IO ()
noFile = do
  report "Please specify a file to parse.."
  exitWith (ExitFailure 1)

printParse :: FilePath -> IO ()
printParse path = do
  p <- parseFile path
  case p of
    Right js -> putStrLn $ pretty $ map validate js
    Left e -> putStrLn $ show e

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> noFile
    ps -> mapM_ printParse ps
