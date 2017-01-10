module Main where

import Parser
import Validator

import System.Environment ( getArgs )
import System.Exit ( exitWith, ExitCode ( ExitFailure ) )
import System.IO ( hPutStrLn, stderr )

import Text.PrettyPrint.GenericPretty

report :: String -> IO ()
report = hPutStrLn stderr

noCommand :: IO ()
noCommand = do
  report "Tell me what to do!"
  exitWith (ExitFailure 1)

invalidCommand :: String -> [String] -> IO ()
invalidCommand c _ = do
  report $ c ++ " is not a valid command."
  exitWith (ExitFailure 1)

printParse :: FilePath -> IO ()
printParse path = do
  p <- parseFile path
  case p of
    Right js -> putStrLn $ pretty $ map validate js
    Left e -> putStrLn $ show e

check :: [FilePath] -> IO ()
check = mapM_ printParse

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> noCommand
    ("check" : paths) -> check paths
    (c:args) -> invalidCommand c args
