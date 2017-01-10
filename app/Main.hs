module Main where

import Ast
import Parser
import Validator

import Control.Monad ( void )

import System.Directory
  ( doesFileExist
  , doesDirectoryExist
  , listDirectory
  )
import System.FilePath ( (</>) )
import System.Environment ( getArgs )
import System.Exit ( exitWith, ExitCode ( ExitFailure ) )
import System.IO ( hPutStrLn, stderr )

import Text.PrettyPrint.GenericPretty

report :: String -> IO ()
report = hPutStrLn stderr

noCommand :: IO a
noCommand = do
  report "Tell me what to do!"
  exitWith (ExitFailure 1)

notAPath :: String -> IO a
notAPath s = do
  report $ s ++ " is not a filesystem path!"
  exitWith (ExitFailure 1)

invalidCommand :: String -> [String] -> IO a
invalidCommand c _ = do
  report $ c ++ " is not a valid command."
  exitWith (ExitFailure 1)

noTopLevelJudgement :: FilePath -> IO a
noTopLevelJudgement path = do
  report $ path ++ " has no top-level judgement."
  report "I don't know how to handle this, sorry."
  exitWith (ExitFailure 1)

parseError :: ParseError -> IO a
parseError e = do
  report $ show e
  exitWith (ExitFailure 1)

parseTopFile :: FilePath -> IO [Judgement]
parseTopFile path = do
  p <- parseFile path
  case p of
    Right js -> pure js
    Left e -> parseError e

parseSubFile :: FilePath -> IO Judgement
parseSubFile path = do
  p <- parseFile path
  case p of
    Right [j] -> pure j
    Right _ -> noTopLevelJudgement path
    Left e -> parseError e

parseDir :: FilePath -> IO [Judgement]
parseDir path = do
  paths <- listDirectory path
  let mrkpaths = filter (\_ -> True) paths -- TODO
  let longpaths = map (path </>) mrkpaths
  mapM parseSubFile longpaths

parsePath :: FilePath -> IO [Judgement]
parsePath path = do
  isFile <- doesFileExist path
  if isFile
  then parseTopFile path
  else do
    isDir <- doesDirectoryExist path
    if not isDir
    then notAPath path
    else parseDir path

parsePaths :: [FilePath] -> IO [[Judgement]]
parsePaths = mapM parsePath

check :: [Judgement] -> IO ()
check js = putStrLn $ pretty $ map validate js

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> noCommand
    ("parse" : paths) -> parsePaths paths >>= putStrLn . pretty
    ("check" : paths) -> parsePaths paths >>= mapM_ check
    (c:args) -> invalidCommand c args
