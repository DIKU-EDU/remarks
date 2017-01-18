module Main where

import Ast
import Parser
import PointsChecker
import PropertyInterp
import PrettyPrinter
import Export

import Control.Monad ( void, filterM, liftM, (<=<) )
import Data.List ( sort )
import System.Directory
  ( doesFileExist, doesDirectoryExist, listDirectory )
import System.FilePath
  ( (<.>), (</>), takeDirectory, takeExtension, dropExtension )
import System.Environment( getArgs )
import System.Exit ( exitWith, ExitCode ( ExitFailure ) )
import System.IO ( hPutStrLn, stderr )

import Text.PrettyPrint.GenericPretty

splitBy delimiter = foldr f [[]] 
  where f c l@(x:xs) | c == delimiter = []:l
                     | otherwise = (c:x):xs

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

noMrkFile :: FilePath -> IO a
noMrkFile path = do
  report $ "Can't find a .mrk file\n" ++ path
  exitWith (ExitFailure 1)

noTopLevelJudgement :: FilePath -> IO a
noTopLevelJudgement path = do
  report $ path ++ " has no top-level judgement."
  report "I don't know how to handle this, sorry."
  exitWith (ExitFailure 1)

mustHaveJudgement :: FilePath -> IO a
mustHaveJudgement path = do
  report $ path ++ " must have at least one judgement."
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

parseFileInDir :: FilePath -> IO [Judgement]
parseFileInDir path = do
  js <- parsePath path
  case js of
    [] -> noTopLevelJudgement path
    _ -> pure js

parseFileWithDir :: FilePath -> IO ([Judgement], Judgement)
parseFileWithDir path = do
  p <- parseFile path
  case p of
    Right js ->
      case length js of
        0 -> mustHaveJudgement path
        n -> pure $ (take (n - 1) js, last js)
    Left e -> parseError e

isMrkPath :: FilePath -> Bool
isMrkPath = (== ".mrk") . takeExtension

parseDir :: FilePath -> IO [Judgement]
parseDir path = do
  paths <- listDirectory path
  let mrkPaths = filter isMrkPath paths
  let fullMrkPaths = map (path </>) (sort mrkPaths)
  liftM concat $ mapM parseFileInDir fullMrkPaths

extPaths :: FilePath -> (FilePath, FilePath)
extPaths path =
  if isMrkPath path
  then (path, dropExtension path)
  else (path <.> "mrk", path)

parsePath :: FilePath -> IO [Judgement]
parsePath path = do
  let (pathWithExt, pathWithoutExt) = extPaths path
  hasFile <- doesFileExist pathWithExt
  hasDir <- doesDirectoryExist pathWithoutExt

  if not hasFile
  then
    if hasDir
    then parseDir path
    else noMrkFile pathWithExt
  else -- we have a .mrk file
    if not hasDir
    then parseTopFile pathWithExt
    else do -- we now also have directory
      (fjs, (Judgement (h, p, cs, js))) <- parseFileWithDir pathWithExt
      dirJs <- parseDir pathWithoutExt
      pure $ fjs ++ [Judgement (h, p, cs, js ++ dirJs)]

parsePaths :: [FilePath] -> IO [[Judgement]]
parsePaths = mapM parsePath

check :: [Judgement] -> IO ()
check js = do
  case mapM (interpProps <=< checkPoints) js of
    Right newJs -> printJs newJs
    Left e -> putStrLn $ show e

printJs :: [Judgement] -> IO ()
printJs = putStrLn . ppJs

export :: [String] -> [Judgement] -> IO ()
export format js = do
  case mapM (interpProps <=< checkPoints) js of
    Right newJs -> putStrLn $ exportCSV ";" format newJs
    Left e -> putStrLn $ show e

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> noCommand
    ("parse" : paths) -> parsePaths paths >>= putStrLn . pretty
    ("check" : paths) -> parsePaths paths >>= mapM_ check
    ("show" : paths) -> parsePaths paths >>= mapM_ printJs
    ("export" : "--format" : format : paths) -> parsePaths paths >>= mapM_ (export (splitBy ';' format))
    ("export" : paths) -> parsePaths paths >>= mapM_ (export ["Title", "Total", "MaxPoints"])
    (c:args) -> invalidCommand c args
