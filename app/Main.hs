module Main where

import Ast
import Export
import Invalid
import Parser
import PointsChecker
import PropertyInterp
import Pending
import PrettyPrinter
import MergeAsts

import Control.Monad ( liftM, (<=<) )
import Data.List ( sort )
import Data.Maybe ( fromMaybe )
import Options.Applicative hiding ( Parser, ParseError )
import qualified Options.Applicative as App
import System.Directory
  ( doesFileExist, doesDirectoryExist, listDirectory )
import System.FilePath
  ( (<.>), (</>), takeExtension, dropExtension, takeDirectory )
import System.Exit ( exitWith, ExitCode ( ExitFailure ) )
import System.IO ( hPutStrLn, stderr )

import Text.PrettyPrint.GenericPretty

splitBy :: (Foldable f, Eq t) => t -> f t -> [[t]]
splitBy delimiter = foldr f []
  where f c [] = f c [[]]
        f c l@(x:xs) | c == delimiter = []:l
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
    Right js -> do
      js_g <- extendWithProp globalProp js
      extendWithProp localProp js_g
    Left e -> parseError e
  where
    globalProp = (takeDirectory path) </> "global.mrk-prop"
    localProp = (dropExtension path) <.> "mrk-prop"
    extendWithProp proppath js_to_extend = do
      hasPropFile <- doesFileExist proppath
      if hasPropFile
      then do
        p_p <- parseFile proppath
        case p_p of
          Right js_p -> pure $ mergeProps js_to_extend js_p
          Left e -> parseError e
      else pure js_to_extend

parseFileInDir :: FilePath -> IO [Judgement]
parseFileInDir path = do
  js <- parsePath path
  case js of
    [] -> noTopLevelJudgement path
    _ -> pure js

parseFileWithDir :: FilePath -> IO ([Judgement], Judgement)
parseFileWithDir path = do
  js <- parseTopFile path
  case length js of
    0 -> mustHaveJudgement path
    n -> pure $ (take (n - 1) js, last js)

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

marshall :: [Judgement] -> Either Invalid [Judgement]
marshall = mapM (interpProps <=< checkPoints)

check :: [Judgement] -> IO ()
check js = do
  case marshall js of
    -- Right newJs -> printJs newJs
    Right _ -> return ()
    Left e -> report $ reportInvalid e

valid :: [Judgement] -> IO ()
valid js = do
  case v js of
    -- Right newJs -> printJs newJs
    Right _ -> return ()
    Left e -> report $ reportInvalid e
  where
    v = mapM (interpProps <=< validPoints)

printJs :: [Judgement] -> IO ()
printJs = putStrLn . ppJs

export :: String -> [Judgement] -> IO ()
export format js = do
  case (exportCSV [delimiter] formatList =<< (mapM interpProps js)) of
    Right docs -> putStrLn docs
    Left e -> report $ reportInvalid e
  where
    formatUpd = replaceTabs format
    delimiter = findDelimiter formatUpd
    formatList = splitBy delimiter formatUpd

export_html :: [Judgement] -> IO ()
export_html js = do
  case (mapM interpProps js) of
    Right newJs -> putStrLn $ exportHTML newJs
    Left e -> report $ reportInvalid e

export_table :: [Judgement] -> IO ()
export_table js = do
  case (mapM interpProps js) of
    Right newJs -> putStrLn $ exportHTMLTable newJs
    Left e -> report $ reportInvalid e

export_results :: [Judgement] -> IO ()
export_results js = do
  case (mapM interpProps js) of
    Right newJs -> putStrLn $ exportResultsTable newJs
    Left e -> report $ reportInvalid e

export_md :: [Judgement] -> IO ()
export_md js = do
  case (mapM interpProps js) of
    Right newJs -> putStrLn $ exportMD newJs
    Left e -> report $ reportInvalid e

export_pdfMark :: [Judgement] -> IO ()
export_pdfMark js = do
  case (mapM interpProps js) of
    Right newJs -> putStrLn $ exportPdfMark newJs
    -- Right newJs -> putStrLn $ show newJs
    Left e -> report $ reportInvalid e

export_feedback :: FeedbackOpts -> [Judgement] -> IO ()
export_feedback opts js = do
  case (mapM interpProps js) of
    Right newJs -> putStrLn $ exportFeedback opts newJs
    -- Right newJs -> putStrLn $ show newJs
    Left e -> report $ reportInvalid e


showSummary :: Word -> [Judgement] -> IO ()
showSummary depth js = do
  case marshall js of
    Right mJs -> printJs $ map (summary depth) mJs
    Left e -> report $ reportInvalid e

pending :: Maybe Int -> (String, [Judgement]) -> IO ()
pending dl (fp,js) = do
  putStr $ fp ++ ": "
  case findPending dl js of
    Nothing  -> putStrLn "No pending corrections."
    (Just s) -> putStrLn "The following corrections are pending:" >> putStrLn s

status :: Maybe Int -> (String, [Judgement]) -> IO ()
status dl (fp,js) = do
  putStr $ fp ++ ": "
  case findStatus dl js of
    Nothing  -> putStrLn "No pending corrections."
    (Just s) -> putStrLn "The following is the corrections status:" >> putStrLn s

replaceTabs :: String -> String
replaceTabs []  = []
replaceTabs [s] = [s]
replaceTabs ('\\':'t':rest) = '\t':(replaceTabs rest)
replaceTabs (s1:s2:rest) = s1:(replaceTabs (s2:rest))

findDelimiter :: String -> Char
findDelimiter [] = ';' -- There is no delimiter so it doesn't matter
findDelimiter (s:ss) =
  if (elem s [',',';','\t'])
  then s
  else findDelimiter ss

data Command
  = CmdParse
  | CmdFeedback Bool
  | CmdCheck
  | CmdValid
  | CmdShow
  | CmdPending (Maybe Int)
  | CmdStatus (Maybe Int)
  | CmdSummary (Maybe Word)
  | CmdExport (Maybe String)
  | CmdExportResults
  | CmdExportTable
  | CmdExportHTML
  | CmdExportMD
  | CmdExportPdfMark
  deriving (Show)

data Args = Args
  { argsCommand :: Command
  , argsPaths :: [String]
  }
  deriving (Show)

cmdFeedbackParser :: App.Parser Command
cmdFeedbackParser =
  CmdFeedback
    <$> switch
      ( long "with-points" )
  <**> helper

cmdWithDepthParser :: Read a => (Maybe a -> Command) ->  App.Parser Command
cmdWithDepthParser cmd =
  cmd
    <$> (optional $ option auto
      ( long "depth" ))
  <**> helper

cmdExportParser :: App.Parser Command
cmdExportParser =
  CmdExport
    <$> (optional $ strOption
      ( long "format" ))
  <**> helper

argsParser :: App.Parser Args
argsParser = Args
  <$> subparser
    ( command "parse" (info (pure CmdParse) (progDesc
        "parse the given PATHs and show their ASTs"))
   <> command "feedback" (info cmdFeedbackParser (progDesc
        "accumulate feedback for PATHs"))
   <> command "valid" (info (pure CmdValid) (progDesc
        "validate structure of files at PATHs"))
   <> command "check" (info (pure CmdCheck) (progDesc
        "validate and check that points have been given at PATHs"))
   <> command "show" (info (pure CmdShow) (progDesc
        "validate, check, and pretty-print"))
   <> command "pending" (info (cmdWithDepthParser CmdPending) (progDesc
        "show judgements not yet marked"))
   <> command "status" (info (cmdWithDepthParser CmdStatus) (progDesc
        "show status(?)"))
   <> command "summary" (info (cmdWithDepthParser CmdSummary) (progDesc
        "validate, check, and summarise the points"))
   <> command "export" (info cmdExportParser (progDesc
        "exports corrections to a semicolon separated list; the format is a semicolon separated string of properties"))
   <> command "exportTable" (info (pure CmdExportTable) (progDesc
        "exports all corrections to a dynamic html-table"))
   <> command "exportResults" (info (pure CmdExportResults) (progDesc
        "export results to a table with result of each sub question"))
   <> command "exportMD" (info (pure CmdExportMD) (progDesc
        "exports remarks to a MarkDown file"))
   <> command "exportHTML" (info (pure CmdExportHTML) (progDesc
        "export remarks to an HTML file"))
   <> command "exportPdfMark" (info (pure CmdExportPdfMark) (progDesc
        "export remarks to format that can be inserted as comments in PDD with GhostScript"))
    )
  <*> some (strArgument
        (metavar "PATH..."
        <> help "List of paths to process" ))

argsInfo :: ParserInfo Args
argsInfo = info (argsParser <**> helper)
  ( fullDesc
  <> progDesc "remarks is a suite of tools for marking student work."
  <> header "remarks - machinery for marking student work" )

main :: IO ()
main = do
  args <- execParser argsInfo
  let paths = argsPaths args
  case argsCommand args of
    CmdParse ->
      with paths $ putStrLn . pretty
    CmdFeedback points ->
      with paths $ mapM_ (export_feedback $
        FeedbackOpts { withPoints = points })
    CmdValid ->
      with paths $ mapM_ valid
    CmdCheck ->
      with paths $ mapM_ check
    CmdShow ->
      with paths $ mapM_ printJs
    CmdPending maybeDepth ->
      withpath paths $ mapM_ (pending maybeDepth)
    CmdStatus maybeDepth ->
      withpath paths $ mapM_ (status maybeDepth)
    CmdSummary maybeDepth ->
      with paths $ mapM_ $ showSummary $ fromMaybe 0 maybeDepth
    CmdExport maybeFormat ->
      with paths $ mapM_ $ export $
        fromMaybe "Title\tTotal\tMaxPoints" maybeFormat
    CmdExportTable ->
      with paths $ mapM_ export_table
    CmdExportResults ->
      with paths $ mapM_ export_results
    CmdExportMD ->
      with paths $ mapM_ export_md
    CmdExportHTML ->
      with paths $ mapM_ export_html
    CmdExportPdfMark ->
      with paths $ mapM_ export_pdfMark
  where
    with :: [FilePath] -> ([[Judgement]] -> IO ()) -> IO ()
    with paths f = parsePaths paths >>= f
    withpath :: [FilePath] -> ([(String,[Judgement])] -> IO ()) -> IO ()
    withpath paths f =
      do
        js <- parsePaths paths
        let fp = map (tail.init.show) paths
            res = zip fp js
        f res
