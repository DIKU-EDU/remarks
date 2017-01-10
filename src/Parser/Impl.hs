{-# LANGUAGE DeriveGeneric #-}

module Parser.Impl where

import Ast
import Text.ParserCombinators.ReadP
import Text.PrettyPrint.GenericPretty

import Control.Monad ( void )
import Data.Maybe ( listToMaybe )

data ParseErrorImpl a
  = NoParse
  | AmbiguousGrammar [a]
  | NotImplemented
  deriving (Eq, Show, Generic)

instance (Out a) => Out (ParseErrorImpl a)

type ParseError = ParseErrorImpl [Judgement]

parseIntegral :: ReadP String
parseIntegral = munch1 (`elem` ['0'..'9'])

-- Source: http://hackage.haskell.org/package/cgi-3001.3.0.2/docs/src/Network-CGI-Protocol.html#maybeRead
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

parsePoints :: ReadP Double
parsePoints = do
  is <- parseIntegral
  fs <- (char '.' *> parseIntegral) +++ pure "0"
  case (maybeRead (is ++ "." ++ fs)) of
    Just x -> pure x
    _ -> pfail

lineToken :: ReadP a -> ReadP a
lineToken p = munch (`elem` [' ', '\t']) *> p

munchTillExcl :: Char -> ReadP String
munchTillExcl c = munch (/= c) <* char c

parseHeader :: Int -> ReadP Header
parseHeader depth = do
  let mark = take depth $ repeat '#'
  void $ string mark
  void $ char ' '
  title <- lineToken $ munchTillExcl ':'

  points <- lineToken $ parsePoints
  void $ lineToken $ char '/'
  maxPoints <- lineToken $ parsePoints

  void $ lineToken $ char '\n'

  pure $ Header (title, points, maxPoints)

parseMood :: ReadP Mood
parseMood = choice
  [ char '+' *> pure Positive
  , char '-' *> pure Negative
  , char '*' *> pure Neutral
  , char '?' *> pure Impartial
  ]

parseLine :: ReadP String
parseLine = munchTillExcl '\n'

parseLines :: String -> ReadP [String]
parseLines indent = many $ string indent *> parseLine

parseCommentPart :: String -> ReadP CommentPart
parseCommentPart indent = do
  void $ string indent
  (fmap CommentCmt $ parseComment indent) <++
    (fmap CommentStr parseLine)

parseComment :: String -> ReadP Comment
parseComment indent = do
  mood <- parseMood
  void $ char ' '
  first <- fmap CommentStr parseLine
  rest <- many $ parseCommentPart (indent ++ "  ")
  pure $ Comment (mood, (first : rest))

parseComment' :: ReadP Comment
parseComment' = do
  void $ string "  "
  parseComment "  "

parseJudgement :: Int -> ReadP Judgement
parseJudgement depth = do
  header <- parseHeader depth
  comments <- many parseComment'
  subjs <- many $ parseJudgement (depth + 1)
  pure $ Judgement (header, comments, subjs)

parseJudgements :: Int -> ReadP [Judgement]
parseJudgements = many . parseJudgement

parse :: ReadP a -> String -> [(a, String)]
parse = readP_to_S

fullParse :: ReadP a -> String -> [a]
fullParse p s = fmap fst $ parse (p <* (skipSpaces >> eof)) s

parseString' :: ReadP a -> String -> Either (ParseErrorImpl a) a
parseString' p s =
  case fullParse p s of
    [] -> Left NoParse
    [a] -> Right a
    as -> Left $ AmbiguousGrammar as

parseString :: String -> Either ParseError [Judgement]
parseString = parseString' (parseJudgements 1)

parseFile' :: ReadP a -> FilePath -> IO (Either (ParseErrorImpl a) a)
parseFile' p path = fmap (parseString' p) $ readFile path

parseFile :: FilePath -> IO (Either ParseError [Judgement])
parseFile = parseFile' (parseJudgements 1)
