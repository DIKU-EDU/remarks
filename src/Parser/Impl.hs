{-# LANGUAGE DeriveGeneric #-}

module Parser.Impl where

import Ast
import Config
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
  fs <- (string ".5") +++ pure "0"
  case (maybeRead (is ++ "." ++ fs)) of
    Just x -> pure x
    _ -> pfail

parseMaxPoints :: ReadP Double
parseMaxPoints = do
  is <- parseIntegral
  case (maybeRead is) of
    Just x -> pure x
    _ -> pfail

lineToken :: ReadP a -> ReadP a
lineToken p = munch (`elem` [' ', '\t', '\r', '\v', '\f']) *> p

lineBreak :: ReadP ()
lineBreak = void $ lineToken $ char '\n'

munchTillExcl :: Char -> ReadP String
munchTillExcl c = munch (/= c) <* char c

parseBonus :: String -> ReadP Judgement
parseBonus title = do
  lineToken $ char '+'
  points <- parsePoints
  void $ lineBreak
  comments <- many parseComment'
  pure $ Bonus (points, comments)

parseRegularJudgement :: Int -> String -> ReadP Judgement
parseRegularJudgement depth title = do
  points <- (lineToken $ parsePoints) +++ (return $ 1/0)
  void $ lineToken $ char '/'
  maxPoints <- lineToken $ parseMaxPoints
  void $ lineBreak

  let header = Header (title, points, maxPoints)

  comments <- many parseComment'
  subjs <- many $ parseJudgement (depth + 1)
  pure $ Judgement (header, comments, subjs)

parseJudgement :: Int -> ReadP Judgement
parseJudgement depth = skipSpaces *> do
  let mark = take depth $ repeat '#'
  void $ string mark
  void $ char ' '
  title <- lineToken $ munchTillExcl ':'

  case title of
    "Bonus" -> parseBonus title
    _ -> parseRegularJudgement depth title

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
parseCommentPart indent = many lineBreak *> do
  void $ string indent
  (fmap CommentCmt $ parseComment indent) <++
    (fmap CommentStr parseLine)

parseComment :: String -> ReadP Comment
parseComment indent = do
  mood <- parseMood
  void $ char ' '
  first <- fmap CommentStr parseLine
  rest <- many $ parseCommentPart (indent ++ indentation)
  pure $ Comment (mood, (first : rest))

parseComment' :: ReadP Comment
parseComment' = many lineBreak *> do
  void $ string indentation
  parseComment indentation

parseJudgements :: Int -> ReadP [Judgement]
parseJudgements depth = many $ parseJudgement depth

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

parseEntry :: ReadP [Judgement]
parseEntry = parseJudgements 1 <* skipSpaces

parseString :: String -> Either ParseError [Judgement]
parseString = parseString' parseEntry

parseFile' :: ReadP a -> FilePath -> IO (Either (ParseErrorImpl a) a)
parseFile' p path = fmap (parseString' p) $ readFile path

parseFile :: FilePath -> IO (Either ParseError [Judgement])
parseFile = parseFile' parseEntry
