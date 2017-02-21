{-# LANGUAGE DeriveGeneric #-}

module Parser.ImplParsec (parseString, parseFile, ParseError) where

import Ast
import Config

import Text.ParserCombinators.Parsec hiding (parse,parseFromFile)
import Text.Parsec.Prim (runP)
import Control.Monad.Identity

type ParserState = [String]

initialState :: ParserState
initialState = []

type MrkParser = GenParser Char ParserState

-------------------------------------------------------------------------------
-- * Functions for parsing
-------------------------------------------------------------------------------

parse :: MrkParser a -> SourceName -> String -> Either ParseError a
parse p = runP p initialState

-- |Parse a Remarks Judgement from a file
parseFile :: String -> IO (Either ParseError [Judgement])
parseFile fname
    = do input <- readFile fname
         return (parse (parseJudgements 1) fname input)

-- |Parse a Remarks Judgement from a string
parseString :: String -> IO (Either ParseError [Judgement])
parseString input = return $ parse (parseJudgements 1) "String" input

-------------------------------------------------------------------------------
-- * Implementation of the parser
-------------------------------------------------------------------------------

parseLine :: MrkParser String
parseLine = manyTill anyChar newline

integral :: MrkParser String
integral = many1 digit

float :: MrkParser Double
float = do
  i <- option "0" integral
  void $ char '.'
  d <- integral
  pure $ read (i ++ "." ++ d)

integer :: MrkParser Integer
integer = (pure . read) =<< integral

parsePoints :: MrkParser Double
parsePoints = try float <|> intAsFloat
  where
    intAsFloat = (pure . fromInteger) =<< integer

parseJudgements :: Int -> MrkParser [Judgement]
parseJudgements depth = many1 (parseJudgement depth)

parseJudgement :: Int -> MrkParser Judgement
parseJudgement depth = do
  void $ try $ spaces >> (string $ replicate depth judgementMarker)
  void $ space
  title <- manyTill anyChar $ char ':'
  void $ space
  case title of
    "Bonus" -> parseBonus depth
    _ -> parseRegularJudgement depth title

parseBonus :: Int -> MrkParser Judgement
parseBonus _ = do
  void $ char '+'
  total <- parsePoints
  void $ newline
  properties <- many parseProperty
  comments <- many $ parseComment 1
  pure $ Bonus (total, properties, comments)


parseRegularJudgement :: Int -> String -> MrkParser Judgement
parseRegularJudgement depth title = do
  total <- option (1/0) parsePoints
  void $ char '/'
  maxPoints <- parsePoints
  void $ newline
  properties <- many parseProperty
  comments <- many $ parseComment 1
  js <- many (parseJudgement (depth + 1))
  pure $ Judgement (Header (title, total, maxPoints), properties, comments, js)

parseProperty :: MrkParser Property
parseProperty = do
      void $ try $ string $ indentation ++ ":"
      name <- manyTill anyChar $ char ':'
      void $ space
      value <- parsePropertyExp
      pure $ Property (name, value)

parsePropertyExp :: MrkParser PropertyExp
parsePropertyExp = choice [lookupProp, value]
  where
    lookupProp = do
      void $ char '['
      index <- integer
      void $ char '.'
      name <- manyTill anyChar $ char ']'
      pure $ Lookup (fromInteger index, name)
    value = do
      c <- satisfy (/='[')
      case c of
       '\n' -> pure $ Value ""
       _    -> (\cs -> pure $ Value (c:cs)) =<< parseLine

parseComment :: Int -> MrkParser Comment
parseComment depth = do
  void $ try $ string $ concat $ replicate depth indentation
  mood <- parseMood
  void $ space
  comment <- parseLine
  morecmt <- many contStr
  comments <- many (parseComment $ depth + 1)
  pure $ Comment (mood, [(CommentStr comment)] ++ (map CommentStr morecmt) ++ (map CommentCmt comments))
  where
    contStr = do
      void $ try $ ((string $ concat $ replicate (depth + 1) indentation) >> notFollowedBy parseMood)
      parseLine

parseMood :: MrkParser Mood
parseMood = choice
  [ char '+' *> pure Positive
  , char '-' *> pure Negative
  , char '*' *> pure Neutral
  , char '?' *> pure Impartial
  , char '!' *> pure Warning
  ]
