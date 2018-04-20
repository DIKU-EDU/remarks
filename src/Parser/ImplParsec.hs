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
         return (parse parseRemarks fname input)

-- |Parse a Remarks Judgement from a string
parseString :: String -> IO (Either ParseError [Judgement])
parseString input = return $ parse parseRemarks "String" input

-------------------------------------------------------------------------------
-- * Implementation of the parser
-------------------------------------------------------------------------------

parseLine :: MrkParser String
parseLine = manyTill anyChar (many1 newline)

endline :: MrkParser ()
endline = void $ many1 newline

integral :: MrkParser String
integral = many1 digit

float :: MrkParser Int
float = do
  i <- option "0" integral
  void $ char '.'
  d1 <- digit
  d2 <- option '0' digit
  pure $ read (i ++ [d1, d2])

integer :: MrkParser Integer
integer = (pure . read) =<< integral

parsePoints :: MrkParser Int
parsePoints = try noAnswer <|> try float <|> intAsFloat
  where
    intAsFloat = (pure . fromInteger . ((*) 100)) =<< integer
    noAnswer = char '-' >> return 0

parseRemarks :: MrkParser [Judgement]
parseRemarks = do
  js <- many1 (parseJudgement 1)
  void spaces
  eof
  pure js

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
  endline
  properties <- many parseProperty
  comments <- many $ parseComment 1
  pure $ Bonus (total, properties, comments)


parseRegularJudgement :: Int -> String -> MrkParser Judgement
parseRegularJudgement depth title = do
  total <- optionMaybe parsePoints
  void $ char '/'
  maxPoints <- parsePoints
  endline
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
parsePropertyExp = choice [try funProp, lookupProp, try number, value]
  where
    funProp = do
      fun <- parsePropertyArithFun
      void $ char '('
      name <- manyTill anyChar $ char ')'
      void $ newline
      pure $ ArithFun fun name
    lookupProp = do
      void $ char '['
      index <- integer
      void $ char '.'
      name <- manyTill anyChar $ char ']'
      void $ newline
      pure $ Lookup (fromInteger index, name)
    number = do
      p <- parsePoints
      void $ newline
      pure $ Num p
    value = do
      c <- satisfy (/='[')
      case c of
       '\n' -> pure $ Value ""
       _    -> (\cs -> pure $ Value (c:cs)) =<< parseLine

parsePropertyArithFun :: MrkParser PropertyArithFun
parsePropertyArithFun = choice
  [ string "sum" *> pure Sum
  , string "min" *> pure Min
  , string "max" *> pure Max
  ]

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
