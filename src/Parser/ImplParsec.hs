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

parseIndentation :: MrkParser ()
parseIndentation = string indentation >> pure ()

endline :: MrkParser ()
endline = void $ many1 newline

-- string :: String -> MrkParser ()
-- string s = sequence_ $ mapM char s

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

parsePointsNum :: MrkParser Int
parsePointsNum = try float <|> intAsFloat
  where
    intAsFloat = (pure . fromInteger . ((*) 100)) =<< integer

parsePoints :: MrkParser Points
parsePoints = try noAnswer <|> try point <|> (return NotGiven)
  where
    point = do
      p <- parsePointsNum
      return $ Given p
    noAnswer = char '-' >> return NotMade

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
  void $ space
  total <- parsePoints
  void $ char '/'
  maxPoints <- parsePointsNum
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
      p <- parsePointsNum
      void $ newline
      pure $ Num p
    -- value = do
      -- c <- satisfy (/='[')
      -- case c of
      --  '\n' -> pure $ Value ""
      --  _    -> do
      --    s <- manyTill (sepBy ";") endline
      --    (\cs -> pure $ Value (c:cs)) =<< 
    value = do
      s <- list
      case s of
       []  -> pure $ Value ""
       [v] -> pure $ Value v
       _   -> pure $ List s
    list = try listH1 <|> listH2
    listH1 = do
      -- spaces
      s  <- manyTill nolineBreak $ char ';'
      ss <- list
      pure (s:ss)
    listH2 = do
      -- spaces
      s <- manyTill anyChar $ char '\n'
      pure [s]
    nolineBreak = do
      c <- lookAhead anyChar
      guard (c /= '\n')
      anyChar



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
  , char '~' *> pure Mixed
  , char '*' *> pure Neutral
  , char '?' *> pure Impartial
  , char '!' *> pure Warning
  ]
