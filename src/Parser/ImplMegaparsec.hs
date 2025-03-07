{-# LANGUAGE DeriveGeneric #-}

-- |
--
-- An implementation of "Parser" using
-- [megaparsec](https://hackage.haskell.org/package/megaparsec).
--
-- The module declaration does not limit its exports to enable white-box
-- testing and debugging.
module Parser.ImplMegaparsec where

import Ast
import Config
import Control.Monad (guard, void)
import Data.Void
import Text.Megaparsec hiding (ParseError, parse)
import qualified Text.Megaparsec as P
import Text.Megaparsec.Char

type MrkParser = Parsec Void String

type ParseError = ParseErrorBundle String Void

-------------------------------------------------------------------------------

-- * Functions for parsing

-------------------------------------------------------------------------------

parse :: MrkParser a -> FilePath -> String -> Either ParseError a
parse p = P.parse p

-- | Parse a Remarks Judgement from a file
parseFile :: String -> IO (Either ParseError [Judgement])
parseFile fname =
  do
    input <- readFile fname
    return (parse parseRemarks fname input)

-- | Parse a Remarks Judgement from a string
parseString :: String -> Either ParseError [Judgement]
parseString input = parse parseRemarks "String" input

-------------------------------------------------------------------------------

-- * Implementation of the parser

-------------------------------------------------------------------------------

endline :: MrkParser ()
endline = choice [void $ some newline, eof]

parseLine :: MrkParser String
parseLine = manyTill anySingle endline

parseIndentation :: MrkParser ()
parseIndentation = string indentation >> pure ()

anyCharNoLineBreak :: MrkParser Char
anyCharNoLineBreak =
  do
    notFollowedBy newline
    anySingle

integral :: MrkParser String
integral = some digitChar

float :: MrkParser Int
float = do
  i <- option "0" integral
  void $ char '.'
  d1 <- digitChar
  d2 <- option '0' digitChar
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
  js <- some (parseJudgement 1)
  space
  eof
  pure js

parseJudgement :: Int -> MrkParser Judgement
parseJudgement depth = do
  void $ try $ space >> string (replicate depth judgementMarker)
  space
  title <- manyTill anyCharNoLineBreak $ char ':'
  case title of
    "Bonus" -> parseBonus depth
    "Feedback" -> parseFeedback depth
    _ -> parseRegularJudgement depth title

parseBonus :: Int -> MrkParser Judgement
parseBonus _ = do
  space
  void $ char '+'
  total <- parsePointsNum
  endline
  properties <- sepEndBy parseProperty endline
  remarks <- many $ parseRemark 1
  pure $ Bonus (total, properties, remarks)

parseFeedback :: Int -> MrkParser Judgement
parseFeedback depth = do
  endline
  properties <- sepEndBy parseProperty endline
  text <- manyTill anySingle $ try (lookAhead (parseJudgement depth))
  pure $ Feedback (properties, text)

parseRegularJudgement :: Int -> String -> MrkParser Judgement
parseRegularJudgement depth title = do
  space
  total <- parsePoints
  void $ char '/'
  maxPoints <- parsePointsNum
  hspace
  endline
  properties <- sepEndBy parseProperty newline
  remarks <- many $ parseRemark 1
  js <- many (parseJudgement (depth + 1))
  pure $ Judgement (Header (title, total, maxPoints), properties, remarks, js)

parseProperty :: MrkParser Property
parseProperty = try property
  where
    property = do
      parseIndentation
      _ <- char ':'
      name <- manyTill anySingle $ char ':'
      space
      value <- parsePropertyExp
      pure $ Property (name, value)

parsePropertyExp :: MrkParser PropertyExp
parsePropertyExp = choice [try funProp, try lookupPropChild, try lookupPropParent, try number, stringVal, value]
  where
    funProp = do
      fun <- parsePropertyArithFun
      void $ char '('
      name <- sepBy1 parsePropertyExp (char ',')
      void $ char ')'
      -- Check length of if
      pure $ ArithFun fun name
    lookupPropChild = do
      void $ char '['
      index <- integer
      void $ char '.'
      -- name <- manyTill anyChar $ char ']'
      name <- choice [try lookupPropChild, cleanValue]
      pure $ Lookup (fromInteger index, name)
    cleanValue = do
      v <- manyTill anySingle $ char ']'
      pure $ Value v
    lookupPropParent = do
      void $ char '['
      name <- manyTill anySingle $ char ']'
      pure $ Lookup (0, Value name)
    number = do
      p <- parsePointsNum
      pure $ Num p
    -- value = do
    -- c <- satisfy (/='[')
    -- case c of
    --  '\n' -> pure $ Value ""
    --  _    -> do
    --    s <- manyTill (sepBy ";") endline
    --    (\cs -> pure $ Value (c:cs)) =<<
    stringVal = do
      void $ char '"'
      name <- manyTill anySingle $ char '"'
      pure $ Value name
    value = do
      s <- list
      case s of
        [] -> pure $ Value ""
        [v] -> pure $ Value v
        _ -> pure $ List s
    list = try listH1 <|> listH2
    listH1 = do
      -- spaces
      s <- manyTill nolineBreak $ char ';'
      ss <- list
      pure (s : ss)
    listH2 = do
      -- spaces
      s <- manyTill anySingle $ endline
      pure [s]
    nolineBreak = do
      c <- lookAhead anySingle
      guard (c /= '\n')
      anySingle

parsePropertyArithFun :: MrkParser PropertyArithFun
parsePropertyArithFun =
  choice
    [ string "sum" *> pure Sum,
      try (string "min") *> pure Min,
      string "max" *> pure Max,
      try (string "points") *> pure PointMap,
      string "prod" *> pure Prod,
      string "div" *> pure Div,
      try (string "index") *> pure Map,
      string "if" *> pure If
    ]

parseRemark :: Int -> MrkParser Remark
parseRemark depth = do
  void $ try $ string $ concat $ replicate depth indentation
  mood <- parseMood
  space
  remark <- parseLine
  morecmt <- many contStr
  remarks <- many (parseRemark $ depth + 1)
  pure $ Remark (mood, [(RemarkStr remark)] ++ (map RemarkStr morecmt) ++ (map RemarkCmt remarks))
  where
    contStr = do
      void $ try $ ((string $ concat $ replicate (depth + 1) indentation) >> notFollowedBy parseMood)
      parseLine

parseMood :: MrkParser Mood
parseMood =
  choice
    [ char '+' *> pure Positive,
      char '-' *> pure Negative,
      char '~' *> pure Mixed,
      char '^' *> pure Positive,
      char 'v' *> pure Negative,
      char '*' *> pure Neutral,
      char '?' *> pure Impartial,
      char '!' *> pure Warning
    ]
