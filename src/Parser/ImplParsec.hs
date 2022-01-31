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
parseString :: String -> Either ParseError [Judgement]
parseString input = parse parseRemarks "String" input

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

anyCharNoLineBreak :: MrkParser Char
anyCharNoLineBreak =
  do
    notFollowedBy newline
    anyChar

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
  void $ spaces
  title <- manyTill anyCharNoLineBreak $ char ':'
  case title of
    "Bonus" -> parseBonus depth
    "Feedback" -> parseFeedback depth
    _ -> parseRegularJudgement depth title

parseBonus :: Int -> MrkParser Judgement
parseBonus _ = do
  void $ spaces
  void $ char '+'
  total <- parsePointsNum
  endline
  properties <- many parseProperty
  comments <- many $ parseComment 1
  pure $ Bonus (total, properties, comments)

parseFeedback :: Int -> MrkParser Judgement
parseFeedback depth = do
  endline
  properties <- many parseProperty
  text <- manyTill anyChar $ try (lookAhead (parseJudgement depth))
  pure $ Feedback (properties, text)
  -- where
  --   parseJudgementStart = do
  --     _ <- char judgementMarker
  --     void $ spaces
  --     void $ manyTill (anyCharNoLineBreak) $ char ':'

parseRegularJudgement :: Int -> String -> MrkParser Judgement
parseRegularJudgement depth title = do
  void $ spaces
  total <- parsePoints
  void $ char '/'
  maxPoints <- parsePointsNum
  endline
  properties <- many parseProperty
  comments <- many $ parseComment 1
  js <- many (parseJudgement (depth + 1))
  pure $ Judgement (Header (title, total, maxPoints), properties, comments, js)

parseProperty :: MrkParser Property
parseProperty = try property
  where
  --   pdfmark :: MrkParser Property
  --   pdfmark = do
  --     parseIndentation
  --     char ':'
  --     string "pdfmark"
  --     char ':'
  --     space
  --     pmtype <- parsePdfMarkType
  --     pure $ PdfMark pmtype
    property = do
      -- void $ try $ string $ indentation ++ ":"
      parseIndentation
      _ <- char ':'
      name <- manyTill anyChar $ char ':'
      spaces
      value <- parsePropertyExp
      void $ newline
      pure $ Property (name, value)

-- parsePdfMarkType :: MrkParser PdfMarkType
-- parsePdfMarkType = try comment <|> try box
--   where
--     comment = do
--       string "Comment"
--       string ";"
--       space
--       page <- manyTill anyChar $ char ';'
--       loc  <- manyTill anyChar endline
--       pure $ PMComment page loc
--     box = do
--       string "Box"
--       string ";"
--       page <- manyTill anyChar $ char ';'
--       loc  <- manyTill anyChar $ char ';'
--       pure $ PMTickBox Nothing page loc

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
      v <- manyTill anyChar $ char ']'
      pure $ Value v
    lookupPropParent = do
      void $ char '['
      name <- manyTill anyChar $ char ']'
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
      name <- manyTill anyChar $ char '"'
      pure $ Value name
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
  , try (string "min") *> pure Min
  , string "max" *> pure Max
  , try (string "points") *> pure PointMap
  , string "prod" *> pure Prod
  , string "div" *> pure Div
  , try (string "index") *> pure Map
  , string "if" *> pure If
  ]

parseComment :: Int -> MrkParser Comment
parseComment depth = do
  void $ try $ string $ concat $ replicate depth indentation
  mood <- parseMood
  void $ spaces
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
  , char '^' *> pure Positive
  , char 'v' *> pure Negative
  , char '*' *> pure Neutral
  , char '?' *> pure Impartial
  , char '!' *> pure Warning
  ]
