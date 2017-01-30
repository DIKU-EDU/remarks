{-# LANGUAGE DeriveGeneric #-}

module Parser.Impl where

import Ast
import Config
import Text.ParserCombinators.ReadP
import Text.PrettyPrint.GenericPretty

import Control.Monad ( void )
import Data.Maybe ( listToMaybe )

data ParseErrorImpl a
  = NoParse FilePath
  | AmbiguousGrammar [a] FilePath
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
parsePoints = choice [p3, p2, p1]
  where
    p1 = do
      is <- parseIntegral
      case (maybeRead is) of
        Just x -> pure x
        _ -> pfail
    p2 = do
      is <- parseIntegral
      void $ char '.'
      fs <- parseIntegral
      case (maybeRead (is ++ "." ++ fs)) of
        Just x -> pure x
        _ -> pfail
    p3 = do
      void $ char '.'
      fs <- parseIntegral
      case (maybeRead ("0." ++ fs)) of
        Just x -> pure x
        _ -> pfail

lineToken :: ReadP a -> ReadP a
lineToken p = munch (`elem` [' ', '\t', '\r', '\v', '\f']) *> p

lineBreak :: ReadP ()
lineBreak = void $ lineToken $ char '\n'

munchTillExcl :: Char -> ReadP String
munchTillExcl c = munch (/= c) <* char c

parseBonus :: ReadP Judgement
parseBonus = do
  void $ lineToken $ char '+'
  points <- parsePoints
  void $ lineBreak
  comments <- many parseComment'
  pure $ Bonus (points, comments)

parsePropertyExp :: ReadP PropertyExp
parsePropertyExp = choice [lookupProp, value]
  where
    lookupProp = do
      void $ char '['
      index <- parseIntegral
      void $ char '.'
      name <- lineToken $ munchTillExcl ']'
      void $ lineBreak
      case (maybeRead (index)) of
        Just i -> pure $ Lookup (i, name)
        _ -> pfail
    value = do
      c <- satisfy (/='[')
      case c of
       '\n' -> pure $ Value ""
       _    -> (\cs -> pure $ Value (c:cs)) =<< parseLine

parseProperty :: ReadP Property
parseProperty = do
  void $ string "  :"
  name <- lineToken $ munchTillExcl ':'
  void $ char ' '
  value <- parsePropertyExp
  pure $ Property (name, value)

parseRegularJudgement :: Int -> String -> ReadP Judgement
parseRegularJudgement depth title = do
  points <- (lineToken $ parsePoints) +++ (return $ 1/0)
  void $ lineToken $ char '/'
  maxPoints <- lineToken $ parsePoints
  void $ lineBreak

  let header = Header (title, points, maxPoints)

  properties <- many parseProperty
  comments <- many parseComment'
  subjs <- many $ parseJudgement (depth + 1)
  pure $ Judgement (header, properties, comments, subjs)

parseJudgement :: Int -> ReadP Judgement
parseJudgement depth = skipSpaces *> do
  let mark = take depth $ repeat '#'
  void $ string mark
  void $ char ' '
  title <- lineToken $ munchTillExcl ':'

  case title of
    "Bonus" -> parseBonus
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

parseString' :: ReadP a -> FilePath -> String -> Either (ParseErrorImpl a) a
parseString' p path s =
  case fullParse p s of
    [] -> Left $ NoParse path
    [a] -> Right a
    as -> Left $ AmbiguousGrammar as path

parseEntry :: ReadP [Judgement]
parseEntry = parseJudgements 1 <* skipSpaces

parseString :: String -> Either ParseError [Judgement]
parseString = parseString' parseEntry "String"

parseFile' :: ReadP a -> FilePath -> IO (Either (ParseErrorImpl a) a)
parseFile' p path = fmap (parseString' p path) $ readFile path

parseFile :: FilePath -> IO (Either ParseError [Judgement])
parseFile = parseFile' parseEntry
