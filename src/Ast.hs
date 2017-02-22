{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Ast
Description : The remarks abstract-syntax tree
Copyright   : (c) DIKU, 2016-present
License     : EUPLv1.1
Stability   : experimental

All @.mrk@ files are parsed as a list of 'Judgement's, as defined below.
-}
module Ast where

import Text.PrettyPrint.GenericPretty

newtype Header
  = Header (String, Maybe Int, Int)
  deriving (Eq, Show, Generic)

instance Out Header

data Mood
  = Positive
  | Negative
  | Neutral
  | Impartial
  | Warning
  deriving (Eq, Show, Generic)

instance Out Mood

data CommentPart
  = CommentStr String
  | CommentCmt Comment
  deriving (Eq, Show, Generic)

instance Out CommentPart

newtype Comment
  = Comment (Mood, [CommentPart])
  deriving (Eq, Show, Generic)

instance Out Comment

newtype Property
  = Property (String, PropertyExp)
  deriving (Eq, Show, Generic)

instance Out Property

data PropertyExp
  = Lookup (Int, String)
  | Sum String
  | Value  String
  | Num  Int
  deriving (Eq, Show, Generic)

instance Out PropertyExp

data Judgement
  = Judgement (Header, [Property], [Comment], [Judgement])
  | Bonus (Int, [Property], [Comment])
  deriving (Eq, Show, Generic)

instance Out Judgement


isLeafJ :: Judgement -> Bool
isLeafJ (Judgement (_, _, _, []))    = True
isLeafJ (Judgement (_, _, _, (_:_))) = False
isLeafJ (Bonus _)                    = False

isNodeJ :: Judgement -> Bool
isNodeJ (Judgement (_, _, _, []))    = False
isNodeJ (Judgement (_, _, _, (_:_))) = True
isNodeJ (Bonus _)                    = False

isBonus :: Judgement -> Bool
isBonus (Bonus _)     = True
isBonus (Judgement _) = False