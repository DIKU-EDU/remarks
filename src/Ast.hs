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
  = Header (String, Double, Double)
  deriving (Eq, Show, Generic)

instance Out Header

data Mood
  = Positive
  | Negative
  | Neutral
  | Impartial
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
  | Num  Double
  deriving (Eq, Show, Generic)

instance Out PropertyExp

data Judgement
  = Judgement (Header, [Property], [Comment], [Judgement])
  | Bonus (Double, [Comment])
  deriving (Eq, Show, Generic)

instance Out Judgement
