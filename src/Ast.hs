{-# LANGUAGE DeriveGeneric #-}

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

data Judgement
  = Judgement (Header, [Comment], [Judgement])
  | Bonus (Double, [Comment])
  deriving (Eq, Show, Generic)

instance Out Judgement
