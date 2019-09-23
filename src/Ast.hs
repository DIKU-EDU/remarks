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
  = Header (String, Points, Int)
  deriving (Eq, Show, Generic)

instance Out Header

data Points
  = NotGiven
  | NotMade
  | Given Int
  deriving (Eq, Show, Generic)

instance Out Points

data Mood
  = Positive
  | Negative
  | Mixed
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
  | ArithFun PropertyArithFun String
  | Value String
  | List [String]
  | Num  Int
  deriving (Eq, Show, Generic)

instance Out PropertyExp

data PdfMarkType
  = PMComment String String -- Page Location
  | PMTickBox     (Maybe Bool) String String -- Correct/Wrong Page Location
  deriving (Eq, Show, Generic)

instance Out PdfMarkType

data PropertyArithFun
  = Sum
  | Min
  | Max
  deriving (Eq, Show, Generic)

instance Out PropertyArithFun

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