module Collector.Html (htmlRemarks) where

import Ast

import Text.PrettyPrint

htmlRemarks :: [Judgement] -> Doc
htmlRemarks js = 
  html $ 
    (head_ $ documentStyle $$ documentScript) $$ 
    (body . table $ htmlTableHead (head js) $+$ vcat (map htmlJudgement js))

tag :: String -> String -> Doc -> Doc
tag tagStr contStr doc = text ("<" ++ tagStr ++ " " ++ contStr ++ ">") $$ nest 2 doc $$ text ("</" ++ tagStr ++ ">") 

html   = tag "html" ""
head_  = tag "head" ""
body   = tag "body" ""
script = tag "script" "type=\"text/javascript\""
style_ = tag "style" ""
table  = tag "table" "border=\"1\""
tr     = tag "tr" ""
trhidden = tag "tr" "style=\"display: none;\""
th     = tag "th" ""
td     = tag "td" ""
toggle = tag "a" "href=\"#\" onclick=\"toggleRow(this);\""
ul     = tag "ul" ""
li     = tag "li" ""

liclass c = tag "li" $ "class=\"" ++ c ++ "\""
tdspan i  = tag "td" $ "colspan=\"" ++ (show i) ++ "\""

br = text "<br>"

details d1 d2 = tag "details" "" ((tag "summary" "" d1) $$ d2)

documentStyle = style_ $ 
  text "details {padding-left: 16px;}" $$      
  text "ul {list-style: none; padding-left: 16px; padding-top: 0px; padding-bottom: 0px; margin-top: 0px; margin-bottom: 0px;}" $$
  text "li.plus:before {content: \"+\"; margin-right: 4px;}" $$
  text "li.minus:before {content: \"-\"; margin-right: 4px;}" $$
  text "li.quest:before {content: \"?\"; margin-right: 4px;}" $$
  text "li.star:before {content: \"*\"; margin-right: 4px;}"

documentScript = script $ 
  nest 2 ((text "function toggleRow(e){") $$
          nest  2 ((text "var subRow = e.parentNode.parentNode.nextElementSibling;") $$
          text "subRow.style.display = subRow.style.display === 'none' ? 'table-row' : 'none';") $$
   (text "}"))

isIntegral :: Double -> Bool
isIntegral x = x == fromInteger (round x)

pointsDoc :: Double -> Doc
pointsDoc v | isNaN v = empty
pointsDoc v | isIntegral v = integer (round v)
pointsDoc v = double v

htmlTableHead :: Judgement -> Doc
htmlTableHead (Judgement (_, _, js)) = 
  tr $ th (text "Title") $$ (vcat $ map maketd js) $$ th (text "Total")
  where
    maketd (Judgement (Header(title, _, maxPoint), _, _)) = th $ text (title ++ "/") <> pointsDoc maxPoint

htmlJudgement :: Judgement -> Doc
htmlJudgement (Judgement (Header(title, points, maxPoint), comments, judgements)) = 
  (tr $ (td . toggle $ text title) $$ vcat (map htmlSubJudgement judgements) $$ (td $ pointsDoc points)) $$
  (trhidden $ tdspan (length judgements+2) $ (htmlDetailComments comments $$ htmlDetailJudgements judgements))

htmlSubJudgement :: Judgement -> Doc
htmlSubJudgement (Judgement (Header(title, points, maxPoint), comments, judgements)) = 
  (td $ pointsDoc points) 

htmlDetailJudgements :: [Judgement] -> Doc 
htmlDetailJudgements = vcat . (map htmlDetailJudgement) 

htmlDetailJudgement :: Judgement -> Doc
htmlDetailJudgement (Judgement (Header(title, points, maxPoint), comments, judgements)) = 
  details 
    (text title <> parens (pointsDoc points <> text "/" <> pointsDoc maxPoint)) 
    (htmlDetailComments comments $$ htmlDetailJudgements judgements)

htmlDetailComments :: [Comment] -> Doc
htmlDetailComments [] = text ""
htmlDetailComments comments = 
  ul . vcat $ map htmlDetailComment comments

htmlDetailComment :: Comment -> Doc
htmlDetailComment (Comment (mood, commentParts)) =
  liclass (htmlDetailMood mood) $ vcat $ map htmlDetailCommentPart commentParts

htmlDetailMood :: Mood -> String
htmlDetailMood Positive  = "plus"
htmlDetailMood Negative  = "minus"
htmlDetailMood Neutral   = "star"
htmlDetailMood Impartial = "quest"

htmlDetailCommentPart :: CommentPart -> Doc
htmlDetailCommentPart (CommentStr string) = text string
htmlDetailCommentPart (CommentCmt comment) =
  ul $ htmlDetailComment comment
