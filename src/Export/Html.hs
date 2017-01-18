module Export.Html (htmlRemarks) where

import Ast

import Export.Generic
import Text.PrettyPrint

htmlRemarks :: [Judgement] -> Doc
htmlRemarks js = 
  doctype $ html $ 
    (head_ $ documentStyle $$ documentScript) $$ 
    (body . table $ htmlTableHead (head js) $+$ vcat (map htmlJudgement js))

tag :: String -> Doc -> Doc -> Doc
tag tagStr attr doc = (text "<" <> text tagStr <+> attr <> text ">") $$ nest 2 doc $$ text ("</" ++ tagStr ++ ">") 

etag :: String -> Doc -> Doc
etag tagStr = tag tagStr empty

atag :: String -> String -> Doc -> Doc
atag tagStr attrStr = tag tagStr (text attrStr)

doctype = ($$) (text "<!DOCTYPE html>")
html   = etag "html"
head_  = etag "head"
body   = etag "body"
script = atag "script" "type=\"text/javascript\""
style_ = etag "style"
table  = atag "table" "border=\"1\""
tr     = etag "tr"
trhidden = atag "tr" "style=\"display: none;\""
th     = etag "th"
td     = etag "td"
toggle = atag "a" "href=\"#\" onclick=\"toggleRow(this);\""
ul     = etag "ul"
li     = etag "li"

liclass c = atag "li" $ "class=\"" ++ c ++ "\""
tdspan i  = atag "td" $ "colspan=\"" ++ (show i) ++ "\""

br = text "<br>"

details d1 d2 = etag "details" ((etag "summary" d1) $$ d2)

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

htmlTableHead :: Judgement -> Doc
htmlTableHead (Judgement (_, _, _, js)) = 
  tr $ th (text "Title") $$ (vcat $ map maketd js) $$ th (text "Total")
  where
    maketd (Judgement (Header(title, _, maxPoint), _, _, _)) = th $ text (title ++ "/") <> pointsDoc maxPoint

htmlJudgement :: Judgement -> Doc
htmlJudgement (Judgement (Header(title, points, maxPoint), _, comments, judgements)) = 
  (tr $ (td . toggle $ text title) $$ vcat (map htmlSubJudgement judgements) $$ (td $ pointsDoc points)) $$
  (trhidden $ tdspan (length judgements+2) $ (htmlDetailComments comments $$ htmlDetailJudgements judgements))

htmlSubJudgement :: Judgement -> Doc
htmlSubJudgement (Judgement (Header(title, points, maxPoint), _, comments, judgements)) = 
  (td $ pointsDoc points) 

htmlDetailJudgements :: [Judgement] -> Doc 
htmlDetailJudgements = vcat . (map htmlDetailJudgement) 

htmlDetailJudgement :: Judgement -> Doc
htmlDetailJudgement (Judgement (Header(title, points, maxPoint), _, comments, judgements)) = 
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
