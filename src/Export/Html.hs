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
tag tagStr attr doc = (text "<" <> text tagStr <> attr <> text ">") $$ nest 2 doc $$ text ("</" ++ tagStr ++ ">")

etag :: String -> Doc -> Doc
etag tagStr = tag tagStr empty

atag :: String -> String -> Doc -> Doc
atag tagStr attrStr = tag tagStr (space <> text attrStr)

doctype :: Doc -> Doc
doctype = ($$) (text "<!DOCTYPE html>")

html :: Doc -> Doc
html   = etag "html"

head_ :: Doc -> Doc
head_  = etag "head"

body :: Doc -> Doc
body   = etag "body"

script :: Doc -> Doc
script = atag "script" "type=\"text/javascript\""

style_ :: Doc -> Doc
style_ = etag "style"

table :: Doc -> Doc
table  = atag "table" "border=\"1\""

tr :: Doc -> Doc
tr     = etag "tr"

trhidden :: Doc -> Doc
trhidden = atag "tr" "style=\"display: none;\""

th :: Doc -> Doc
th     = etag "th"

td :: Doc -> Doc
td     = etag "td"

toggle :: Doc -> Doc
toggle = atag "a" "href=\"#\" onclick=\"toggleRow(this);\""

ul :: Doc -> Doc
ul     = etag "ul"

liclass :: String -> Doc -> Doc
liclass c = atag "li" $ "class=\"" ++ c ++ "\""

tdspan :: Show a => a -> Doc -> Doc
tdspan i  = atag "td" $ "colspan=\"" ++ (show i) ++ "\""

details :: Doc -> Doc -> Doc
details d1 d2 = etag "details" ((etag "summary" d1) $$ d2)

documentStyle :: Doc
documentStyle = style_ $
  text "details {padding-left: 16px;}" $$
  text "ul {list-style: none; padding-left: 16px; padding-top: 0px; padding-bottom: 0px; margin-top: 0px; margin-bottom: 0px;}" $$
  text "li.plus:before {content: \"+\"; margin-right: 4px;}" $$
  text "li.minus:before {content: \"-\"; margin-right: 4px;}" $$
  text "li.quest:before {content: \"?\"; margin-right: 4px;}" $$
  text "li.star:before {content: \"*\"; margin-right: 4px;}"

documentScript :: Doc
documentScript = script $
  nest 2 ((text "function toggleRow(e){") $$
          nest  2 ((text "var subRow = e.parentNode.parentNode.nextElementSibling;") $$
          text "subRow.style.display = subRow.style.display === 'none' ? 'table-row' : 'none';") $$
   (text "}"))

htmlTableHead :: Judgement -> Doc
htmlTableHead (Bonus _) =
  tr $ th (text "Bonus")
htmlTableHead (Judgement (_, _, _, js)) =
  tr $ th (text "Title") $$ (vcat $ map maketd js) $$ th (text "Total")
  where
    maketd (Judgement (Header(title, _, maxPoint), _, _, _)) =
      th $ text (title ++ "/") <> pointsDoc maxPoint
    maketd (Bonus _) =
      th $ text "Bonus"

htmlJudgement :: Judgement -> Doc
htmlJudgement (Bonus (points, comments)) =
  (tr $ td $ pointsDoc points) $$ (trhidden $ htmlDetailComments comments)
htmlJudgement (Judgement (Header(title, points, _), _, comments, judgements)) =
  (tr $ (td . toggle $ text title) $$
    vcat (map htmlSubJudgement judgements) $$
    (td $ pointsDoc points)) $$
  (trhidden $ tdspan (length judgements+2) $ (htmlDetailComments comments $$ htmlDetailJudgements judgements))

htmlSubJudgement :: Judgement -> Doc
htmlSubJudgement (Bonus (points, _)) =
  (td $ pointsDoc points)
htmlSubJudgement (Judgement (Header(_, points, _), _, _, _)) =
  (td $ pointsDoc points)

htmlDetailJudgements :: [Judgement] -> Doc
htmlDetailJudgements = vcat . (map htmlDetailJudgement)

htmlDetailJudgement :: Judgement -> Doc
htmlDetailJudgement (Bonus (points, comments)) =
  details (text "Bonus" <+> parens (pointsDoc points)) (htmlDetailComments comments)
htmlDetailJudgement (Judgement (Header(title, points, maxPoint), _, comments, judgements)) =
  details
    (text title <+> parens (pointsDoc points <> text "/" <> pointsDoc maxPoint))
    (htmlDetailComments comments $$ htmlDetailJudgements judgements)

htmlDetailComments :: [Comment] -> Doc
htmlDetailComments [] = empty
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
