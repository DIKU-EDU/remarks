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
  text "li.excl:before {content: \"!\"; margin-right: 4px;}" $$
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
  tr $ th (text "Title") $$ (vcat $ map maketh js) $$ th (text "Total")
  where
    maketh (Judgement (Header(title, _, maxPoint), _, _, _)) =
      th $ text (title ++ "/") <> pointsDoc (Just maxPoint)
    maketh (Bonus _) =
      th $ text "Bonus"

htmlJudgement :: Judgement -> Doc
htmlJudgement (j @ (Bonus (_, _, comments))) =
  (tr $ td $ lookupTotal j) $$ (trhidden $ htmlDetailComments comments)
htmlJudgement (j @ (Judgement (_, _, comments, judgements))) =
  (tr $ (td . toggle $ lookupTitle j) $$
    vcat (map htmlSubJudgement judgements) $$
    (td $ lookupTotal j)) $$
  (trhidden $ tdspan (length judgements+2) $ (htmlDetailComments comments $$ htmlDetailJudgements judgements))

htmlSubJudgement :: Judgement -> Doc
htmlSubJudgement j = td $ lookupTotal j

htmlDetailJudgements :: [Judgement] -> Doc
htmlDetailJudgements = vcat . (map htmlDetailJudgement)

htmlDetailJudgement :: Judgement -> Doc
htmlDetailJudgement (j @ (Bonus (_, _, comments))) =
  details (text "Bonus" <+> parens (lookupTotal j)) (htmlDetailComments comments)
htmlDetailJudgement (j @ (Judgement (_, _, comments, judgements))) =
  details
    (lookupTitle j <+> parens (lookupTotal j <> text "/" <> lookupMaxPoints j))
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
htmlDetailMood Warning   = "excl"

htmlDetailCommentPart :: CommentPart -> Doc
htmlDetailCommentPart (CommentStr string) = text string
htmlDetailCommentPart (CommentCmt comment) =
  ul $ htmlDetailComment comment
