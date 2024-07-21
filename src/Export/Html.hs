module Export.Html (htmlRemarks) where

import Ast
import Export.Generic
-- use (<>) from Text.PrettyPrint
import Text.PrettyPrint
import Prelude hiding ((<>))

htmlRemarks :: [Judgement] -> Doc
htmlRemarks js =
  doctype $
    html $
      (head_ $ documentStyle $$ documentScript)
        $$ (body . table $ htmlTableHead (head js) $+$ vcat (map htmlJudgement js))

tag :: String -> Doc -> Doc -> Doc
tag tagStr attr doc = (text "<" <> text tagStr <> attr <> text ">") $$ nest 2 doc $$ text ("</" ++ tagStr ++ ">")

etag :: String -> Doc -> Doc
etag tagStr = tag tagStr empty

atag :: String -> String -> Doc -> Doc
atag tagStr attrStr = tag tagStr (space <> text attrStr)

doctype :: Doc -> Doc
doctype = ($$) (text "<!DOCTYPE html>")

html :: Doc -> Doc
html = etag "html"

head_ :: Doc -> Doc
head_ = etag "head"

body :: Doc -> Doc
body = etag "body"

script :: Doc -> Doc
script = atag "script" "type=\"text/javascript\""

style_ :: Doc -> Doc
style_ = etag "style"

table :: Doc -> Doc
table = atag "table" "border=\"1\""

tr :: Doc -> Doc
tr = etag "tr"

trhidden :: Doc -> Doc
trhidden = atag "tr" "style=\"display: none;\""

th :: Doc -> Doc
th = etag "th"

td :: Doc -> Doc
td = etag "td"

toggle :: Doc -> Doc
toggle = atag "span" "href=\"#\" onclick=\"toggleRow(this);\" style=\"text-decoration: underline;color: blue;\" "

ul :: Doc -> Doc
ul = etag "ul"

liclass :: String -> Doc -> Doc
liclass c = atag "li" $ "class=\"" ++ c ++ "\""

tdspan :: (Show a) => a -> Doc -> Doc
tdspan i = atag "td" $ "colspan=\"" ++ (show i) ++ "\""

details :: Doc -> Doc -> Doc
details d1 d2 = etag "details" ((etag "summary" d1) $$ d2)

documentStyle :: Doc
documentStyle =
  style_ $
    text "details {padding-left: 16px;}"
      $$ text "ul {list-style: none; padding-left: 16px; padding-top: 0px; padding-bottom: 0px; margin-top: 0px; margin-bottom: 0px;}"
      $$ text "li.plus:before {content: \"+\"; margin-right: 4px;}"
      $$ text "li.minus:before {content: \"-\"; margin-right: 4px;}"
      $$ text "li.quest:before {content: \"?\"; margin-right: 4px;}"
      $$ text "li.excl:before {content: \"!\"; margin-right: 4px;}"
      $$ text "li.excl:before {content: \"!\"; margin-right: 4px;}"
      $$ text "li.star:before {content: \"*\"; margin-right: 4px;}"

documentScript :: Doc
documentScript =
  script $
    nest
      2
      ( (text "function toggleRow(e){")
          $$ nest
            2
            ( (text "var subRow = e.parentNode.parentNode.nextElementSibling;")
                $$ text "subRow.style.display = subRow.style.display === 'none' ? 'table-row' : 'none';"
            )
          $$ (text "}")
      )

htmlTableHead :: Judgement -> Doc
htmlTableHead (Feedback _) = empty
htmlTableHead (Bonus _) =
  tr $ th (text "Bonus")
htmlTableHead (Judgement (_, _, _, js)) =
  tr $ th (text "Title") $$ (vcat $ map maketh js) $$ th (text "Total")
  where
    maketh (Judgement (Header (title, _, maxPoint), _, _, _)) =
      th $ text (title ++ "/") <> pointsDoc (Given maxPoint)
    maketh (Bonus _) =
      th $ text "Bonus"
    maketh (Feedback _) = empty

htmlJudgement :: Judgement -> Doc
htmlJudgement (Feedback _) = empty
htmlJudgement (j@(Bonus (_, _, remarks))) =
  (tr $ td $ lookupTotal j) $$ (trhidden $ htmlDetailRemarks remarks)
htmlJudgement (j@(Judgement (_, _, remarks, judgements))) =
  ( tr $
      (td . toggle $ lookupTitle j)
        $$ vcat (map htmlSubJudgement judgements)
        $$ (td $ lookupTotal j)
  )
    $$ (trhidden $ tdspan (length judgements + 2) $ (htmlDetailRemarks remarks $$ htmlDetailJudgements judgements))

htmlSubJudgement :: Judgement -> Doc
htmlSubJudgement j = td $ lookupTotal j

htmlDetailJudgements :: [Judgement] -> Doc
htmlDetailJudgements = vcat . (map htmlDetailJudgement)

htmlDetailJudgement :: Judgement -> Doc
htmlDetailJudgement (Feedback _) = empty
htmlDetailJudgement (j@(Bonus (_, _, remarks))) =
  details (text "Bonus" <+> parens (lookupTotal j)) (htmlDetailRemarks remarks)
htmlDetailJudgement (j@(Judgement (_, _, remarks, judgements))) =
  details
    (lookupTitle j <+> parens (lookupTotal j <> text "/" <> lookupMaxPoints j))
    (htmlDetailRemarks remarks $$ htmlDetailJudgements judgements)

htmlDetailRemarks :: [Remark] -> Doc
htmlDetailRemarks [] = empty
htmlDetailRemarks remarks =
  ul . vcat $ map htmlDetailRemark remarks

htmlDetailRemark :: Remark -> Doc
htmlDetailRemark (Remark (mood, remarkParts)) =
  liclass (htmlDetailMood mood) $ vcat $ map htmlDetailRemarkPart remarkParts

htmlDetailMood :: Mood -> String
htmlDetailMood Positive = "plus"
htmlDetailMood Mixed = "tilde"
htmlDetailMood Negative = "minus"
htmlDetailMood Neutral = "star"
htmlDetailMood Impartial = "quest"
htmlDetailMood Warning = "excl"

htmlDetailRemarkPart :: RemarkPart -> Doc
htmlDetailRemarkPart (RemarkStr string) = text string
htmlDetailRemarkPart (RemarkCmt remark) =
  ul $ htmlDetailRemark remark
