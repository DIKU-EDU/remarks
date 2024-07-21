module PrettyPrinter (ppJ_d, ppJs, ppPoints, ppRemarks, ppRemark, ppPropExp) where

import Ast
-- use (<>) from Text.PrettyPrint

import Data.List (intersperse)
import Export.Generic (pointsDoc, propertyExpDoc)
import Text.PrettyPrint
import Prelude hiding ((<>))

ppJ_d :: Int -> Judgement -> String
ppJ_d d = render . (formatJudgement (d + 1))

ppJs :: [Judgement] -> String
ppJs = render . vcat . intersperse (text "") . map (formatJudgement 1)

ppPoints :: Points -> String
ppPoints = render . pointsDoc

ppRemarks :: [Remark] -> String
ppRemarks = render . vcat . (map formatRemark)

ppRemark :: Remark -> String
ppRemark = render . formatRemark

ppPropExp :: PropertyExp -> String
ppPropExp = render . propertyExpDoc

-- semicolon :: Doc
-- semicolon = text ";"

formatJudgement :: Int -> Judgement -> Doc
formatJudgement depth (Bonus (p, properties, comments)) =
  (text $ replicate depth '#')
    <+> text "Bonus"
    <> colon
    <+> text "+"
    <> pointsDoc (Given p)
    $+$ (nest 2 $ vcat $ map formatProperty properties)
    $+$ (nest 2 $ vcat $ map formatRemark comments)
formatJudgement depth (Feedback (properties, feedback)) =
  (text $ replicate depth '#') <+> text "Feedback" <> colon
    $+$ (nest 2 $ vcat $ map formatProperty properties)
    $+$ (text feedback)
formatJudgement depth (Judgement (header, properties, comments, judgements)) =
  formatHeader depth header
    $+$ (nest 2 $ vcat $ map formatProperty properties)
    $+$ (nest 2 $ vcat $ map formatRemark comments)
    $+$ (vcat $ map (formatJudgement (depth + 1)) judgements)

formatHeader :: Int -> Header -> Doc
formatHeader depth (Header (title, point, maxPoints)) =
  (text $ replicate depth '#')
    <+> text title
    <> colon
    <> space
    <> pointsDoc point
    <> text "/"
    <> pointsDoc (Given maxPoints)

formatProperty :: Property -> Doc
formatProperty (Property (name, value)) =
  colon <> text name <> colon <+> propertyExpDoc value

-- formatProperty (PdfMark pmType) =
--   colon <> text "pdfmark" <> colon <+> (formatPMType pmType)

-- formatPMType :: PdfMarkType -> Doc
-- formatPMType (PMComment page loc) =
--   text "Remark" <> semicolon <+> text page <> semicolon <+> text loc
-- formatPMType (PMTickBox _ page loc) =
--   text "TickBox" <> semicolon <+> text page <> semicolon <+> text loc

formatRemark :: Remark -> Doc
formatRemark (Remark (mood, commentParts)) =
  formatMood mood <+> (vcat $ map formatRemarkPart commentParts)

formatMood :: Mood -> Doc
formatMood Positive = text "+"
formatMood Negative = text "-"
formatMood Neutral = text "*"
formatMood Impartial = text "?"
formatMood Warning = text "!"
formatMood Mixed = text "~"

formatRemarkPart :: RemarkPart -> Doc
formatRemarkPart (RemarkStr string) = text string
formatRemarkPart (RemarkCmt comment) = formatRemark comment
