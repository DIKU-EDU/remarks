module Export.PdfMark (genPdfMark) where

import Ast
import Data.List (intersperse)
import Export.Generic
import PrettyPrinter
import Text.PrettyPrint hiding ((<>))

genPdfMark :: [Judgement] -> String
genPdfMark js = render $ (header $+$ text "" $+$ jmts)
  where
    jmts = vcat . intersperse (text "") $ map formatJudgement js
    header =
      text "[ /Title (Annotation Remarks)"
        $+$ text "  /Author (Kirkedal)"
        $+$ text "  /Subject (Generated based on Remarks.)"
        $+$ text "  /Keywords ()"
        $+$
        -- text "  /ModDate (D:20101219192842)" $+$
        -- text "  /CreationDate (D:20101219092842)" $+$
        text "  /Creator (Kirkedal)"
        $+$ text "  /Producer (Ghostscript under the direction of Remarks)"
        $+$ text "  /DOCINFO pdfmark"

formatJudgement :: Judgement -> Doc
formatJudgement j@(Judgement (_, properties, remarks, judgements)) =
  (vcat $ map (formatPdfMark (formatHeader j) (formatPoints j) properties remarks) properties)
    $+$ (vcat $ map formatJudgement judgements)
formatJudgement j@(Bonus (_, properties, remarks)) =
  (vcat $ map (formatPdfMark (text "Bonus") (formatPoints j) properties remarks) properties)
formatJudgement (Feedback (properties, txt)) =
  (vcat $ map (formatPdfMark (text "Feedback") (text txt) properties []) properties)

formatPdfMark :: Doc -> Doc -> [Property] -> [Remark] -> Property -> Doc
formatPdfMark header points props comms (Property ("pdfmark", List pmtype)) =
  formatPdfMarkType pmtype header points props comms
formatPdfMark _ _ _ _ _ = empty

formatPdfMarkType :: [String] -> Doc -> Doc -> [Property] -> [Remark] -> Doc
formatPdfMarkType (("Remark") : rest) header points _ comms =
  text "[" <+> text "/Title" <+> (parens header)
    $+$ text "  /Contents" <+> parens (points <> text "\n" <> (text $ formatRemarks comms))
    $+$ text "  /SrcPg" <+> text page
    $+$ text "  /Rect" <+> text loc
    $+$ text "  /Subtype /Text"
    $+$ text "  /Name /Note"
    $+$ text "  /Open true"
    $+$ text "  /Color [.5 .5 0]"
    $+$ text "  /ANN pdfmark"
  where
    page = head rest
    loc = head $ tail rest
formatPdfMarkType _ _ _ _ _ = empty

formatRemarks :: [Remark] -> String
formatRemarks cs = concat $ intersperse "\n" $ map (\x -> "" ++ (escapeParens $ ppRemark x)) cs

formatHeader :: Judgement -> Doc
formatHeader j = text (getTitle j)

formatPoints :: Judgement -> Doc
formatPoints j@(Judgement (Header (_, NotMade, _), _, _, _)) = parens $ (text "-" <> text "/" <> text (getMaxPoints j))
formatPoints j = parens $ (text (getTotal j) <> text "/" <> text (getMaxPoints j))

escapeParens :: String -> String
escapeParens (')' : xs) = '\\' : ')' : escapeParens xs
escapeParens ('(' : xs) = '\\' : '(' : escapeParens xs
escapeParens (x : xs) = x : escapeParens xs
escapeParens "" = ""
