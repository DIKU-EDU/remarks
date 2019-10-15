module Export.PdfMark (genPdfMark) where

import Ast
import PrettyPrinter

import Export.Generic
import Text.PrettyPrint hiding ((<>))
import Data.List (intersperse)

genPdfMark :: [Judgement] -> String
genPdfMark js = render $ (header $+$ text "" $+$ jmts)
  where
    jmts = vcat . intersperse (text "") $ map formatJudgement js
    header =
      text "[ /Title (Annotation Remarks)" $+$
      text "  /Author (Kirkedal)" $+$
      text "  /Subject (Generated based on Remarks.)" $+$
      text "  /Keywords ()" $+$
      -- text "  /ModDate (D:20101219192842)" $+$
      -- text "  /CreationDate (D:20101219092842)" $+$
      text "  /Creator (Kirkedal)" $+$
      text "  /Producer (Ghostscript under the direction of Remarks)" $+$
      text "  /DOCINFO pdfmark"


formatJudgement :: Judgement -> Doc
formatJudgement j@(Judgement (_, properties, comments, judgements)) =
  (vcat $ map (formatPdfMark (formatHeader j) (formatPoints j) properties comments) properties) $+$
  (vcat $ map formatJudgement judgements)
formatJudgement j@(Bonus (_, properties, comments)) =
  (vcat $ map (formatPdfMark (text "Bonus") (formatPoints j) properties comments) properties)
formatJudgement (Feedback (properties, txt)) =
  (vcat $ map (formatPdfMark (text "Feedback") (text txt) properties []) properties)

formatPdfMark :: Doc -> Doc -> [Property] -> [Comment] -> Property -> Doc
formatPdfMark header points props comms (Property ("pdfmark", List pmtype)) =
  formatPdfMarkType pmtype header points props comms
formatPdfMark _ _ _ _ _ = empty

formatPdfMarkType :: [String] -> Doc -> Doc -> [Property] -> [Comment] -> Doc
formatPdfMarkType (("Comment"):rest) header points _ comms =
  text "[" <+> text "/Title" <+> (parens header) $+$
  text "  /Contents" <+> parens (points <> text "\n" <> (text $ formatComments comms)) $+$
  text "  /SrcPg" <+> text page $+$
  text "  /Rect" <+> text loc $+$
  text "  /Subtype /Text" $+$
  text "  /Name /Note" $+$
  text "  /Open true" $+$
  text "  /Color [.5 .5 0]" $+$
  text "  /ANN pdfmark"
  where
    page = head rest
    loc  = head $ tail rest
formatPdfMarkType _ _ _ _ _ = empty

formatComments :: [Comment] -> String
formatComments cs = concat $ intersperse "\n" $ map (\x -> "" ++ ppComment x) cs

formatHeader :: Judgement -> Doc
formatHeader j = text (getTitle j)

formatPoints :: Judgement -> Doc
formatPoints j@(Judgement (Header (_, NotMade, _), _, _, _)) = parens $ (text "-" <> text "/" <> text (getMaxPoints j))
formatPoints j = parens $ (text (getTotal j) <> text "/" <> text (getMaxPoints j))





