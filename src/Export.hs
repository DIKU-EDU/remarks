module Export (FeedbackOpts(..), exportFeedback, exportHTML, exportCSV, exportHTMLTable, exportResultsTable, exportMD, unify, summary, exportPdfMark) where

import Ast
import Invalid
import Export.Html
import Export.CSV
import Export.HtmlTable
import Export.ResultsTable
import Export.MD
import Export.PdfMark
import Export.Feedback
import Export.Generic ( unify, summary, toHTML, toCSV, transp )

import Text.PrettyPrint

exportHTML :: [Judgement] -> String
exportHTML = render . htmlRemarks

exportCSV :: String -> [String] -> [Judgement] -> Either Invalid String
exportCSV delim ps js = (pure . render) =<< csvRemarks delim ps js

exportHTMLTable :: [Judgement] -> String
exportHTMLTable js = toHTML $ transp $ htmlTableRemarks js

exportMD :: [Judgement] -> String
exportMD js = mdRemarks js

exportFeedback :: FeedbackOpts -> [Judgement] -> String
exportFeedback opts js = feedbackRemarks opts js

exportResultsTable :: [Judgement] -> String
exportResultsTable js = toCSV ";" $ transp $ resultsTableRemarks js
-- exportResultsTable js = toHTML $ resultsTableRemarks js

exportPdfMark :: [Judgement] -> String
exportPdfMark js = genPdfMark js
