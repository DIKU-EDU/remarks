module Export (exportHTML, exportCSV) where

import Ast
import Invalid
import Export.Html
import Export.CSV

import Text.PrettyPrint

exportHTML :: [Judgement] -> String
exportHTML = render . htmlRemarks

exportCSV :: String -> [String] -> [Judgement] -> Either Invalid String
exportCSV delim ps js = (pure . render) =<< csvRemarks delim ps js

