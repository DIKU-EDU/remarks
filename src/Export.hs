module Export (exportHTML, exportCSV) where

import Ast
import Export.Html
import Export.CSV

import Text.PrettyPrint

exportHTML :: [Judgement] -> String
exportHTML = render . htmlRemarks

exportCSV :: String -> [String] -> [Judgement] -> String
exportCSV delim ps js = render $ csvRemarks delim ps js

