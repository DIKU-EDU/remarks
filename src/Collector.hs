module Collector (collectHTML) where

import Ast
import Collector.Html

import Text.PrettyPrint

collectHTML :: [Judgement] -> String
collectHTML = render . htmlRemarks
