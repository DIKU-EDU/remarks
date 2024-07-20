-- |
--
-- A wrapper module, exposing the main parsing types and functions.
--
-- For a concrete implementation, see "Parser.ImplMegaparsec".
--
-- This wrapping allows to both treat the parser as a black box (e.g.,
-- [Parser.BlackBoxTests](https://github.com/DIKU-EDU/remarks/blob/master/test/Parser/BlackBoxTests.hs)),
-- as well as import and use (i.e., test or debug)
-- implementation-specific details.
module Parser
  ( ParseError,
    parseString,
    parseFile,
  )
where

import Parser.ImplMegaparsec
  ( ParseError,
    parseFile,
    parseString,
  )
