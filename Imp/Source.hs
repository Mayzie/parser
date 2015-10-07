
module Imp.Source where
import Imp.Source.Exp
import Imp.Source.Lexer
import Imp.Source.Tokens
import Imp.Parsec
import qualified Imp.Source.Parser      as PF


-- | Lex a thing, then parse it.
lexParse :: Parser Token a -> String -> Maybe a
lexParse p str
 = case lexer str of
        Nothing         -> Nothing
        Just tokens'
         -> case parse p tokens' of
                [(x, [])]       -> Just x
                _               -> Nothing


-- Functions for testing at the ghci command-line.
-- | Parse a string as a program.
programOfString :: String -> Maybe Program
programOfString str
 = lexParse PF.program str


-- | Parse an expr as a program.
exprOfString  :: String -> Maybe Exp
exprOfString str
 = lexParse PF.expr str
