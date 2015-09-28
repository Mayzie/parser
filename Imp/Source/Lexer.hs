
module Imp.Source.Lexer where
import Imp.Source.Tokens
import Imp.Parsec


-- | Lex a string into tokens.
lexer :: String -> Maybe [Token]
lexer str
 = case parse tokens str of
        [(tokens', "")]  -> Just tokens'
        _                -> Nothing


-- | Parse a sequence of tokens,
--   separated by arbitrary white space.
tokens :: Parser Char [Token]
tokens
 = do   some space
        t  <- token
        some space
        ts <- alt tokens (return [])
        some space
        return (t : ts)


-- | Parse a single token.
token :: Parser Char Token
token 
 = altss        
        [ do    match str
                return tok
        | (str, tok)    <- atoms]

 $ alt  (do     n   <- nat
                return $ KNum n)

        (do     str <- many alphanum
                return $ KId str)


-- | Atomic tokens.
atoms :: [(String, Token)]
atoms
 =      [ -- operators
          ("+",         KOp "+")
        , ("-",         KOp "-")
        , ("*",         KOp "*")
        , ("/",         KOp "/")
        , ("<",         KOp "<")
        , (">",         KOp ">")
        , ("==",        KOp "==")

          -- punctuation
        , ("(",         KRoundBra)
        , (")",         KRoundKet)
        , ("{",         KBraceBra)
        , ("}",         KBraceKet)
        , (",",         KComma)
        , (";",         KSemi)
        , ("=",         KEquals)
  
          -- keywords
        , ("fun",       Kfun)
        , ("vars",      Kvars)
        , ("if",        Kif)
        , ("then",      Kthen)
        , ("else",      Kelse)
        , ("return",    Kreturn) 
        ] 
