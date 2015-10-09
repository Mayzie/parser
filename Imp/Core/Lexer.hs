
module Imp.Core.Lexer where
import Imp.Core.Tokens
import Imp.Core.Exp
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

 $ alts [ (do   n   <- nat
                return $ KNum n)

        , (do   char 'r'
                n <- nat
                return $ KReg n)

        , (do   str <- many alphanum
                return $ KId str)]


-- | Atomic tokens.
atoms :: [(String, Token)]
atoms
 =      [
          -- punctuation
          ("(",         KRoundBra)
        , (")",         KRoundKet)
  
          -- keywords
        , ("lc",        KLc)
        , ("ld",        KLd)
        , ("st",        KSt)
        , ("br",        KBr)
        , ("ret",       KRet)
        , ("call",      KCall) 

        , ("add",       KOp OpAdd)
        , ("sub",       KOp OpSub)
        , ("mul",       KOp OpMul) 
        , ("div",       KOp OpDiv) 
        , ("lt",        KOp OpLt)
        , ("gt",        KOp OpGt)
        , ("eq",        KOp OpEq)
        ] 
