
module Imp.Core.Tokens where
import Imp.Core.Exp


-- | Tokens of the source language.
data Token
        = KNum  Int             -- Number
        | KReg  Int
        | KId   String          -- Identifier
        | KOp   OpArith

        -- Punctuation
        | KRoundBra             -- '('
        | KRoundKet             -- ')'

        -- Keywords
        | KLc
        | KLd
        | KSt
        | KBr
        | KRet
        | KCall
        deriving (Show, Eq)


-- | Take a number from a token.
takeNum :: Token -> Maybe Int
takeNum tt
 = case tt of
        KNum n  -> Just n
        _       -> Nothing


-- | Take an identifier from a token.
takeId :: Token -> Maybe String
takeId tt
 = case tt of
        KId s   -> Just  s
        _       -> Nothing

takeReg :: Token -> Maybe Int
takeReg tt
 = case tt of
        KReg n  -> Just n
        _       -> Nothing