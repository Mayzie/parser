
module Imp.Source.Parser where
import Imp.Source.Exp
import Imp.Source.Tokens
import Imp.Parsec


-- | Whole program.
program  :: Parser Token Program
program = return $ Program []
-- TODO: finish me


-- | Parse an expression.
expr  :: Parser Token Exp
expr
 = alts 
 [      -- number
   do   n       <- num
        return  $ XNum n

        -- single identifier
 , do   i       <- ident
        return  $ XId i
 ]


-- | Parse a number.
num   :: Parser Token Int
num = from takeNum


-- | Parse an identifier.
ident :: Parser Token Id
ident
 = do   i       <- from takeId
        return  $ Id i 


-- | Parse arguments separated by commas.
idents :: Parser Token [Id]
idents
 = alts
 [ do   i       <- ident
        only KComma
        is      <- idents
        return  $ i : is

 , do   i       <- ident
        return  [i]
 ]


-- | Parse an operator.
oper :: Parser Token Op
oper
 = alts
 [ do   only    (KOp str)
        return  op
 | (str, op)    <- ops]


-- | Operator names.
ops :: [(String, Op)]
ops
 =      [ ("+",  OpAdd)
        , ("-",  OpSub)
        , ("*",  OpMul)
        , ("/",  OpDiv)
        , ("<",  OpLt)
        , (">",  OpGt)
        , ("==", OpEq) ]       

