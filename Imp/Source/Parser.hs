
module Imp.Source.Parser where
import Imp.Source.Exp
import Imp.Source.Tokens
import Imp.Parsec


-- | Whole program.
program  :: Parser Token Program
program
 = alts
 [
   do  f     <- functions
       return   $ Program f
 ]

args :: Parser Token [Id]
args
 = alts
 [
   do  only KRoundBra
       i <- idents
       only KRoundKet
       return $ i
 , do  only KRoundBra
       only KRoundKet
       return $ []
 ] 
 
functions :: Parser Token [Function]
functions
 = alts
 [
   do  f1       <- function
       f2       <- functions
       return   $ f1 : f2
 , do  return   $ []
 ]
 
function :: Parser Token Function
function 
 = alts
 [
   do  only Kfun
       name     <- ident
       a        <- args
       v        <- vars
       b        <- block
       return   $ Function name a v b
 ]
 
vars  :: Parser Token [Id]
vars
 = alts
 [
   do  only Kvars
       v     <- idents
       return   $ v
 ]
 
block :: Parser Token Block
block
 = alts
 [
   do  only KBraceBra
       statements  <- stmts
       only KBraceKet
       return   $ Block statements
 ]
 
stmts :: Parser Token [Stmt]
stmts
 = alts
 [
   do  s1       <- stmt
       s2       <- stmts   
       return   $ s1 : s2
 , do  return  $ []
 ] 

stmt  :: Parser Token Stmt
stmt
 = alts
 [
   do  only Kif
       i        <- ident
       only Kthen
       bIf      <- block
       only Kelse
       bElse    <- block
       return   $ SIfElse i bIf bElse
 , do  only Kif
       i        <- ident
       only Kthen
       b        <- block
       return   $ SIf i b
 , do  i        <- ident
       only KEquals
       e        <- expr
       only KSemi
       return   $ SAssign i e
 , do  only Kreturn
       i        <- ident
       only KSemi
       return   $ SReturn i
 ]

-- | Parse an expression.
expr  :: Parser Token Exp
expr
 = alts 
 [
   do   only KRoundBra
        e1      <- expr
        o       <- oper
        e2      <- expr
        only KRoundKet
        return  $ XOp o e1 e2
 , do
        i       <- ident
        a       <- args
        return  $ XApp i a
       -- number
 , do   n       <- num
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

