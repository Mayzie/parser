
module Imp.Core.Parser where
import Imp.Core.Exp
import Imp.Core.Tokens
import Imp.Parsec

-- | Whole <program>.
program  :: Parser Token Program
program
 =  do  only KRoundBra
        fs      <- funcs
        only KRoundKet
        return  $ Program fs

funcs :: Parser Token [Function]
funcs 
 = alts
 [
   do   f       <- func
        fs      <- funcs
        return  $ f : fs

 , do   return []
 ]

func :: Parser Token Function
func 
 =  do  only KRoundBra
        i       <- ident
        a       <- args
        b       <- blcks
        only KRoundKet
        return  $ Function i a b

args :: Parser Token [Id]
args 
 = alts
 [ 
   -- ROG: <arguments> ::= ( <id_list> )
   do   only KRoundBra
        is      <- idents
        only KRoundKet
        return  $ is

   -- ROG: <arguments> ::= ( empty )
 , do   only KRoundBra
        only KRoundKet
        return  []

 ]

blcks :: Parser Token [Block]
blcks
 = alts
 [
   do   b <- blck
        bs <- blcks 
        return $ b : bs

 , do   return []
 ]

blck :: Parser Token Block
blck
 = alts
 [
   do   only KRoundBra
        i      <- num
        is   <- instrs
        only KRoundBra
        return  $ Block i is
 ]

instrs :: Parser Token [Instr]
instrs 
 = alts
 [
   do   i <- inst
        is <- instrs 
        return $ i : is

 , do   return []
 ]

inst :: Parser Token Instr
inst
 = alts
 [
   do   only KRoundBra
        only (KOp OpAdd)
        r1 <- reg
        r2 <- reg
        r3 <- reg
        only KRoundKet
        return $ IArith OpAdd (Reg r1) (Reg r2) (Reg r3)

 , do   only KRoundBra
        only (KOp OpSub)
        r1 <- reg
        r2 <- reg
        r3 <- reg
        only KRoundKet
        return $ IArith OpSub (Reg r1) (Reg r2) (Reg r3)

 , do   only KRoundBra
        only (KOp OpMul)
        r1 <- reg
        r2 <- reg
        r3 <- reg
        only KRoundKet
        return $ IArith OpMul (Reg r1) (Reg r2) (Reg r3)  

 , do   only KRoundBra
        only (KOp OpDiv)
        r1 <- reg
        r2 <- reg
        r3 <- reg
        only KRoundKet
        return $ IArith OpDiv (Reg r1) (Reg r2) (Reg r3)  

 , do   only KRoundBra
        only (KOp OpLt)
        r1 <- reg
        r2 <- reg
        r3 <- reg
        only KRoundKet
        return $ IArith OpLt (Reg r1) (Reg r2) (Reg r3)  

 , do   only KRoundBra
        only (KOp OpGt)
        r1 <- reg
        r2 <- reg
        r3 <- reg
        only KRoundKet
        return $ IArith OpGt (Reg r1) (Reg r2) (Reg r3)

 , do   only KRoundBra
        only (KOp OpEq)
        r1 <- reg
        r2 <- reg
        r3 <- reg
        only KRoundKet
        return $ IArith OpEq (Reg r1) (Reg r2) (Reg r3)  

 , do   only KRoundBra
        only KBr
        r1 <- reg
        n1 <- num
        n2 <- num
        only KRoundKet
        return $ IBranch (Reg r1) n1 n2  

 , do   only KRoundBra
        only KRet
        r1 <- reg
        only KRoundKet
        return $ IReturn (Reg r1) 

 , do   only KRoundBra
        only KCall
        r1 <- reg
        i1 <- ident
        rs <- regs
        only KRoundKet
        return $ ICall (Reg r1) i1 rs
 , do   only KRoundBra
        only KLc
        r <- reg
        n <- num
        only KRoundKet
        return $ IConst (Reg r) n
 , do   only  KRoundBra
        only KLd
        r <- reg
        i <- ident
        only KRoundKet
        return $ ILoad (Reg r) i
 , do   only KRoundBra
        only KSt
        i <- ident
        r <- reg
        only KRoundKet
        return $ IStore i (Reg r)
 ]

-- | Parse a number. <NUM>
num   :: Parser Token Int
num = from takeNum

reg   :: Parser Token Int
reg = from takeReg


-- | Parse an identifier. <id_list> ::= <ID>
ident :: Parser Token Id
ident
 = do   i       <- from takeId
        return  $ Id i 

regs :: Parser Token [Reg]
regs
 = alts
 [ do   i       <- reg
        is      <- regs
        return  $ (Reg i) : is

 , do   i       <- reg
        return  [(Reg i)]
 ]

-- | Parse arguments separated by commas. <id_list> ::= <ID>, <id_list>
idents :: Parser Token [Id]
idents
 = alts
 [ do   i       <- ident
        is      <- idents
        return  $ i : is

 , do   i       <- ident
        return  [i]
 ]