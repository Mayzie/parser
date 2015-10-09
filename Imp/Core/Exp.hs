
module Imp.Core.Exp where
import Data.List


-- | Core programs.
data Program
        = Program [Function]
        --deriving Show

instance Show Program where
    show (Program funs) = "( " ++ intercalate "\n  " (map show funs)++ ")"


-- | Core Functions.
data Function
        = Function Id [Id] [Block]
        --deriving Show

instance Show Function where
    show (Function name args block) = "(" ++ show name ++ " "  ++ "(" 
                                        ++  intercalate " " (map show args) ++ ")" ++ "\n    "
                                        ++ intercalate "\n    " (map show block)  ++ ") "


-- | Core Blocks.
data Block
        = Block Int [Instr]
        --deriving Show

instance Show Block where
    show (Block num instr) = "(" ++ show num ++ "  " ++ intercalate "\n        " (map show instr) ++ " ) "


-- | Instructions.
data Instr
        = IConst        Reg Int                 -- ^ lc
        | ILoad         Reg Id                  -- ^ ld
        | IStore        Id  Reg                 -- ^ st
        | IArith        OpArith Reg Reg Reg     -- ^ arithmetic ops
        | IBranch       Reg Int Int             -- ^ br
        | IReturn       Reg                     -- ^ ret
        | ICall         Reg Id [Reg]            -- ^ call
        --deriving Show

instance Show Instr where
    show (IConst reg val) = "(lc " ++ show reg ++ " " ++ show val ++ ")"
    show (ILoad reg val) = "(ld " ++ show reg ++ " " ++  show val ++ ")"
    show (IStore i reg) = "(st " ++ show i ++ " " ++  show reg ++ ")"
    show (IArith op r1 r2 r3) = "(" ++ show op ++ " " ++  show r1 ++ " " ++  show r2 ++ " " ++  show r3 ++ ")"
    show (IBranch r b1 b2) = "(br " ++ show r ++ " " ++ show b1 ++ " " ++ show b2 ++ ")"
    show (IReturn r) = "(ret " ++ show r ++ ")"
    show (ICall f i regs) = "(call " ++ show f ++ " " ++ show i ++ " " ++ intercalate " " (map show regs) ++ ")"


-- | Arithmetic operators.
data OpArith
        = OpAdd
        | OpSub
        | OpMul
        | OpDiv
        | OpLt
        | OpGt
        | OpEq
        --deriving Show

instance Show OpArith where
    show (OpAdd) = "add"
    show (OpSub) = "sub"
    show (OpMul) = "mul"
    show (OpDiv) = "div"
    show (OpLt) = "lt"
    show (OpGt) = "gt"
    show (OpEq) = "eq"


-- | Identifiers.
data Id
        = Id String
        deriving (Eq)

instance Show Id where
    show (Id name) = name


-- | Register numbers.
data Reg
        = Reg Int
        deriving (Eq)

instance Show Reg where
    show (Reg num) = "r" ++ show num


-- | Environment of Registers and Identifiers.
data Env
        = Env [(Id, Int)] [(Reg, Int)]
        deriving (Show, Eq)
