
module Imp.Source.Exp where


-- | Source Programs.
data Program
        = Program [Function]
        deriving Show


-- | Source Functions.
data Function
        = Function Id [Id] [Id] Block
        deriving Show


-- | Source Blocks.
data Block
        = Block [Stmt]
        deriving Show


-- | Source Statements.
data Stmt
        = SAssign Id  Exp
        | SIf     Id  Block
        | SIfElse Id  Block Block
        | SReturn Id
        deriving Show


-- | Source Expressions.
data Exp
        = XNum  Int
        | XId   Id
        | XApp  Id [Id]
        | XOp   Op Exp Exp
        deriving Show


-- | Source Identifiers.
data Id
        = Id    String
        deriving (Show, Eq)


-- | Source Operators.
data Op
        = OpAdd
        | OpSub
        | OpMul
        | OpDiv
        | OpLt
        | OpGt
        | OpEq
        deriving (Show, Eq)
