
module Imp.Source.Convert where
import qualified Imp.Source.Exp         as S
import qualified Imp.Core.Exp           as C
--import Control.Monad.State


-- | Used to keep track of Block Number and Register Number.
data Counters
        = Counters Int Int


------------------------------------------------------------
-- Program/Function related
------------------------------------------------------------


-- | Convert a program from Source to Core.
convertProgram :: S.Program -> C.Program
convertProgram (S.Program functions)
        = (C.Program $ convertFunctions (Counters 0 1) functions)


-- | Convert all Functions from Source to Core.
convertFunctions :: Counters -> [S.Function] -> [C.Function]
convertFunctions _ [] = []
convertFunctions c (f:functions)
        = let res = convertFunction c f
          in res : (convertFunctions c (functions))


-- | Convert a Function from Source to Core.
convertFunction :: Counters -> S.Function -> C.Function
convertFunction (Counters n r) (S.Function i a _ b)
-- = let res@(rB, blk@(C.Block nB bB)) = convertBlock (Counters n r) b
--   in ((Counters nB rB), (C.Function (convertId i) (convertArgs a) blk))
 = let (_, blks) = convertBlock (Counters n r) b
   in (C.Function (convertId i) (convertArgs a) blks)


------------------------------------------------------------
-- Block related
------------------------------------------------------------


-- | Extract the contents of the Block (Core).
blockToTuple :: C.Block -> (Int, [C.Instr])
blockToTuple (C.Block n i) = (n, i)


-- | Convert a Block (Source) to a list of Blocks (Core).
convertBlock :: Counters -> S.Block -> (Int, [C.Block])
convertBlock (Counters n r) (S.Block statements)
 = let ((Counters nS rS), instructions) = convertStatements (Counters (n+1) (r+1)) statements
   in (rS+1, [(C.Block (nS + 1) instructions)])


------------------------------------------------------------
-- Statement related
------------------------------------------------------------


-- | Convert all Statements (Source) into Instructions (Core).
convertStatements :: Counters -> [S.Stmt] -> (Counters, [C.Instr])
convertStatements n [] = (n, [])
convertStatements c (s:statements)
        = (c, (snd (convertStatement c s) : snd (convertStatements c statements)))


-- | Convert a Statement (Source) into an Instruction (Core).
convertStatement :: Counters -> S.Stmt -> (Counters, C.Instr)
convertStatement (Counters n r) (S.SAssign _ _)
        = ((Counters n r), C.IConst (C.Reg r) 0) -- ToDo
convertStatement c _ = (c, (C.IReturn (C.Reg 1)))
-- TODO: Need to account for more Stmts


------------------------------------------------------------
-- Operation related
------------------------------------------------------------


-- | Convert each Operation from Source to Core.
convertOp :: S.Op -> C.OpArith
convertOp S.OpAdd = C.OpAdd
convertOp S.OpSub = C.OpSub
convertOp S.OpMul = C.OpMul
convertOp S.OpDiv = C.OpDiv
convertOp S.OpLt = C.OpLt
convertOp S.OpGt = C.OpGt
convertOp S.OpEq = C.OpEq


------------------------------------------------------------
-- Identifier/Register related
------------------------------------------------------------


-- | Convert an Identifier from Source to Core.
convertId :: S.Id -> C.Id
convertId (S.Id str) = C.Id str


-- | Convert a list of Identifiers from Source to Core.
convertArgs :: [S.Id] -> [C.Id]
convertArgs [] = []
convertArgs ((S.Id a) : args) = (C.Id a) : convertArgs args
