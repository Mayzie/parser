
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


-- | Convert a program from Source to Core. | Looks good
convertProgram :: S.Program -> C.Program
convertProgram (S.Program functions)
        = (C.Program $ convertFunctions (Counters 0 1) functions)


-- | Convert all Functions from Source to Core. | Looks good
convertFunctions :: Counters -> [S.Function] -> [C.Function]
convertFunctions _ [] = []
convertFunctions c (f:functions)
        = let res = convertFunction c f
          in res : (convertFunctions c functions)


-- | Convert a Function from Source to Core. | Not sure about function variables
convertFunction :: Counters -> S.Function -> C.Function
convertFunction (Counters n r) (S.Function name a _ b)
        = let (_, blks) = convertBlock (Counters n r) b
          in (C.Function (convertId name) (convertArgs a) blks)


------------------------------------------------------------
-- Block related
------------------------------------------------------------


-- | Convert a Block (Source) to a list of Blocks (Core).
convertBlock :: Counters -> S.Block -> (Counters, [C.Block])
convertBlock c@(Counters n _) (S.Block statements)
        = let (c1, instrs, blks) = convertStatements c statements
          in (c1, ((C.Block n instrs) : blks))


-- | Extract the contents of the Block (Core).
blockToTuple :: C.Block -> (Int, [C.Instr])
blockToTuple (C.Block n i) = (n, i)


------------------------------------------------------------
-- Statement related
------------------------------------------------------------


-- | Convert all Statements (Source) into Instructions (Core).
convertStatements :: Counters -> [S.Stmt] -> (Counters, [C.Instr], [C.Block])
convertStatements n [] = (n, [], [])
convertStatements c (s:statements)
        = do let (c1, instrs1, blks1) = (convertStatement c s)
             let (c2, instrs2, blks2) = (convertStatements c1 statements)
             (c2, (instrs1 ++ instrs2), (blks1 ++ blks2))


-- | Convert a Statement (Source) into an Instruction (Core).
convertStatement :: Counters -> S.Stmt -> (Counters, [C.Instr], [C.Block])
convertStatement c@(Counters n r) stmt
        = case stmt of
              (S.SAssign var e)
                  -> do let ((Counters n1 r1), instrs) = (convertExp c e)
                        let instr = (C.IStore (convertId var) (C.Reg r1))
                        let c1 = (Counters n1 (r1+1))
                        (c1, instrs ++ [instr], [])
              (S.SReturn var)
                  -> do let loadInstr = (C.ILoad (C.Reg r) (convertId var))
                        let retInstr = (C.IReturn (C.Reg r))
                        let instrs = [loadInstr] ++ [retInstr]
                        let c1 = (Counters n (r+1))
                        (c1, instrs, [])
              (S.SIf var blk)
                  -> do let (c1@(Counters n1 r1), blks) = convertBlock (Counters (n+1) (r+1)) blk
                        let loadInstr = [(C.ILoad (C.Reg r) (convertId var))]
                        let branchInstr = [(C.IBranch (C.Reg r) (n+1) (n+1))]
                        (c1, loadInstr ++ branchInstr, blks)
              _
                  -> do let instrs = [(C.IReturn (C.Reg r))]
                        let c1 = (Counters n (r+1))
                        (c1, instrs, [])
-- TODO: Need to account for more Stmts


-- | Convert an Expression (Source) to Instruction (Core).
convertExp :: Counters -> S.Exp -> (Counters, [C.Instr])
convertExp c@(Counters n r) e
        = case e of
              (S.XNum x)
                  -> (c, [(C.IConst (C.Reg r) x)])
              (S.XId v)
                  -> (c, [(C.ILoad (C.Reg r) (convertId v))])
              (S.XApp name args)
                  -> do let c1 = (Counters n r)
                        let instr = (C.ICall (C.Reg r) (convertId name) (convertArgsReg args n))
                        (c1, [instr])
              (S.XOp op e1 e2)
                  -> do let ((Counters _ r1), instrs1) = (convertExp c e1)
                        let c1 = (Counters n (r1+1))
                        let ((Counters _ r2), instrs2) = (convertExp c1 e2)
                        let c2 = (Counters n (r2+1))
                        let instr = (C.IArith (convertOp op) (C.Reg (r2+1)) (C.Reg r) (C.Reg (r1+1)))
                        (c2, instrs1 ++ instrs2 ++ [instr])


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


-- | Convert a list of Identifiers (Source) to Registers (Core).
convertArgsReg :: [S.Id] -> Int -> [C.Reg]
convertArgsReg [] _ = []
convertArgsReg ((S.Id _) : args) n = (C.Reg n) : convertArgsReg args (n+1)
