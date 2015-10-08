
module Imp.Source.Convert where
import qualified Imp.Source.Exp         as S
import qualified Imp.Core.Exp           as C
import Control.Monad.State

data Counters
        = Counters Int Int  -- Block Counter, Register Counter
 
-- Gets second element from a 2-element tuple (opposite of `fst` function)
lst :: (a,b) -> b
lst (_,b) = b

-- | Convert a program from the source to core languages.
convertProgram :: S.Program -> C.Program
convertProgram (S.Program functions)
        = (C.Program $ convertFunctions (Counters 0 1) functions)

convertFunctions :: Counters -> [S.Function] -> [C.Function]
convertFunctions _ [] = []
convertFunctions (f:functions) = let res = convertFunction f
                                 in (lst res) : (convertFunctions (fst res) (functions))

convertFunction :: Counters -> S.Function -> (Counters, C.Function)
convertFunction (Counters n r) (S.Function i a _ b)
 = let res@(rB, (C.Block nB bB)) = convertBlock (Counters n r) b
   in ((Counters nB rB), (C.Function (convertId i) (convertArgs a) (convertBlock b)))
                                          
blockToTuple :: C.Block -> (Int, [Instr])
blockToTuple (C.Block n i) = (n, i)

convertArgs :: [S.Id] -> [C.Id]
convertArgs [] = []
convertArgs (S.Id (a:arguments)) = (C.Id a) : convertArgs arguments

convertBlock :: Counters -> S.Block -> (Int, [C.Block])
convertBlock (Counters n r) (S.Block statements)
 = let (Counters n r) intructions = convertStatements n statements
   in ([C.Block (n + 1) instructions])
                                       
convertStatements :: Counters -> [S.Stmt] -> (Counters, [C.Instr])
convertStatements n [] = (n, [])
convertStatements c (s:statements) = convertStatement c s : convertStatements c statements

convertStatement :: Counters -> S.Stmt -> (Counters, C.Instr)
convertStatement (Counters n r) (S.SAssign i e) = ((Counters n r), -- ToDo

-- | Convert a source identifier to a core identifier.
convertId :: S.Id -> C.Id
convertId (S.Id str) = C.Id str

convertOp :: S.Op -> C.OpArith
convertOp S.OpAdd = C.OpAdd
convertOp S.OpSub = C.OpSub
convertOp S.OpMul = C.OpMul
convertOp S.OpDiv = C.OpDiv
convertOp S.OpLt = C.OpLt
convertOp S.OpGt = C.OpGt
convertOp S.OpEq = C.OpEq
