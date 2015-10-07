
module Imp.Source.Check.UndefinedFunc where
import Imp.Source.Check.Common
import Imp.Source.Check.Error
import Imp.Source.Exp


-- | Check that the program references no undefined functions.
checkUndefinedFuncs :: Program -> [Error]
checkUndefinedFuncs (Program functions)
 = map (ErrorUndefinedFunc) (findComplement (getFuncNamesFromExp $ getProgramFuncCalls functions) (nameOfFunctions functions))
