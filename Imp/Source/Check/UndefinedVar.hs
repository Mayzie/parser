
module Imp.Source.Check.UndefinedVar where
import Imp.Source.Check.Common
import Imp.Source.Check.Error
import Imp.Source.Exp

-- | Check that the program references no undefined variables.
checkUndefinedVars :: Program -> [Error]
checkUndefinedVars (Program []) = []
checkUndefinedVars (Program functions) = checkUndefinedFuncVars functions

checkUndefinedFuncVars :: [Function] -> [Error]
checkUndefinedFuncVars [] = []
checkUndefinedFuncVars (f:functions) = checkUndefinedVar f ++ checkUndefinedFuncVars functions
 
checkUndefinedVar :: Function -> [Error]
checkUndefinedVar f@(Function _ _ _ block)
 = map (ErrorUndefinedVar) (findComplement (getRefVariables block) (getFunctionVariables f))
