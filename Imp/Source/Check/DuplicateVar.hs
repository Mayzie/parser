
module Imp.Source.Check.DuplicateVar where
import Imp.Source.Check.Common
import Imp.Source.Check.Error
import Imp.Source.Exp


-- | Checks that the program contains no duplicate variable names.
checkDuplicateVars :: Program -> [Error]
checkDuplicateVars (Program []) = []
checkDuplicateVars (Program functions) = checkDuplicateFuncVars functions

checkDuplicateFuncVars :: [Function] -> [Error]
checkDuplicateFuncVars [] = []
checkDuplicateFuncVars (f:functions) = checkDuplicateFuncVar f ++ checkDuplicateFuncVars functions

-- | Checks that an individual function contains no duplicate variable names.
checkDuplicateFuncVar :: Function -> [Error]
checkDuplicateFuncVar f = let variables = getFunctionVariables f
                          in map ErrorDuplicateVar $ findDuplicates variables
