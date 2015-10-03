
module Imp.Source.Check.UndefinedVar where
import Imp.Source.Check.Error
import Imp.Source.Exp


-- | Check that the program references no undefined variables.
-- ToDo
checkUndefinedVar :: Program -> [Error]
checkUndefinedVar (Program funs)
 = []
