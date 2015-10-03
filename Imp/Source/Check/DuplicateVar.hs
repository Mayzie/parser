
module Imp.Source.Check.DuplicateVar where
import Imp.Source.Check.Error
import Imp.Source.Exp


-- | Check that the program contains no duplicate variable names.
-- ToDo
checkDuplicateVar :: Program -> [Error]
checkDuplicateVar (Program funs)
 = []
