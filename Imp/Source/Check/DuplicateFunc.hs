
module Imp.Source.Check.DuplicateFunc where
import Imp.Source.Check.Error
import Imp.Source.Exp


-- | Check that the program contains no duplicate function names.
-- ToDo
checkDuplicateFunc :: Program -> [Error]
checkDuplicateFunc (Program funs)
 = []
