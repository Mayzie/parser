
module Imp.Source.Check.InvalidArgs where
import Imp.Source.Check.Error
import Imp.Source.Exp


-- | Check that the program contains no functions with mismatching arguments.
-- ToDo
checkInvalidArgs :: Program -> [Error]
checkInvalidArgs (Program funs)
 = []
