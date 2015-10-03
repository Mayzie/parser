
module Imp.Source.Check.Syntax where
import Imp.Source.Check.Error
import Imp.Source.Exp


-- | Check that the program is syntactically valid.
-- ToDo
checkSyntax :: Program -> [Error]
checkSyntax (Program funs)
 = []
