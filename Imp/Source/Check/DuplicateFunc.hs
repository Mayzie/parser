
module Imp.Source.Check.DuplicateFunc where
import Imp.Source.Check.Common
import Imp.Source.Check.Error
import Imp.Source.Exp

-- | Check that the program contains no duplicate function names.
checkDuplicateFunc :: Program -> [Error]
checkDuplicateFunc (Program functions)
 = let nameFuncs = nameOfFunctions functions
   in map ErrorDuplicateFunc $ findDuplicates nameFuncs
