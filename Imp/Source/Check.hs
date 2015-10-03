
module Imp.Source.Check where
-- Error: function '<function name>' undefined
import Imp.Source.Check.UndefinedFunc
-- Error: function '<function name>' redefined
import Imp.Source.Check.DuplicateFunc
-- Error: function '<function name>' expects <n> argument(s)
import Imp.Source.Check.InvalidArgs
-- Error: variable '<variable name>' undefined
import Imp.Source.Check.UndefinedVar
-- Error: variable '<variable name>' redefined
import Imp.Source.Check.DuplicateVar
-- Error: No main function defined
import Imp.Source.Check.Main
-- Syntax Error
import Imp.Source.Check.Syntax
import Imp.Source.Check.Error
import Imp.Source.Exp


-- | Run all the available semantic checks on a program.
checkProgram :: Program -> [Error]
checkProgram program
        = checkUndefinedFunc  program
        ++ checkDuplicateFunc program
        ++ checkInvalidArgs   program
        ++ checkUndefinedVar  program
        ++ checkDuplicateVar  program
        ++ checkMain          program
        ++ checkSyntax        program
