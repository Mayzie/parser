
module Imp.Core.Execute where
import qualified Imp.Core.Exp       as C


-- | Execute a program from the core language
executeProgram :: C.Program -> [Int] -> String
executeProgram (C.Program _) _
        = "Testing"
        -- Above is just temporary to make sure the flow works
