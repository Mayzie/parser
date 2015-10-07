
module Imp.Source.Check.InvalidArgs where
import Imp.Source.Check.Error
import Imp.Source.Exp


-- | Check that the program contains no functions with mismatching arguments.
-- ToDo
checkInvalidArgs :: Program -> [Error]
checkInvalidArgs (Program functions)
 = []

{-
 = map (ErrorInvalidArgs) (filter (\x y -> get
 
checkInvalidFuncArrArgs :: [Function] -> [Error]
checkInvalidFuncArrArgs [] = []
checkInvalidFuncArrArgs (f:functions) = []

checkInvalidFuncArgs :: Function -> Exp -> Maybe Error
checkInvalidFuncArgs (Function _ args _ _) = if length args == length
-}
