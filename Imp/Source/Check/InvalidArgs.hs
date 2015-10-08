
module Imp.Source.Check.InvalidArgs where
import Imp.Source.Check.Common
import Imp.Source.Check.Error
import Imp.Source.Exp

lst :: (a, b) -> b
lst (_, b) = b

-- | Check that the program contains no functions with mismatching arguments.
-- ToDo
checkInvalidArgs :: Program -> [Error]
checkInvalidArgs (Program functions)
 = map (\x -> ErrorInvalidArgs (fst x) (lst x)) (filter(\x -> (lst x) >= 0) (checkInvalidArgs' functions (getFuncNamesFromExp (getProgramFuncCalls functions))))
   
checkInvalidArgs' :: [Function] -> [(Id, [Id])] -> [(Id, Int)]
checkInvalidArgs' [] _ = []
checkInvalidArgs' _ [] = []
checkInvalidArgs' functions (i:idents) = if not ((length $ lst i) == len)
                                         then (fst i, len):checkInvalidArgs' functions idents
                                         else checkInvalidArgs' functions idents
                                         where len = getFuncArgCount functions (fst i)
 
getFuncArgCount :: [Function] -> Id -> Int
getFuncArgCount f i = case getFuncWithId f i of
                          Just (Function _ a _ _)
                           -> length a
                          Nothing
                           -> -1

getFuncWithId :: [Function] -> Id -> Maybe Function
getFuncWithId [] _ = Nothing
getFuncWithId (f@(Function fid _ _ _):functions) i = if fid == i 
                                                      then Just f 
                                                      else getFuncWithId functions i

{-
 = map (ErrorInvalidArgs) (filter (\x y -> get
 
checkInvalidFuncArrArgs :: [Function] -> [Error]
checkInvalidFuncArrArgs [] = []
checkInvalidFuncArrArgs (f:functions) = []

checkInvalidFuncArgs :: Function -> Exp -> Maybe Error
checkInvalidFuncArgs (Function _ args _ _) = if length args == length
-}
