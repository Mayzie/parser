module Imp.Source.Check.Common where
import Imp.Source.Exp
import Data.List

sampleProg :: Program
sampleProg = Program
  [ Function
      (Id "test")
      [ Id "n" ]
      [ Id "a" , Id "b" , Id "a" ]
      (Block
         [ SAssign (Id "tmp") (XApp (Id "nine") [ Id "x" ])
         , SReturn (Id "x")
         ])
  , Function
      (Id "main")
      []
      [ Id "n" ]
      (Block [ SAssign (Id "ret") (XNum 0) , SReturn (Id "ret") ])
  ]
  
getFunctions :: Program -> [Function]
getFunctions (Program functions) = functions

-- | Deletes the n'th element from a list [a]
-- | Example: deleteAt 5 [1..10]
-- |          Delete's the 5th element (5) from the list [1..10]
deleteAt :: Int -> [a] -> [a]
deleteAt _ [] = []
deleteAt x ys = if x < 0 || x >= (length ys) then ys else (take x ys) ++ (drop (x + 1) ys)

-- | Returns all elements that occur more than once in a list [a]
-- | Example: findDuplicates [1,1,2,1,2,3,4,5]
-- |          Returns [1,1,2,1,2]
findDuplicates :: (Eq a) => [a] -> [a]
findDuplicates [] = []
findDuplicates xs = filter (\x -> length (x `elemIndices` xs) > 1) xs

-- | Returns all elements that occur in the second argument, but not in the first
-- | Example: findComplement [1,2,3,4,5] [1,2,3]
-- |          Returns [4,5]
findComplement :: (Eq a) => [a] -> [a] -> [a]
findComplement [] _ = []
findComplement xs [] = xs
findComplement xs ys = filter (\x -> not (x `elem` ys)) xs

-- | Get the name of a function.
nameOfFunction :: Function -> Id
nameOfFunction (Function name _ _ _) = name

-- | Gets the name of a list of functions
nameOfFunctions :: [Function] -> [Id]
nameOfFunctions [] = []
nameOfFunctions (f:functions) = nameOfFunction f : nameOfFunctions functions

getFunctionBlocks :: [Function] -> [Block]
getFunctionBlocks [] = []
getFunctionBlocks (f:functions) = getFunctionBlock f : getFunctionBlocks functions

getFunctionBlock :: Function -> Block
getFunctionBlock (Function _ _ _ b) = b

getProgramFuncCalls :: [Function] -> [Exp]
getProgramFuncCalls [] = []
getProgramFuncCalls (f:functions) = getFunctionCalls f ++ getProgramFuncCalls functions

-- | Returns all of the references to various functions made within a function
getFunctionCalls :: Function -> [Exp]
getFunctionCalls (Function _ _ _ block) = getBlockFunctionCalls $ getBlocks block

getBlockFunctionCalls :: [Block] -> [Exp]
getBlockFunctionCalls [] = []
getBlockFunctionCalls ((Block stmts):blocks) = getStmtArrFunctionCalls stmts ++ getBlockFunctionCalls blocks

getStmtArrFunctionCalls :: [Stmt] -> [Exp]
getStmtArrFunctionCalls [] = []
getStmtArrFunctionCalls (s:statements) = case getStmtFunctionCalls s of
                                              Just x
                                               -> x : getStmtArrFunctionCalls statements
                                              Nothing
                                               -> getStmtArrFunctionCalls statements

getStmtFunctionCalls :: Stmt -> Maybe Exp
getStmtFunctionCalls (SAssign _ e) = getExpFunctionCall e
getStmtFunctionCalls _ = Nothing

getExpFunctionCall :: Exp -> Maybe Exp
getExpFunctionCall ret@(XApp _ _) = Just ret
getExpFunctionCall _ = Nothing

-- | Given an Exp (which is an XApp), returns the names of all of the functions
getFuncNamesFromExp :: [Exp] -> [(Id, [Id])]
getFuncNamesFromExp [] = []
getFuncNamesFromExp (e:exps) = case getFuncNameFromExp e of
                                    Just x
                                     -> x : getFuncNamesFromExp exps
                                    Nothing
                                     -> getFuncNamesFromExp exps

getFuncNameFromExp :: Exp -> Maybe (Id, [Id])
getFuncNameFromExp (XApp name args) = Just (name, args)
getFuncNameFromExp _ = Nothing

-- | Gets all of the variables from the first argument: `a` (must be either a Function or Block type)
-- | If `a` is a list of functions, then it returns the variables in that functions environment
-- | If `a` is a list of blocks, then it returns all variables references in those blocks
getVariables :: [Either Function Block] -> [Id]
getVariables [] = []
getVariables ((Left (Function _ args vars _)):functions) = (args ++ vars) ++ getVariables functions
getVariables ((Right (Block b)):blocks) = getStmtVariables b ++ getVariables blocks

getFunctionVariables :: Function -> [Id]
getFunctionVariables (Function _ args vars b) = args ++ vars ++ getBlockVariables b

getBlockVariables :: Block -> [Id]
getBlockVariables (Block block) = getStmtVariables block

getStmtVariables :: [Stmt] -> [Id]
getStmtVariables [] = []
getStmtVariables (s:statements) = case getStmtVariable s of
                                       Just x
                                        -> x : getStmtVariables statements
                                       Nothing
                                        -> getStmtVariables statements

getStmtVariable :: Stmt -> Maybe Id
getStmtVariable (SAssign x _) = Just x
getStmtVariable _ = Nothing

-- | Returns all referenced/named variables in a block.
getRefVariables :: Block -> [Id]
getRefVariables (Block s) = getRefStmtArrVariables s

getRefStmtArrVariables :: [Stmt] -> [Id]
getRefStmtArrVariables [] = []
getRefStmtArrVariables (s:statements) = getRefStmtVariables s ++ getRefStmtArrVariables statements

getRefStmtVariables :: Stmt -> [Id]
getRefStmtVariables (SIf i b) = i : getRefVariables b
getRefStmtVariables (SIfElse i bIf bElse) = i : getRefVariables bIf ++ getRefVariables bElse
getRefStmtVariables (SReturn i) = [i]
getRefStmtVariables (SAssign i e) = i : getRefExpVariables e

getRefExpVariables :: Exp -> [Id]
getRefExpVariables (XOp _ e1 e2) = getRefExpVariables e1 ++ getRefExpVariables e2
getRefExpVariables (XId i) = [i]
--getRefExpVariables (XApp i a) = i : a
getRefExpVariables _ = []

-- | Retrieves all block's within a block
getBlocks :: Block -> [Block]
getBlocks b@(Block []) = [b]
getBlocks b@(Block s) = b : getStatementBlocks s

getStatementBlocks :: [Stmt] -> [Block]
getStatementBlocks [] = []
getStatementBlocks (s:statements) = getStatementBlocks' s ++ getStatementBlocks statements

getStatementBlocks' :: Stmt -> [Block]
getStatementBlocks' (SIf _ block) = getBlocks block
getStatementBlocks' (SIfElse _ blockIf blockElse) = getBlocks blockIf ++ getBlocks blockElse
getStatementBlocks' _ = []
