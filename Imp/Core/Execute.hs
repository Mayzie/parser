
module Imp.Core.Execute where
import qualified Imp.Core.Exp           as C


import Data.List


------------------------------------------------------------
-- Program related
------------------------------------------------------------


-- | Execute a program from the core language (main function)
executeProgram :: C.Program -> [Int] -> String
executeProgram (C.Program funs) args
        = do let search = searchFunction funs (C.Id "main")
             case search of
                Just fun
                    -> executeFunction funs args fun
                Nothing
                    -> "Main not found"


------------------------------------------------------------
-- Function related
------------------------------------------------------------


-- | Execute the given Function
executeFunction :: [C.Function] -> [Int] -> C.Function -> String
executeFunction funs args (C.Function _ funArgs blks)
        = do let newIdLs = zip funArgs args     -- TODO: Error checking for too few/many args
             let newEnv = (C.Env newIdLs [])
             let search = searchBlock blks 0
             case search of
                Just blk
                    -> executeBlock funs blks newEnv blk
                _
                    -> "Block 0 not found, or problems"


-- | Search for the given Function | Tested and works
searchFunction :: [C.Function] -> C.Id -> Maybe C.Function
searchFunction [] _ = Nothing
searchFunction (fun : funs) (C.Id expName)
        = case fun of
            (C.Function (C.Id actName) _ _)
                -> if (actName == expName)
                    then (Just fun)
                    else searchFunction funs (C.Id expName)


------------------------------------------------------------
-- Block related
------------------------------------------------------------


-- | Execute the given Block
executeBlock :: [C.Function] -> [C.Block] -> C.Env -> C.Block -> String
executeBlock funs blks env (C.Block _ instrs)
        = executeInstrs funs blks env instrs


-- | Search for the given Block | Tested and works
searchBlock :: [C.Block] -> Int -> Maybe C.Block
searchBlock [] _ = Nothing
searchBlock (blk : blks) expNum
        = case blk of
            (C.Block actNum _)
                -> if (actNum == expNum)
                    then (Just blk)
                    else searchBlock blks expNum


------------------------------------------------------------
-- Instruction related
------------------------------------------------------------


-- | Execute all instructions
executeInstrs :: [C.Function] -> [C.Block] -> C.Env -> [C.Instr] -> String
executeInstrs _ _ _ [] = "\n"     -- TODO: Change to send error because no more instrs
executeInstrs funs blks env (instr : instrs)
        = do let (newEnv, str) = (executeInstr funs blks env instr)
             if (isInfixOf "Returned:" str)
                 then ("\n" ++ str)
                 else "\n" ++ str ++ (executeInstrs funs blks newEnv instrs)


-- | Execute the given Instr
executeInstr :: [C.Function] -> [C.Block] -> C.Env -> C.Instr -> (C.Env, String)
executeInstr funs blks (C.Env envId envReg) instr
        = case instr of
            (C.IConst r n)
                -> do let newEnvReg = insertValue envReg (r, n)
                      let newEnv = (C.Env envId newEnvReg)
                      let str = "[ " ++ printEnv newEnv
                      (newEnv, str)
            (C.ILoad r v)
                -> do let newEnv = iLoad (C.Env envId envReg) r v
                      let str = "[ " ++ printEnv newEnv
                      (newEnv, str)
            (C.IStore v r)
                -> do let newEnv = iStore (C.Env envId envReg) v r
                      let str = "[ " ++ printEnv newEnv
                      (newEnv, str)
            (C.IArith op r1 r2 r3)
                -> do let newEnvReg = iArith envReg op r1 r2 r3
                      let newEnv = (C.Env envId newEnvReg)
                      let str = "[ " ++ printEnv newEnv
                      (newEnv, str)
            (C.IBranch r n1 n2)
                -> do let blkNum = if (searchValue envReg r) == 0 then n2 else n1
                      let str = iBranch funs blks (C.Env envId envReg) blkNum
                      ((C.Env envId envReg), str)
            (C.IReturn r)
                -> do let result = searchValue envReg r
                      let str = "Returned: " ++ (show result)
                      ((C.Env envId envReg), str)
            (C.ICall r v args)
                -> do let values = map (searchValue envReg) args
                      let (n, str) = iCall funs values v
                      let newEnvReg = insertValue envReg (r, n)
                      ((C.Env envId newEnvReg), str)


-- | Loads the given Identifier into the given Register | Tested and works
iLoad :: C.Env -> C.Reg -> C.Id -> C.Env
iLoad (C.Env idLs regLs) r v
        = do let value = searchValue idLs v
             let newRegLs = insertValue regLs (r, value)
             (C.Env idLs newRegLs)


-- | Stores the given Register into the given Identifier | Tested and works
iStore :: C.Env -> C.Id -> C.Reg -> C.Env
iStore (C.Env idLs regLs) v r
        = do let value = searchValue regLs r
             let newIdLs = insertValue idLs (v, value)
             (C.Env newIdLs regLs)


-- | Go into the relevant branch
iBranch :: [C.Function] -> [C.Block] -> C.Env -> Int -> String
iBranch funs blks env n
        = do let search = searchBlock blks n
             let str = "-- branch to block " ++ (show n)
             case search of
                 Just blk -> concat [str, executeBlock funs blks env blk]
                 Nothing -> "Block " ++ (show n) ++ " not found"


-- | Go into the relevant function
iCall :: [C.Function] -> [Int] -> C.Id -> (Int, String)
iCall funs values v@(C.Id name)
        = do let search = searchFunction funs v
             let str1 = "-- calling function " ++ name
             case search of
                 Just fun
                     -> do let str2 = executeFunction funs values fun
                           --let n = read (last (splitOn " " str2))
                           (0, concat [str1, str2])
                 Nothing -> (0, "Function " ++ name ++ " not found")


------------------------------------------------------------
-- Operation related
------------------------------------------------------------


-- | Evaluates the operation and returns the new env | Tested and works
iArith :: [(C.Reg, Int)] -> C.OpArith -> C.Reg -> C.Reg -> C.Reg -> [(C.Reg, Int)]
iArith regLs op r1 r2 r3
        = case op of
            C.OpAdd
                -> do let value = value2 + value3
                      insertValue regLs (r1, value)
            C.OpSub
                -> do let value = value2 - value3
                      insertValue regLs (r1, value)
            C.OpMul
                -> do let value = value2 * value3
                      insertValue regLs (r1, value)
            C.OpDiv
                -> do let value = div value2 value3
                      insertValue regLs (r1, value)
            C.OpLt
                -> do let value = if value2 < value3 then 1 else 0
                      insertValue regLs (r1, value)
            C.OpGt
                -> do let value = if value2 > value3 then 1 else 0
                      insertValue regLs (r1, value)
            C.OpEq
                -> do let value = if value2 == value3 then 1 else 0
                      insertValue regLs (r1, value)
          where value2 = searchValue regLs r2
                value3 = searchValue regLs r3


------------------------------------------------------------
-- Environment related (Search, Insert, and Print)
------------------------------------------------------------


-- | Combine two environments into one
--combineEnv :: C.Env -> C.Env -> C.Env
--combineEnv (C.Env r1 v1) (C.Env r2 v2)
--        = do let r3 = r1 ++ r2
--             let v3 = v1 ++ v2
--             (C.Env r3 v3)


-- | Return the value in the given Identifier/Register | Tested and works
searchValue :: Eq a => [(a, Int)] -> a -> Int
searchValue [] _ = 0
searchValue ((x, currNum) : xs) a1
        = if x == a1
            then currNum
            else searchValue xs a1


-- | Insert the value in the given Identifier/Register | Tested and works
insertValue :: Eq a => [(a, Int)] -> (a, Int) -> [(a, Int)]
insertValue [] a1 = [a1]
insertValue ((x, currNum) : xs) (a1, n)
        = if x == a1
            then [(a1, n)] ++ xs
            else [(x, currNum)] ++ insertValue xs (a1, n)


-- | Print the entire environment | Tested and works
printEnv :: C.Env -> String
printEnv (C.Env [] []) = "]"
printEnv (C.Env [] (reg : envReg)) = printReg reg ++ (printEnv (C.Env [] envReg))
printEnv (C.Env (var : envId) envReg) = printId var ++ (printEnv (C.Env envId envReg))


-- | Print a single Register | Tested and works
printReg :: (C.Reg, Int) -> String
printReg ((C.Reg r), n) = "(r" ++ (show r) ++ ", " ++ (show n) ++ "), "


-- | Print a single Identifier | Tested and works
printId :: (C.Id, Int) -> String
printId ((C.Id v), n) = "(" ++ v ++ ", " ++ (show n) ++ "), "


-- | Print the given Register or Identifier | Not working
--printRegOrId :: Eq a => (a, Int) -> String
--printRegOrId (x, n)
--        = case x of
--            (C.Reg r) -> "(r" ++ (show r) ++ ", " ++ (show n) ++ "),"
--            (C.Id v) -> "(" ++ (show v) ++ ", " ++ (show n) ++ "),"
