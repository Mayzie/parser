
module Imp.Core.Execute where
import qualified Imp.Core.Exp           as C


import Imp.Core.Check


------------------------------------------------------------
-- Program related
------------------------------------------------------------


-- | Execute a program from the core language (main function)
executeProgram :: C.Program -> [Int] -> (Maybe Int, String)
executeProgram (C.Program funs) args
        = do let search = searchFunction funs (C.Id "main")
             case search of
                Just fun
                    -> do let (result, str) = executeFunction funs args fun
                          (result, checkProgram str)
                Nothing
                    -> (Nothing, "Main not found")


------------------------------------------------------------
-- Function related
------------------------------------------------------------


-- | Execute the given Function
executeFunction :: [C.Function] -> [Int] -> C.Function -> (Maybe Int, String)
executeFunction funs args (C.Function _ funArgs blks)
        = do let newIdLs = zip funArgs args
             -- Checks if too few/many args were given
             if (length funArgs == length args)
                 then do let newEnv = (C.Env newIdLs [])
                         let search = searchBlock blks 0
                         -- Checks if the specified Block exists
                         case search of
                             Just blk
                                 -> executeBlock funs blks newEnv blk
                             _
                                 -- TODO: Remove this
                                 -> (Nothing, "Block 0 not found, or problems")
                 else (Nothing, "ErrorInvalidArgs")


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
executeBlock :: [C.Function] -> [C.Block] -> C.Env -> C.Block -> (Maybe Int, String)
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
executeInstrs :: [C.Function] -> [C.Block] -> C.Env -> [C.Instr] -> (Maybe Int, String)
executeInstrs _ _ _ [] = (Nothing, "ErrorBlockEndReached")
executeInstrs funs blks env (instr : instrs)
        = do let (newEnv, result, str) = (executeInstr funs blks env instr)
             case result of
                 Just _
                     -> (result, ("\n" ++ str ++ "\n"))
                 Nothing
                     -> do let (newRes, str2) = executeInstrs funs blks newEnv instrs
                           (newRes, "\n" ++ str ++ str2)


-- | Execute the given Instr
executeInstr :: [C.Function] -> [C.Block] -> C.Env -> C.Instr -> (C.Env, Maybe Int, String)
executeInstr funs blks env@(C.Env envId envReg) instr
        = case instr of
            (C.IConst r n)
                -> do let newEnvReg = insertValue envReg (r, n)
                      let newEnv = (C.Env envId newEnvReg)
                      let str = "iConst  [ " ++ printEnv newEnv
                      (newEnv, Nothing, str)
            (C.ILoad r v)
                -> do let (newEnv, err) = iLoad env r v
                      let str = "iLoad   [ " ++ printEnv newEnv ++ err
                      (newEnv, Nothing, str)
            (C.IStore v r)
                -> do let (newEnv, err) = iStore env v r
                      let str = "iStore  [ " ++ printEnv newEnv ++ err
                      (newEnv, Nothing, str)
            (C.IArith op r1 r2 r3)
                -> do let (_, err2) = searchValue envReg r2
                      let (_, err3) = searchValue envReg r3
                      let newEnvReg = iArith envReg op r1 r2 r3
                      let newEnv = (C.Env envId newEnvReg)
                      let str = "iArith  [ " ++ printEnv newEnv ++ err2 ++ err3
                      (newEnv, Nothing, str)
            (C.IBranch r n1 n2)
                -> do let blkNum = if fst (searchValue envReg r) == 0 then n2 else n1
                      let (result, str) = iBranch funs blks env blkNum
                      (env, result, str ++ snd (searchValue envReg r))
            (C.IReturn r)
                -> do let (result, err) = searchValue envReg r
                      let str = "Returned: " ++ (show result)
                      (env, Just result, str ++ err)
            (C.ICall r v args)
                -> do let list = map (searchValue envReg) args
                      let values = foldr (\x -> (++ [(fst x)])) [] list
                      let err = foldr (\x -> (++ (snd x))) "" list
                      let (n, str) = iCall funs values v
                      let newEnvReg = insertValue envReg (r, n)
                      ((C.Env envId newEnvReg), Nothing, str ++ err)


-- | Loads the given Identifier into the given Register | Tested and works
iLoad :: C.Env -> C.Reg -> C.Id -> (C.Env, String)
iLoad (C.Env idLs regLs) r v
        = do let (value, err) = searchValue idLs v
             let newRegLs = insertValue regLs (r, value)
             ((C.Env idLs newRegLs), err)


-- | Stores the given Register into the given Identifier | Tested and works
iStore :: C.Env -> C.Id -> C.Reg -> (C.Env, String)
iStore (C.Env idLs regLs) v r
        = do let (value, err) = searchValue regLs r
             let newIdLs = insertValue idLs (v, value)
             ((C.Env newIdLs regLs), err)


-- | Go into the relevant branch
iBranch :: [C.Function] -> [C.Block] -> C.Env -> Int -> (Maybe Int, String)
iBranch funs blks env n
        = do let search = searchBlock blks n
             let str = "-- branch to block " ++ (show n)
             case search of
                 Just blk
                     -> do let (result, str2) = executeBlock funs blks env blk
                           (result, concat [str, str2])
                 Nothing -> (Nothing, "Block " ++ (show n) ++ " not found")


-- | Go into the relevant function
iCall :: [C.Function] -> [Int] -> C.Id -> (Int, String)
iCall funs values v@(C.Id name)
        = do let search = searchFunction funs v
             let str1 = "-- calling function " ++ name
             case search of
                 Just fun
                     -> do let (mb, str2) = executeFunction funs values fun
                           let str3 = concat [str1, str2, "-- returned from " ++ name]
                           case mb of
                               Just result -> (result, str3)
                               Nothing -> (0, str3)
                 Nothing
                     -> (-999, "Function " ++ name ++ " not found")


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
          where (value2, _) = searchValue regLs r2
                (value3, _) = searchValue regLs r3


------------------------------------------------------------
-- Environment related (Search, Insert, and Print)
------------------------------------------------------------


-- | Return the value in the given Identifier/Register | Tested and works
searchValue :: Eq a => [(a, Int)] -> a -> (Int, String)
searchValue [] _ = (0, "ErrorUndefinedRegOrId")
searchValue ((x, currNum) : xs) a1
        = if x == a1
            then (currNum, "")
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
