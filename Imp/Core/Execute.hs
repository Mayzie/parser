
module Imp.Core.Execute where
import Imp.Core.Exp
import Imp.Core.Check


------------------------------------------------------------
-- Program related
------------------------------------------------------------


-- | Execute a program from the core language (main function)
executeProgram :: Program -> [Int] -> (Maybe Int, String)
executeProgram (Program funs) args
        = do let fun = searchFunction funs (Id "main")
             let (result, str) = executeFunction funs args fun
             (result, checkProgram str)


------------------------------------------------------------
-- Function related
------------------------------------------------------------


-- | Execute the given Function
executeFunction :: [Function] -> [Int] -> Function -> (Maybe Int, String)
executeFunction funs args (Function _ funArgs blks)
        = do let newIdLs = zip funArgs args
             -- Checks if too few/many args were given
             if (length funArgs == length args)
                 then do let newEnv = (Env newIdLs [])
                         let blk = searchBlock blks 0
                         executeBlock funs blks newEnv blk
                 else (Nothing, "ErrorInvalidArgs")


-- | Search for the given Function | Tested and works
searchFunction :: [Function] -> Id -> Function
searchFunction [] v = (Function v [] [])
searchFunction (fun@(Function (Id actName) _ _) : funs) v@(Id expName)
        = if (actName == expName)
            then fun
            else searchFunction funs v


------------------------------------------------------------
-- Block related
------------------------------------------------------------


-- | Execute the given Block
executeBlock :: [Function] -> [Block] -> Env -> Block -> (Maybe Int, String)
executeBlock funs blks env (Block _ instrs)
        = executeInstrs funs blks env instrs


-- | Search for the given Block | Tested and works
searchBlock :: [Block] -> Int -> Block
searchBlock [] n = (Block n [])
searchBlock (blk@(Block actNum _) : blks) expNum
        = if (actNum == expNum)
            then blk
            else searchBlock blks expNum


------------------------------------------------------------
-- Instruction related
------------------------------------------------------------


-- | Execute all Instructions
executeInstrs :: [Function] -> [Block] -> Env -> [Instr] -> (Maybe Int, String)
executeInstrs _ _ _ [] = (Nothing, "ErrorBlockEndReached")
executeInstrs funs blks env (instr : instrs)
        = do let (newEnv, result, str) = (executeInstr funs blks env instr)
             case result of
                 Just _
                     -> (result, ("\n" ++ str ++ "\n"))
                 Nothing
                     -> do let (newRes, str2) = executeInstrs funs blks newEnv instrs
                           (newRes, "\n" ++ str ++ str2)


-- | Execute the given Instruction
executeInstr :: [Function] -> [Block] -> Env -> Instr -> (Env, Maybe Int, String)
executeInstr funs blks env@(Env envId envReg) instr
        = case instr of
            (IConst r n)
                -> do let newEnvReg = insertValue envReg (r, n)
                      let newEnv = (Env envId newEnvReg)
                      let str = "iConst  [ " ++ printEnv newEnv
                      (newEnv, Nothing, str)
            (ILoad r v)
                -> do let (newEnv, err) = iLoad env r v
                      let str = "iLoad   [ " ++ printEnv newEnv ++ err
                      (newEnv, Nothing, str)
            (IStore v r)
                -> do let (newEnv, err) = iStore env v r
                      let str = "iStore  [ " ++ printEnv newEnv ++ err
                      (newEnv, Nothing, str)
            (IArith op r1 r2 r3)
                -> do let (_, err2) = searchValue envReg r2
                      let (_, err3) = searchValue envReg r3
                      let newEnvReg = iArith envReg op r1 r2 r3
                      let newEnv = (Env envId newEnvReg)
                      let str = "iArith  [ " ++ printEnv newEnv ++ err2 ++ err3
                      (newEnv, Nothing, str)
            (IBranch r n1 n2)
                -> do let blkNum = if fst (searchValue envReg r) == 0 then n2 else n1
                      let (result, str) = iBranch funs blks env blkNum
                      (env, result, str ++ snd (searchValue envReg r))
            (IReturn r)
                -> do let (result, err) = searchValue envReg r
                      let str = "Returned: " ++ (show result)
                      (env, Just result, str ++ err)
            (ICall r v args)
                -> do let list = map (searchValue envReg) args
                      let values = foldr (\x -> (++ [(fst x)])) [] list
                      let err = foldr (\x -> (++ (snd x))) "" list
                      let (n, str) = iCall funs values v
                      let newEnvReg = insertValue envReg (r, n)
                      ((Env envId newEnvReg), Nothing, str ++ err)


-- | Loads the given Identifier into the given Register | Tested and works
iLoad :: Env -> Reg -> Id -> (Env, String)
iLoad (Env idLs regLs) r v
        = do let (value, err) = searchValue idLs v
             let newRegLs = insertValue regLs (r, value)
             ((Env idLs newRegLs), err)


-- | Stores the given Register into the given Identifier | Tested and works
iStore :: Env -> Id -> Reg -> (Env, String)
iStore (Env idLs regLs) v r
        = do let (value, err) = searchValue regLs r
             let newIdLs = insertValue idLs (v, value)
             ((Env newIdLs regLs), err)


-- | Go into the relevant branch
iBranch :: [Function] -> [Block] -> Env -> Int -> (Maybe Int, String)
iBranch funs blks env n
        = do let blk = searchBlock blks n
             let str = "-- branch to block " ++ (show n)
             let (result, str2) = executeBlock funs blks env blk
             (result, concat [str, str2])


-- | Go into the relevant function
iCall :: [Function] -> [Int] -> Id -> (Int, String)
iCall funs values v@(Id name)
        = do let fun = searchFunction funs v
             let str1 = "-- calling function " ++ name
             let (mb, str2) = executeFunction funs values fun
             let str3 = concat [str1, str2, "-- returned from " ++ name]
             case mb of
                 Just result -> (result, str3)
                 Nothing -> (0, str3)


------------------------------------------------------------
-- Operation related
------------------------------------------------------------


-- | Evaluates the operation and returns the new env | Tested and works
iArith :: [(Reg, Int)] -> OpArith -> Reg -> Reg -> Reg -> [(Reg, Int)]
iArith regLs op r1 r2 r3
        = case op of
            OpAdd
                -> do let value = value2 + value3
                      insertValue regLs (r1, value)
            OpSub
                -> do let value = value2 - value3
                      insertValue regLs (r1, value)
            OpMul
                -> do let value = value2 * value3
                      insertValue regLs (r1, value)
            OpDiv
                -> do let value = div value2 value3
                      insertValue regLs (r1, value)
            OpLt
                -> do let value = if value2 < value3 then 1 else 0
                      insertValue regLs (r1, value)
            OpGt
                -> do let value = if value2 > value3 then 1 else 0
                      insertValue regLs (r1, value)
            OpEq
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
printEnv :: Env -> String
printEnv (Env [] []) = "]"
printEnv (Env [] (reg : envReg)) = printReg reg ++ (printEnv (Env [] envReg))
printEnv (Env (var : envId) envReg) = printId var ++ (printEnv (Env envId envReg))


-- | Print a single Register | Tested and works
printReg :: (Reg, Int) -> String
printReg ((Reg r), n) = "(r" ++ (show r) ++ ", " ++ (show n) ++ "), "


-- | Print a single Identifier | Tested and works
printId :: (Id, Int) -> String
printId ((Id v), n) = "(" ++ v ++ ", " ++ (show n) ++ "), "
