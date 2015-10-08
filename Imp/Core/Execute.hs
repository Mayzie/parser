
module Imp.Core.Execute where
import qualified Imp.Core.Exp           as C


-- | Execute a program from the core language (main function)
executeProgram :: C.Program -> [Int] -> String
executeProgram (C.Program funs) args
        = do let search1 = searchFunction funs (C.Id "main")
             case search1 of
                Just fun
                    -> executeFunction (C.Env [] []) fun
                Nothing
                    -> "Main not found"


-- | Execute the given Function
executeFunction :: C.Env -> C.Function -> String
executeFunction env (C.Function _ _ blks)
        = do let search = searchBlock blks 0
             case search of
                Just blk
                    -> executeBlock env blk
                _
                    -> "Block 0 not found, or problems"


-- | Execute the given Block
executeBlock :: C.Env -> C.Block -> String
executeBlock env (C.Block _ instrs)
        = executeInstrs env instrs


-- | Execute all instructions
executeInstrs :: C.Env -> [C.Instr] -> String
executeInstrs _ [] = "\n" ++ "End"
executeInstrs env (instr : instrs)
        = do let (newEnv, str) = (executeInstr env instr)
             str ++ "\n" ++ (executeInstrs newEnv instrs)


-- | Execute the given Instr
executeInstr :: C.Env -> C.Instr -> (C.Env, String)
executeInstr (C.Env envId envReg) instr
        = case instr of
            (C.IConst r n)
                -> do let newEnvReg = iConst envReg r n
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
            (C.IReturn r)
                -> do let result = iReturn envReg r
                      let str = "Returned: " ++ (show result)
                      ((C.Env envId envReg), str)
            _
                -> do let str = "Unsupported Instruction"
                      ((C.Env envId envReg), str)


-- | Stores the given constant in a Register | Tested and works
iConst :: [(C.Reg, Int)] -> C.Reg -> Int -> [(C.Reg, Int)]
iConst [] r n = [(r, n)]
iConst ls r n = insertValue ls (r, n)


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


-- | Returns the value in the given register | Tested and works
iReturn :: [(C.Reg, Int)] -> C.Reg -> Int
iReturn [] _ = 0
iReturn ls r = searchValue ls r


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


-- | Combine two environments into one
--combineEnv :: C.Env -> C.Env -> C.Env
--combineEnv (C.Env r1 v1) (C.Env r2 v2)
--        = do let r3 = r1 ++ r2
--             let v3 = v1 ++ v2
--             (C.Env r3 v3)


-- | Search for the given Function | Tested and works
searchFunction :: [C.Function] -> C.Id -> Maybe C.Function
searchFunction [] _ = Nothing
searchFunction (fun : funs) (C.Id expName)
        = case fun of
            (C.Function (C.Id actName) _ _)
                -> if (actName == expName)
                    then (Just fun)
                    else searchFunction funs (C.Id expName)


-- | Search for the given Block | Tested and works
searchBlock :: [C.Block] -> Int -> Maybe C.Block
searchBlock [] _ = Nothing
searchBlock (blk : blks) expNum
        = case blk of
            (C.Block actNum _)
                -> if (actNum == expNum)
                    then (Just blk)
                    else searchBlock blks expNum


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
