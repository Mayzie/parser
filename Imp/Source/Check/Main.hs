
module Imp.Source.Check.Main where
import Imp.Source.Check.Common
import Imp.Source.Check.Error
import Imp.Source.Exp


-- | Check that the program contains a main function.
checkMain :: Program -> [Error]
checkMain (Program funs)
 = let  hasMain = foldl (||) False 
                $ map (== (Id "main")) 
                $ map nameOfFunction funs

   in   if hasMain 
         then []
         else [ErrorNoMain]
