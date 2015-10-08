
module Imp.Source.Check.Error where
import Imp.Source.Exp


-- | Problems we might find in the input program.
data Error
        = ErrorNoMain
        | ErrorUndefinedFunc Id
        | ErrorDuplicateFunc Id
        | ErrorInvalidArgs   Id Int
        | ErrorUndefinedVar  Id
        | ErrorDuplicateVar  Id
        | ErrorSyntax

-- | Pretty print an error.
prettyError :: Error -> String
prettyError err
 = case err of
        ErrorNoMain
         -> "No main function defined."
        ErrorUndefinedFunc func
         -> "function '" ++ (show func) ++ "' undefined."
        ErrorDuplicateFunc func
         -> "function '" ++ (show func) ++ "' redefined."
        ErrorInvalidArgs func args
         -> "function '" ++ (show func) ++ "' expects " ++ (show args) ++ " argument(s)."
        ErrorUndefinedVar var
         -> "variable '" ++ (show var) ++ "' undefined."
        ErrorDuplicateVar var
         -> "variable '" ++ (show var) ++ "' redefined."
        ErrorSyntax
         -> "Syntax Error."
