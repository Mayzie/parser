
module Imp.Source.Check.Error where


-- | Problems we might find in the input program.
data Error
        = ErrorNoMain
        | ErrorUndefinedFunc String
        | ErrorDuplicateFunc String
        | ErrorInvalidArgs   String Int
        | ErrorUndefinedVar  String
        | ErrorDuplicateVar  String
	| ErrorSyntax

-- | Pretty print an error.
prettyError :: Error -> String
prettyError err
 = case err of
        ErrorNoMain     
         -> "No main function defined."
        ErrorUndefinedFunc func
         -> "function '" ++ func ++ "' undefined."
        ErrorDuplicateFunc func
         -> "function '" ++ func ++ "' redefined."
        ErrorInvalidArgs func args
         -> "function '" ++ func ++ "' expects " ++ (show args) ++ " argument(s)."
        ErrorUndefinedVar var
         -> "function '" ++ var ++ "' undefined."
        ErrorDuplicateVar var
         -> "function '" ++ var ++ "' redefined."
        ErrorSyntax
         -> "Syntax Error."
        _ -> ""
