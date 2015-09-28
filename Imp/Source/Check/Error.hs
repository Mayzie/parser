
module Imp.Source.Check.Error where


-- | Problems we might find in the input program.
data Error
        = ErrorNoMain


-- | Pretty print an error.
prettyError :: Error -> String
prettyError err
 = case err of
        ErrorNoMain     
         -> "No main function defined."
