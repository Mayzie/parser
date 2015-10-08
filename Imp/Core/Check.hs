
module Imp.Core.Check where
import Data.List


-- | Check the debugging String for any Errors
checkProgram :: String -> String
checkProgram str
        = if isInfixOf "Error" str
             then checkInvalidArgs      str
               ++ checkUndefinedRegOrId str
               ++ checkBlockEndReached  str
             else str

-- | Check for ErrorInvalidArgs
checkInvalidArgs :: String -> String
checkInvalidArgs str
        = if isInfixOf "ErrorInvalidArgs" str
             then "ErrorInvalidArgs"
             else ""


-- | Check for ErrorUndefinedReg
checkUndefinedRegOrId :: String -> String
checkUndefinedRegOrId str
        = if isInfixOf "ErrorUndefinedRegOrId" str
             then "ErrorUndefinedRegOrId"
             else ""


-- | Check for ErrorBlockEndReached
checkBlockEndReached :: String -> String
checkBlockEndReached str
        = if isInfixOf "ErrorBlockEndReached" str
             then "ErrorBlockEndReached"
             else ""
