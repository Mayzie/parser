
module Imp.Source.Convert where
import qualified Imp.Source.Exp         as S
import qualified Imp.Core.Exp           as C


-- | Convert a program from the source to core languages.
convertProgram :: S.Program -> C.Program
convertProgram (S.Program _)
        = C.Program [(C.Function (C.Id "A") [(C.Id "A")] [(C.Block 1 [(C.IConst (C.Reg 1) 1)])])]
        -- Above is just temporary to avoid cabal complaining


-- | Convert a source identifier to a core identifier.
convertId :: S.Id -> C.Id
convertId (S.Id str) = C.Id str
