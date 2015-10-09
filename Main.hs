
module Main where
import qualified Imp.Source                     as S
import qualified Imp.Source.Lexer               as S
import qualified Imp.Source.Check               as S
import qualified Imp.Source.Check.Error         as S
import qualified Imp.Source.Convert             as S
import qualified Imp.Core.Execute               as C
import qualified Imp.Core.Exp                   as T -- For testing only

import qualified Data.Algorithm.Diff            as Diff
import qualified Data.Algorithm.DiffOutput      as Diff
import qualified Text.Show.Pretty               as Text
import qualified System.Environment             as System
import qualified System.Directory               as System

import Control.Monad
import Data.List


main :: IO ()
main
 = do   args     <- System.getArgs

        case args of

         -- Lex a file.
         ["-lex",   file]
          | isSuffixOf ".imp" file
          -> do str     <- readFile file
                let out = Text.ppShow $ S.lexer str
                showResult out (file ++ ".lex")

          | otherwise
          -> error $ "Cannot lex " ++ file

         -- Parse a file.
         ["-parse", file]
          | isSuffixOf ".imp" file
          -> do str     <- readFile file
                case S.programOfString str of
                 Nothing -> showResult ("Error: " ++ S.prettyError S.ErrorSyntax) (file ++ ".parse")
                 Just prog
                  -> do let out = Text.ppShow $ prog
                        showResult out (file ++ ".parse")

          | otherwise
          -> error $ "Cannot parse " ++ file

         -- Check a file.
         ["-check", file]
          | isSuffixOf ".imp" file
          -> do str     <- readFile file
                case S.programOfString str of
                 Nothing -> showResult ("Error: " ++ S.prettyError S.ErrorSyntax) (file ++ ".check")
                 Just prog
                  -> do let out = unlines
                                $ map (\err -> "Error: " ++ S.prettyError err)
                                $ S.checkProgram prog
                        showResult out (file ++ ".check")

          | otherwise
          -> error $ "Cannot check " ++ file

         -- Convert a file.
         ["-convert", file]
          | isSuffixOf ".imp" file
          -> do str     <- readFile file
                case S.programOfString str of
                 Nothing -> showResult ("Error: " ++ S.prettyError S.ErrorSyntax) (file ++ ".convert")
                 Just progSource
                  -> do let core = S.convertProgram progSource
                        let out  = Text.ppShow core
                        showResult out (file ++ ".convert")

         -- Execute a file (TODO check if correct).
         ("-execute" : (file : mainArgs))
          | isSuffixOf ".imp" file
-- If Task 2 is not done
            -> do let pro = T.Program [
                                (T.Function (T.Id "main") [(T.Id "n")] [
                                    (T.Block 0 [
                                        (T.IConst (T.Reg 1) 2),
                                        --(T.IConst (T.Reg 2) 3),
                                        --(T.IStore (T.Id "a") (T.Reg 1)),
                                        --(T.ILoad (T.Reg 2) (T.Id "a")),
                                        --(T.IArith T.OpAdd (T.Reg 3) (T.Reg 1) (T.Reg 2)),
                                        --(T.IArith T.OpSub (T.Reg 3) (T.Reg 1) (T.Reg 2)),
                                        --(T.IArith T.OpMul (T.Reg 3) (T.Reg 1) (T.Reg 2)),
                                        --(T.IArith T.OpDiv (T.Reg 3) (T.Reg 1) (T.Reg 2)),
                                        --(T.IArith T.OpLt  (T.Reg 3) (T.Reg 1) (T.Reg 2)),
                                        --(T.IArith T.OpGt  (T.Reg 3) (T.Reg 1) (T.Reg 2)),
                                        --(T.IArith T.OpEq  (T.Reg 3) (T.Reg 1) (T.Reg 2)),
                                        --(T.IBranch (T.Reg 1) 1 2),
                                        --(T.ICall (T.Reg 2) (T.Id "fac") [(T.Reg 2)]),
                                        --(T.ICall (T.Reg 2) (T.Id "fac") []),
                                        (T.IReturn (T.Reg 1))
                                    ])--,
                                    --(T.Block 1 [
                                    --    (T.IReturn (T.Reg 1))
                                    --]),
                                    --(T.Block 2 [
                                    --    (T.IReturn (T.Reg 2))
                                    --])
                                ])--,
                                --(T.Function (T.Id "fac") [(T.Id "a")] [
                                --    (T.Block 0 [
                                --        (T.IConst (T.Reg 6) 10),
                                --        (T.IReturn (T.Reg 6))
                                --    ])
                                --])
                            ]
                  let fac = T.Program [ (T.Function (T.Id "factorial") [(T.Id "n")] [ (T.Block 0 [ (T.ILoad (T.Reg 1) (T.Id "n")), (T.IConst (T.Reg 2) 0), (T.IArith T.OpEq (T.Reg 3) (T.Reg 1) (T.Reg 2)), (T.IStore (T.Id "cond") (T.Reg 3)), (T.ILoad (T.Reg 4) (T.Id "cond")), (T.IBranch (T.Reg 4) 1 2) ]), (T.Block 1 [ (T.IConst (T.Reg 5) 1), (T.IStore (T.Id "tmp") (T.Reg 5)), (T.ILoad (T.Reg 6) (T.Id "tmp")), (T.IReturn (T.Reg 6)) ]), (T.Block 2 [ (T.ILoad (T.Reg 7) (T.Id "n")), (T.IConst (T.Reg 8) 1), (T.IArith T.OpSub (T.Reg 9) (T.Reg 7) (T.Reg 8)), (T.IStore (T.Id "tmp") (T.Reg 9)), (T.ILoad (T.Reg 10) (T.Id "tmp")), (T.ICall (T.Reg 11) (T.Id "factorial") [(T.Reg 10)]), (T.ILoad (T.Reg 12) (T.Id "n")), (T.IArith T.OpMul (T.Reg 13) (T.Reg 11) (T.Reg 12)), (T.IStore (T.Id "tmp") (T.Reg 13)), (T.ILoad (T.Reg 14) (T.Id "tmp")), (T.IReturn (T.Reg 14)) ]) ]), (T.Function (T.Id "main") [(T.Id "n")] [ (T.Block 0 [ (T.ILoad (T.Reg 1) (T.Id "n")), (T.ICall (T.Reg 2) (T.Id "factorial") [(T.Reg 1)]), (T.IStore (T.Id "tmp") (T.Reg 2)), (T.ILoad (T.Reg 3) (T.Id "tmp")), (T.IReturn (T.Reg 3)) ]) ])]

                  let exec = C.executeProgram pro (map read mainArgs)
                  if isInfixOf "Error" (snd exec)
                      then showResult (snd exec) (file ++ ".execute")
                      else case (fst exec) of
                            Just a
                                -> do showResult (show a) (file ++ ".execute")
                                      -- Use this for debugging
                                      --showResult (snd exec) (file ++ ".execute")
                            Nothing
                                -> do showResult "" (file ++ ".execute")
                                      -- Use this for debugging
                                      --showResult (snd exec) (file ++ ".execute")

-- If Task 2 is done
--          -> do str     <- readFile file
--                case S.programOfString str of
--                 Nothing -> error "parse error"
--                 Just progSource
--                  -> do let core = S.convertProgram progSource
--                        -- map is needed to convert each argument to Int
--                        let exec = C.executeProgram core (map read mainArgs)
--                        showResult exec (file ++ ".execute")

         _ -> help


-- | Display command-line help.
help :: IO ()
help
 = putStr
 $ unlines
        [ "imp'n it up"
        , ""
        , "  imp -lex     <file>        Lex a file."
        , "  imp -parse   <file>        Parse a file."
        , "  imp -check   <file>        Check a file for problems."
        , "  imp -convert <file>        Convert a file from source to core."
        , "  imp -execute <file>        Execute a file to retrieve the result." ]


-- | Given an result string and the path to a file containing the expected
--   output, if the file exists then show the diff between the actual and
--   expected, otherwise just show the expected.
showResult :: String -> FilePath -> IO ()
showResult strResult fileExpected
 = do
        putStrLn $ strResult
        exists  <- System.doesFileExist fileExpected

        when exists
         $ do   strExpected <- readFile fileExpected

                when (not $ null strExpected)
                 $ do   let diff    = Diff.ppDiff
                                    $ Diff.getGroupedDiff
                                        (lines strResult)
                                        (lines strExpected)
                        if diff == "\n"
                         then putStrLn $ "\nOK"
                         else putStrLn $ "\nDIFF\n" ++ diff

