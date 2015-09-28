
module Main where
import qualified Imp.Source                     as S
import qualified Imp.Source.Lexer               as S
import qualified Imp.Source.Check               as S
import qualified Imp.Source.Check.Error         as S
import qualified Imp.Source.Convert             as S

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
                let out = Text.ppShow $ S.programOfString str
                showResult out (file ++ ".parse")

          | otherwise
          -> error $ "Cannot parse " ++ file

         -- Check a file.
         ["-check", file]
          | isSuffixOf ".imp" file
          -> do str     <- readFile file
                case S.programOfString str of
                 Nothing -> error "parse error"
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
                 Nothing -> error "parse error"
                 Just progSource
                  -> do let core = S.convertProgram progSource
                        let out  = Text.ppShow core
                        showResult out (file ++ ".convert")
                
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
        , "  imp -convert <file>        Convert a file from source to core." ]


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

