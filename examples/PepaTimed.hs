{-|
    This is a simple example use of the Pepa library.
    Given a pepa model file we produce an equivalent
    timed system view of the model.
-}

module Main
    ( main )
where

{- Standard library modules imported -}
import System.Environment
     ( getArgs )
import Text.ParserCombinators.Parsec 
    ( ParseError )
{- External Library modules imported -}
{- Local modules imported -}
import Language.Pepa.Syntax
    ( ParsedModel ) 
import Language.Pepa.Parser
    ( parsePepaFile )
import Language.Pepa.Print
    ( hPrintPepaModel )
import Language.Pepa.Ode.TimedSystemEquation
    ( TimedSystem
    , toTimedSystem
    , formatTimedSystem
    )
{- End of module imports -}


main :: IO ()
main = getArgs >>=  processArgs
   
-- Very simplestic options parsing there is no need to go to the
-- length of using 'System.GetOpt' since so far we only have one option.
-- In the future probably the whole library will have a common set of
-- command-line options and we can use that function to parse them.       
processArgs :: [ String ] -> IO () 
processArgs []                          = putStrLn "No input files to analyse"
processArgs ("--print-pepa" : files)    = mapM_ (convertFile True)  files
processArgs files                       = mapM_ (convertFile False) files

convertFile :: Bool -> FilePath -> IO ()
convertFile printPepa file = 
    do contents <- readFile file
       let pModel  = parsePepaFile file contents
           pResult = interpretParseResult pModel
           output  = formatResult printPepa pResult
       putStrLn output

formatResult :: Bool -> Either ParseError (ParsedModel, TimedSystem) -> String
formatResult _      (Left err)                  = show err
formatResult False (Right (_, timedSystem))     =
    formatTimedSystem timedSystem
formatResult True (Right (pModel, timedSystem)) =
    unlines [ "---- The Pepa Model ----"
            , hPrintPepaModel pModel
            , "---- The Timed System ----"
            , formatTimedSystem timedSystem
            ]

interpretParseResult :: Either ParseError ParsedModel
                     -> Either ParseError (ParsedModel, TimedSystem)
interpretParseResult (Left err)     = Left err
interpretParseResult (Right model)  = Right (model, toTimedSystem model)

