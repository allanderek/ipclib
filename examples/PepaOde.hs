{-|
    This is a simple example use of the Pepa library.
-}

module Main ( main )
where

{- Standard library modules imported -}
import System.Environment ( getArgs )
--import System.Exit        ( exitFailure )
import Text.ParserCombinators.Parsec ( ParseError )
{- External Library modules imported -}
{- Local modules imported -}
import Language.Pepa.Syntax                ( ParsedModel ) 
import Language.Pepa.Parser                ( parsePepaFile )
import Language.Pepa.Ode.TimedSystemEquation ( toTimedSystem
                                              , formatTimedSystem
                                              )
import Language.Pepa.Ode.OdeGeneration     ( toOdeSystem
                                           , formatOdeSystem
                                           )
{- End of module imports -}


main :: IO ()
main = getArgs >>= processArgs
          
processArgs :: [ String ] -> IO () 
processArgs []       = putStrLn "No input files to analyse"
processArgs files    = mapM_ convertFile files

convertFile :: FilePath -> IO ()
convertFile file = 
    do contents <- readFile file
       let pResult        = parsePepaFile file contents
           odeModelString = interpretParseResult pResult
       putStrLn odeModelString

interpretParseResult :: Either ParseError ParsedModel -> String
interpretParseResult (Left err)     = show err
interpretParseResult (Right model)  =
    unlines [ "----  The timed system-----"
            , formatTimedSystem timedSystem
            , "---- The ode system -----" 
            , formatOdeSystem $ toOdeSystem timedSystem model
            ]
    where
    timedSystem = toTimedSystem model
