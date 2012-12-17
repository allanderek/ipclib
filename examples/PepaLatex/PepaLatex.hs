{-|
    This is a simple example use of the Pepa library.
-}
module Main
   ( main )
where

{- Standard library modules imported -}
import System.Console.GetOpt
   ( OptDescr  ( .. ) )
import System.Environment
   ( getArgs )
import Text.ParserCombinators.Parsec
   ( ParseError )
{- External Library modules imported -}
import Language.Latex.Print
   ( printDocument )
{- Local modules imported -}
import Ipc.Cli
   ( Cli               ( .. )
   , CliOpt
   , toCli
   , baseCliOptions
   , getProcessRenames
   , getRateRenames
   )
import Language.Pepa.Syntax
   ( ParsedModel ) 
import Language.Pepa.Parser
   ( parsePepaFile )
import Language.Pepa.Latex
   ( modelToLatex )
import Language.Pepa.Transform.Replace
   ( ProcessRenameMap
   , replaceComponentNames
   , RateRenameMap
   , replaceRateNames
   )
{- End of module imports -}


main :: IO ()
main = getArgs >>= processArgs


processArgs :: [ String ] -> IO () 
processArgs cliArgs =
   case toCli True version "pepalatex" pepaLatexOptions cliArgs of
     CliValid options inputFiles ->
        case (getProcessRenames options, getRateRenames options) of
           (Right pRenames, Right rRenames) -> 
               mapM_ (convertFile pRenames rRenames) inputFiles
           (Left err      , _             ) -> print err
           (_             , Left err      ) -> print err
     CliInter _ _                ->   
         putStrLn "pepalatex cannot be run in interactive mode"
     CliInfo  _ _ infoString     -> 
         putStrLn infoString
     CliError  _ _ errorString   -> 
         putStrLn errorString


version :: String
version = "This is pepalatex version 0.01"

-- We do not add any options so the type parameter to 'CliOpt' is ()
pepaLatexOptions :: [ OptDescr ( CliOpt () ) ]
pepaLatexOptions = baseCliOptions

convertFile :: ProcessRenameMap -> RateRenameMap -> FilePath -> IO ()
convertFile pRenames rRenames file = 
   do contents <- readFile file
      let pResult     = parsePepaFile file contents
          showF       = printDocument . modelToLatex . renameModel
          latexString = formatParseResult showF pResult
      putStrLn latexString
   where
   renameModel :: ParsedModel -> ParsedModel
   renameModel =  (replaceRateNames rRenames) .
                  (replaceComponentNames pRenames)


formatParseResult :: (a -> String) -> Either ParseError a -> String
formatParseResult _ (Left err)     = show err
formatParseResult f (Right a)      = f a
