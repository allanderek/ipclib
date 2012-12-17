{-
-}
module Main
  ( main )
where

{- Standard Library Modules Imported -}
import System.Console.GetOpt
  ( getOpt
  , usageInfo
  , ArgOrder    ( .. )
  , OptDescr    ( .. )
  , ArgDescr    ( .. )
  )
import System.Environment
  ( getArgs
  , getProgName
  )
import System.Exit
  ( ExitCode       ( .. )
  , exitWith
  )
{- External Library Modules Imported -}
{- Local Modules Imported -}
import Eba.Eba
  ( compileFile )
{- End of Imports -}

data CliFlag =
    CliHelp
  | CliVersion
  deriving Eq


options :: [ OptDescr CliFlag ]
options =
  [ Option   "h"     [ "help" ]
    (NoArg CliHelp)
    "Print the help message to standard out and then exit"

  , Option   "v"     [ "version" ]
    (NoArg CliVersion)
    "Print out the version of this program"
  ]

helpMessage :: String -> String
helpMessage progName =
  usageInfo progName options

versionMessage :: String
versionMessage = "This is version 0.001"

-- | The main exported function
main :: IO ()
main = getArgs >>= processOptions

processOptions :: [ String ] -> IO ()
processOptions cliArgs =
  case getOpt Permute  options cliArgs of
    (flags, args, [])       -> 
      processArgs flags args
    (_flags, _args, errors) -> 
      do progName <- getProgName
         ioError $ userError (concat errors ++ helpMessage progName)

-- We assume all of the arguments are files to process
processArgs :: [ CliFlag ] -> [ String ] -> IO ()
processArgs flags files
  | elem CliHelp flags    = getProgName >>= (putStrLn . helpMessage)
  | elem CliVersion flags = putStrLn versionMessage
  | otherwise             = 
    do errorCodes <- mapM compileFile files
       if null [ i | ExitFailure i <- errorCodes ] 
          then exitWith ExitSuccess
          else exitWith $  ExitFailure 1


