{-
  This is the main module of the @ipc@ compiler, the flagship
  application of the @ipclib@ library.
-}

module Main
  ( main )
where

{- Standard Library Modules Imported -}
import System.Console.GetOpt
  ( OptDescr  ( .. ) )
import System.Exit
  ( exitWith )
{- External Library Modules Imported -}
{- Local modules imported -}
import Ipc.Ipc
  ( compileFiles
  , ipcVersion
  , ipcBanner
  )
import Ipc.Cli
  ( getCliArgs
  , Cli                  ( .. )
  , CliOpt               ( .. )
  , toCli
  , baseCliOptions
  )

-- Probably temporary
import Data.List ( isSuffixOf )
import Language.Ptrees.Evaluate
  ( evaluatePtreeFile )
{- End of Module Imports -}

main :: IO ()
main = getCliArgs >>= (processArgs . toCliIpc)

toCliIpc :: [ String ] -> Cli ()
toCliIpc = toCli ipcVersion "ipc" ipcOptions

-- Processing of the command-line arguments.
processArgs :: Cli () -> IO () 
processArgs (CliValid _options [])            =
  putStrLn "Okay no input files to work on, I can be lazy and do nothing"
-- This is temporary until we sort out how ptrees and ipc will work
-- together, I think ultimately everything will go through ptrees
-- and we'll just translate ipc flags file.pepa into a ptree.
-- Also we can do a similar thing for ipcweb (and gipc).
processArgs (CliValid options [ file ])
  | isSuffixOf ".ptree" file                  =
    evaluatePtreeFile options file >>= exitWith
  | otherwise                                 =
    compileFiles options [ file ] >>= exitWith
processArgs (CliValid options files )         =
  compileFiles options files >>= exitWith
processArgs (CliError  _ _ errorString)       =
  putStrLn $ ipcBanner ++ errorString
processArgs (CliInfo   _ _ infoString)        =
  putStrLn $ ipcBanner ++ infoString


ipcOptions :: [ OptDescr ( CliOpt () ) ]
ipcOptions = baseCliOptions
