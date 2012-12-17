{-|
    This is a simple example use of the Pepa library.
-}

module Main
  ( main )
where

{- Standard library modules imported -}
import System.Console.GetOpt
  ( OptDescr  ( .. ) )
import System.Exit
  ( ExitCode  ( .. )
  , exitFailure
  )
{- External Library Modules Imported -}
{- Local modules imported -}
import Language.Pepa.Syntax
  ( ParsedModel ) 
import Ipc.Ipc
  ( parseAndMain
  , processPepaModel
  , CliOptions
  )
import Ipc.Cli
  ( getCliArgs
  , Cli                  ( .. )
  , CliOpt               ( .. )
  , toCli
  , baseCliOptions
  )
import Language.Pepa.MainControl
  ( MainControl )

{- End of module imports -}


main :: IO ()
main = getCliArgs >>= (processArgs . toCliIpc)

toCliIpc :: [ String ] -> Cli ()
toCliIpc = toCli pepaCheckVersion "pepacheck" pepaCheckOptions

pepaCheckOptions :: [ OptDescr ( CliOpt () ) ]
pepaCheckOptions = baseCliOptions


pepaCheckVersion :: String
pepaCheckVersion = "0.99"


-- Processing of the command-line arguments.
processArgs :: Cli () -> IO () 
processArgs (CliValid options files)    =
  do errorCodes <- mapM (processFile options) files
     if null [ i | ExitFailure i <- errorCodes ]
        then return ()
        else exitFailure
processArgs (CliInfo  _ _ infoString)   =
  do putStrLn pepaCheckBanner
     putStrLn infoString
processArgs (CliError  _ _ errorString) =
  do putStrLn pepaCheckBanner
     putStrLn errorString


pepaCheckBanner :: String
pepaCheckBanner = "This is pepacheck version " ++ pepaCheckVersion

processFile :: CliOptions a -> FilePath -> IO ExitCode
processFile options file =
  parseAndMain options file processModel postAction
  where
  processModel :: ParsedModel -> MainControl ()
  processModel model = (processPepaModel options model) >> return ()

  postAction :: () -> IO ExitCode
  postAction _ = return ExitSuccess

