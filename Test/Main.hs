{-
-}
module Main
  ( main )
where

{- Standard Library Modules Imported -}
import System.Cmd
  ( system )
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
{- External Library Modules Imported -}
{- Local Modules Imported -}
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

versionMessage :: String -> String
versionMessage progName = 
  progName ++ ": This is version 0.001"

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
processArgs flags _files
  | elem CliHelp flags    = getProgName >>= (putStrLn . helpMessage)
  | elem CliVersion flags = getProgName >>= (putStrLn . versionMessage)
  | otherwise             = do performTests
                               return ()


performTests :: IO ()
performTests =
  do passageResults <- performPassageTests
     putStrLn $ hprintPassageResults passageResults
     performInternalResults

{- NOTE: these passage tests I could easily translate into HUnit tests
   whereby the test is simply that it produces the correct files
-}
type PassageResults = [ PassageResult ]
data PassageResult = PassageResult { commandFailed :: Bool
                                   , cdfAgrees :: Bool
                                   , pdfAgrees :: Bool
                                   }

hprintPassageResults :: PassageResults -> String
hprintPassageResults results =
  unlines [ -- unlines $ map hprintPassageResult results
            printNumber "Number of tests:             " noTests
          , printNumber "Number of failed commands:   " noFailed
          , printNumber "Number of cdf disagreements: " noCDFDisagrees
          , printNumber "Number of pdf disagreements: " noPDFDisagrees
          ]
  where
  noTests        = length results
  noFailed       = length $ filter commandFailed results
  noCDFDisagrees = length $ filter (not . cdfAgrees) results
  noPDFDisagrees = length $ filter (not . pdfAgrees) results

  printNumber :: String -> Int -> String
  printNumber s i = s ++ (show i)


{- Wish to descend into the passage directory and then perform
   all the tests there, however let's just get one test working first
   We will assume that each test in this directory does produce a 
   similarly named -cdf and -pdf file.
-}
performPassageTests :: IO PassageResults
performPassageTests =
  do _exitCode <- runIpc
     okay      <- compareFiles "Test/passage/test01-cdf.csv" 
                               "Test/passage/results/test01-cdf.csv" 
     return [ PassageResult { commandFailed = False
                            , cdfAgrees     = okay
                            , pdfAgrees     = True
                            }
            ]
  where
  -- This should be passed in as an argument maybe
  ipcTested = "./dist/build/ipc/ipc"
  runIpc    = system $ unwords [ ipcTested, "Test/passage/test01.ptree" ]
     

{-
  Very rough and bare bones attempt to compare to files.
  Of course we really need to be a bit more lenient with numbers.
-}
compareFiles :: FilePath -> FilePath -> IO Bool
compareFiles f1 f2 =
  do contents1 <- readFile f1
     contents2 <- readFile f2
     return (contents1 == contents2)
  

{-
  The internal results will try to do it the way I initially thought
  of, that is to have the ptree file and result described here and we
  just compare the evaluated ptree results to the one described here.
  Downside is that we may need to write a parser for ptree results.
  I think in the end both styles of tests are a good thing.
-}
performInternalResults :: IO ()
performInternalResults = return ()