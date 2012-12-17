#!/usr/bin/env runhaskell

{- Imported Standard Libraries -}
import Data.List        ( isSuffixOf, partition )
import Control.Monad    ( mapM, filterM )
import System.Directory ( getCurrentDirectory
			, getDirectoryContents
			, doesDirectoryExist
			)
import System.Cmd       ( system )
import System.Exit      ( ExitCode ( .. ) )


{-
  This file implements the simple automation of the running
  of the tests and reporting of the results.
-}

main :: IO ()
main = do directories <- findTestDirectories
          dirReports  <- mapM runTestsInDir directories
          let reportStrings  = map summaryToString dirReports
          let overallSummary = summaryToNumbers $ addSummaries dirReports
          putStrLn $ unlines reportStrings
          putStrLn "------------------"
          putStrLn "The overall summary is"
          putStrLn overallSummary


data Summary = Summary { sumTestDir       :: FilePath
                        , goodSucceeded    :: Int
                        , goodFailed       :: Int
                        , badSucceeded     :: Int
                        , badFailed        :: Int
                        , goodErrorReports :: [ String ]
                        , badErrorReports  :: [ String ]
                        } 

summaryToNumbers :: Summary -> String
summaryToNumbers summary =
    unlines   [ "    The good tests: "
              , "        " ++ (show $ goodSucceeded summary) 
                           ++ " tests passed"
              , "        " ++ (show $ goodFailed summary) 
                           ++ " tests failed"
              , "    The bad tests: "
              , "        " ++ (show $ badSucceeded summary)
                           ++ " tests passed"
              , "        " ++ (show $ badFailed summary)
                           ++ " tests failed"
              ]


addSummaries :: [ Summary ] -> Summary
addSummaries = foldl1 addTwoSummaries

addTwoSummaries :: Summary -> Summary -> Summary
addTwoSummaries s1 s2 =
    Summary { sumTestDir    = ""
            , 
goodSucceeded = goodSucceeded s1 +
                              goodSucceeded s2
            , goodFailed    = goodFailed s1 +
                              goodFailed s2
            , badSucceeded  = badSucceeded s1 +
                              badSucceeded s2
            , badFailed     = badFailed s1 +
                              badFailed s2
            , goodErrorReports = goodErrorReports s1 ++
                                 goodErrorReports s2
            , badErrorReports  = badErrorReports s1 ++
                                 badErrorReports s2
            }


summaryToString :: Summary -> String
summaryToString summary =
    unlines $ [ "Running tests in the directory " ++ 
                sumTestDir summary
              , "    The good tests: "
              , "        " ++ (show $ goodSucceeded summary) 
                           ++ " tests passed"
              , "        " ++ (show $ goodFailed summary) 
                           ++ " tests failed"
              , "    The bad tests: "
              , "        " ++ (show $ badSucceeded summary)
                           ++ " tests passed"
              , "        " ++ (show $ badFailed summary)
                           ++ " tests failed"
              ] ++ goodErrorReports summary
                ++ badErrorReports  summary


runTestsInDir :: FilePath -> IO Summary
runTestsInDir dirName =
    do (goodS, goodF) <- runGoodTests dirName
       (badS, badF)   <- runBadTests dirName
       let gErrorReports = map makeErrorString goodF
           bErrorReports = map makeErrorString badF
           summary       = Summary { sumTestDir       = dirName
                                   , goodSucceeded    = length goodS
                                   , goodFailed       = length goodF
                                   , badSucceeded     = length badS
                                   , badFailed        = length badF
                                   , goodErrorReports = gErrorReports
                                   , badErrorReports  = bErrorReports
                                   }
       return summary
    where 
    makeErrorString :: FilePath -> String
    makeErrorString f = unwords [ "The test file"
                                , f
                                , "FAILED"
                                ]

{-
  Essentially returns all the directories under the tests directory,
  except that we make sure to remove from the list the [-misc-]
  directory has this has different test characteristics.
-}
findTestDirectories :: IO [ FilePath ]
findTestDirectories = 
    do curDir   <- getCurrentDirectory
       testDirs <- getDirectoryContents $ curDir ++ "/tests"
       filterM isTestDir $ map ("tests/" ++) testDirs
    where isTestDir :: FilePath -> IO Bool
	  isTestDir f = do isDir  <- doesDirectoryExist f
			   isDir1 <- doesDirectoryExist $ f ++ "/good"
			   isDir2 <- doesDirectoryExist $ f ++ "/bad"
			   return $ isDir && (f /= "misc")
				          && isDir1
                                          && isDir2
	                       



{- The results of running some tests -}
type TestResults = ( [ FilePath ] -- Those that succeeded
		   , [ FilePath ] -- Those that failed
		   )

{- For the bad results we of course want them to fail rather than succeed -}
makeBadResults :: TestResults -> TestResults
makeBadResults (succeeded, failed) = (failed, succeeded)


ipcCommand :: String -> IO String
ipcCommand arg = do executable <- ipcExecutable
		    return $ unwords [ executable
				     , arg
				     ]

{-
  Run the bad tests
-}
runTests :: FilePath -> IO TestResults
runTests dirName =
    do tests    <- returnPepaFiles dirName
       let testFiles = map ( dirName ++ ) tests
           runTest fileName  = do command <- ipcCommand fileName
                                  ec      <- system command
                                  return (ec, fileName)
       results  <- mapM runTest testFiles
       let pResults  = partition isSuccess results
           isSuccess :: (ExitCode, FilePath) -> Bool
           isSuccess (ExitSuccess, _) = True
           isSuccess _                = False
           iResults  = ( map snd $ fst pResults
                       , map snd $ snd pResults)
       return iResults
       
runBadTests :: FilePath -> IO TestResults
runBadTests dirName = do results <- runTests (dirName ++ "/bad/")
			 return $ makeBadResults results

runGoodTests :: FilePath -> IO TestResults
runGoodTests dirName = runTests $ dirName ++ "/good/"


{-
  For each test directory we want to return all the pepa files
  within the good and bad sub-directories, this function returns
  all the [-.pepa-] files within a given directory.
-}
returnPepaFiles :: FilePath -> IO [ FilePath ]
returnPepaFiles dirName =
    do dirExists <- doesDirectoryExist dirName
       contents  <- getDirectoryContents dirName
       let pepaFiles 
	       | dirExists = filter isPepaFile contents
	       | otherwise = []
       return pepaFiles
    where isPepaFile :: FilePath -> Bool
	  isPepaFile = isSuffixOf ".pepa"

{-
  Annoyingly there was a change in the cabal build system which means
  my ghc 6.4 version at work expects to see the ipc executable at
  [-../build/dist/ipc-]
  whereas my ghc 6.4.2 version at home expects it to be at
  [-../build/dist/ipc/ipc-].
  Hence this is a little definition to attempt to find out where it
  is.
-}
ipcExecutable :: IO String
ipcExecutable = do dirExists <- doesDirectoryExist "./dist/build/pepacheck/"
		   let executable
			   | dirExists = "./dist/build/pepacheck/pepacheck"
			   | otherwise = "./dist/build/pepacheck"
		   return executable
