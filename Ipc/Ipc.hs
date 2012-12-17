{-# LANGUAGE PatternGuards #-}
{-|
  The main control logic for the ipc compiler.
  Also exports some helpful functions for ipclib using applications.
-}

module Ipc.Ipc
  ( compileFiles
  , parseAndMain
  , processPepaModel
  , CliOption
  , CliOptions

  , StateResult
  , getSpaceGenerationOptions
  , getStatesSpaceResult
  , getStatesSizeReport
  , getPassageTimeResult
  , getPassageEndResult
  , getPassEndNormalise
  , getSteadyResult 

  , MeasureSpecs   ( .. )
  , StartCondition ( .. )
  , qualifyMeasureSpecs
  , getSourceStates

  , ipcVersion
  , ipcBanner
 
  , probDensityCSVFileSuffix
  , cumuDistCSVFileSuffix

  , exitFromMainControl

  , quoteCommand
  , surroundInQuotes
  )
where

{- Standard library modules imported -}
import Control.Arrow
  ( second
  , (&&&)
  )
import Control.Monad
  ( liftM
  , when
  )
import Data.Char
  ( isSpace )
import Data.List
  ( nub
  , isPrefixOf
  , isSuffixOf
  )
import qualified Data.Map as Map
  ( toList
  , null
  )
import Data.Map
  ( Map )
import Data.Maybe
  ( mapMaybe
  , fromMaybe
  )
import qualified Data.Set as Set
import Data.Set
  ( Set )
import System.Cmd
  ( system )
import System.Environment
  ( getProgName )
import System.Exit
  ( ExitCode       ( .. ) )
import qualified System.Directory as Directory
import qualified System.FilePath as File
import System.FilePath
  ( dropExtension
  , addExtension
  , takeExtension
  , takeDirectory
  , combine
  )
import System.IO
  ( stderr
  , hPutStrLn
  )

{- External Library modules imported -}
{- Local modules imported -}
import Ipc.Cli
  ( CliOpt               ( .. )
  , IpcGraphOutput       ( .. )
  , getStaticAnalysis
  , getStaunch

  , getProbeSpecs
  , getMasterBool
  , getStartActions
  , getStopActions

  , getRateOptions

  , getGraphLineWidth
  , getSolutionControl
  , PostHydra            ( .. )
  , getPostHydra

  , getHydraCommands

  , getStandardOut
  )
import Ipc.DrawGraph
  ( plotSimpleGraph
  , makeSimpleGraph
  , SimpleLine         ( .. )
  , GraphOutput        ( .. )
  )
import qualified Language.Pepa.QualifiedName as Qualified
import Language.Pepa.QualifiedName
  ( ShowOrig             ( .. ) )
import qualified Language.Pepa.Rates as Rates
import Language.Pepa.Syntax
  ( ParsedModel
  , RateIdentifier
  , RateSpec
  )
import qualified Language.Pepa.FileParser as FileParser
import qualified Language.Pepa.Parser as PepaParser
import qualified Language.Pepa.Print as Print
import Language.Pepa.Print
  ( hprintPepaModel )
import qualified Language.Pepa.Transform.Simplify as Simplify
import Language.Pepa.Transform.Replace
  ( overrideRateDefinitionList )
import qualified Language.Pepa.Transform.Rules.Syntax as Rules.Syntax
import qualified Language.Pepa.Transform.Rules.Parser as Rules.Parser
import qualified Language.Pepa.Transform.Rules.Apply  as Rules.Apply
import Language.Pepa.Analysis.Analysis
  ( analyseModel
  , splitMessages
  , messagesOfReport
  , AnalysisReport        ( .. )
  , hprintAnResult
  )

import qualified Language.Pepa.Utils as Utils
{- Performance specification probes -}
import Language.Pepa.Probes.Parser
  ( probeDefParser )
import Language.Pepa.Probes.AddProbes
  ( addMasterProbe
  , addPassageProbe
  , addProbeDefs
  , ProbeTranslateFlags     ( .. )
  )
import Language.Pepa.Probes.Syntax
  ( probeLabelsList
  , ProbeDef
  )
import qualified Language.Pepa.Compile.Model as Model
import qualified Language.Pepa.Compile.States as States
import Language.Pepa.Compile.States
  ( StateSpace              ( .. )
  , StateId
  , GenerateOptions         ( .. )
  , getModelStateSpace
  , stateSpaceSize
  , removeImmediateStates
  , getSatisfyingStates
  , findTargetedStates
  , StateCondition
  , stateSpaceToDot
  )
import qualified Language.Pepa.Compile.EstimateSize as EstimateSize
import Language.Pepa.Compile.GenMatrix
  ( getGeneratorMatrix
  , SteadyState
  , solveForSteadyState
  , showSteadyResults
  , compareWithPepatoReport
  , calculateAverageReponseTime
  )
import Language.Pepa.Compile.EmbeddedMarkov
  ( solveForEmbeddedSteadyState )
import Language.Pepa.Compile.Uniformise
  ( PassageResult
  , PassageEndResult
  , pdfResult
  , cdfResult

  , uniformiseGenMatrix
  , getPassageTimes 
  , getPassageEndTimes

  , hprintPassageResult 
  , formatCsvPassageResultCdf 
  , formatCsvPassageResultPdf 
  )
import qualified Language.Pepa.Compile.Pddl as Pddl
import qualified Language.Pepa.Compile.JavaSimulator as JavaSimulator
import Language.Pepa.Compile.JavaSimulator
  ( SimulatorConfig  ( .. ) )

{- Hydra Imports -}
import Language.Pepa.Compile.Hydra
  ( hydraFlatModel
  , hydraHierModel
  , addHydraPassageSpecs
  )
import Language.Hydra.Print
  ( printDnamModelFile )
{- Prism imports -}
import Language.Pepa.Compile.Prism
  ( prismFromPepa
  , makePrismFile
  , getPrismExplictModel
  )
import Language.Prism.Print
  ( printPrismModelFile )
{- Fsp Imports -}
import Language.Pepa.Compile.Fsp
  ( pepaModelToFspModel )
import Language.Fsp.Print
  ( printFspModel )

{- Dizzy imports -}
import Language.Dizzy.Print
  ( printDizzyModelFile )

import Language.Pepa.Compile.Dizzy
  ( pepaToDizzy )

{- The main control type imports -}
import qualified Language.Pepa.MainControl as MainControl
import Language.Pepa.MainControl
  ( MainControl
  , LogInfo
  , LogEntry             ( .. )
  , valueResult
  , closeOrError
  , resultWarning
  , resultError
  , closeMainControlWith
  )
import qualified Language.Pepa.Pepato.OutputParser as Pepato
{- End of module imports -}

-- The type of command-line options. Most of the functions in here should
-- be parametric in the type of the non-standard command-line options, this
-- means that such functions can be used by any derived program.
type CliOption    = CliOpt
type CliOptions a = [ CliOption a ]


ipcVersion :: String
ipcVersion = "0.99"

ipcBanner :: String
ipcBanner = unlines $ zipLines extraBanner ipcBannerInfo

{-
  Zips two sets of strings together as two columns.
  We make each line in the first set as long as the longest.
  We also repeat a set of blank lines after the first set
  in case the first set is shorter than the second.
-}
zipLines :: [ String ] -> [ String ] -> [ String ]
zipLines l1 = 
  zipWith (++) newL1
  where
  -- We add 4 so that we will have a gap of 2 spaces between
  -- the two columns.
  len   = 2 + (maximum $ map length l1)
    
  -- Justifies each line in l1 by adding spaces to
  -- the end of each line. We also add an infinite
  -- amount of empty lines which will serve to correctly
  -- justify the second column if the second column is
  -- longer than the first.
  newL1 = map justifyLine $ l1 ++ repeat ""

  -- Justifies a line by adding spaces to the end.
  justifyLine :: String -> String
  justifyLine l = l ++ (replicate (len - length l) ' ')


ipcBannerInfo :: [ String ]
ipcBannerInfo =
  [ "** The International PEPA compiler:"
  , "ipc v" ++ ipcVersion
  , "Produced on location in/between Edinburgh, "
  , "Leeds, London, Ludlow, Lucca and Sophia-Antipolis"
  , "2002-8"
  , "Contributors:"
  , "    Jeremy Bradley, Allan Clark,"
  , "    Stephen Gilmore and Fragkiskos Gounelas"
  , "Queries or problems: a.d.clark@ed.ac.uk"
  ]

extraBanner :: [ String ]
extraBanner =
  [ "|\\      /|"
  , "| \\    / |"
  , "|  \\  /  |"
  , "|   \\/   |"
  , "|   <>   |"
  , "|   /\\   |"
  , "|  /  \\  |"
  , "| /    \\ |"
  , "|/      \\|"
  ]




{-|
  Compiles a list of files depending on the given options.
-}
compileFiles :: CliOptions a -> [ FilePath ] -> IO ExitCode
compileFiles options files
  | any isFspOption   options        = compileAll compileForFsp
  | any isDizzyOption options        = compileAll compileForDizzy
  | any isDotOption   options        = compileAll compileForDot
  | any isPddlOption  options        = compileAll compileForPddl
  | any isJavaOption  options        = compileAll compileForJavaSimulator
  | [ stage ] <- hydraStages          = compileAll $ compileForHydra stage
  | not $ null hydraStages           = failWithMessage hStagesError
  | [ stage ] <- prismStages          = compileAll $ compileForPrism stage
  | not $ null prismStages           = failWithMessage pStagesError
  | any isComparePepato options      = comparePepatoFiles
  | otherwise                        = compileAll compileModel
  where
  hydraStages  = [ s | CliHydraStage s <- options ]
  hStagesError = "You may only specify one hydra stage"

  prismStages  = [ s | CliPrism s <- options ]
  pStagesError = "You may only specify one prism stage"

  -- Currently we do not actually fail at all, this is something I'm hoping
  -- to fix eventually. For now we just output the message but obviously
  -- don't do any compiling of the model.
  -- Eventually each compilation should return some kind of indication of
  -- success, even if it is just a 'System.Exit.ExitCode'
  failWithMessage :: String -> IO ExitCode
  failWithMessage s = 
    do hPutStrLn stderr s
       return $ ExitFailure 1

  -- Use the given function to compile all the files with the given
  -- command-line options and collect up their return codes as one.
  compileAll f = Utils.collectErrorCodes (f options) files

  -- Frankly I find this stuff all horrible and needs a drastic rewrite
  comparePepatoFiles :: IO ExitCode
  comparePepatoFiles =
    do results <- mapM (compareWithPepato options) files
       putStrLn $ getOutput results
       -- Performing the comparison has been a success, if perhaps the
       -- results of the comparison are not deemed to be a success.
       return ExitSuccess
    where
    getOutput :: [ ComparisonReport ] -> String
    getOutput results = 
      unlines [ unlines $ map showComparisonReport results
              , "-------------------------"
              , "Summary"
              , displayNumber "The number of files analysed was: " 
                              numberFiles
              , displayNumber "The number of files which disagreed was: "
                              numberDis
              , displayNumber "The number of individual disagreements was: "
                              numberInd
              , displayNumber "The number of failures by ipc was: "
                              numberIpcFailed
              , displayNumber "The number of failures by pepato was: "
                              numberPepatoFailed
              , displayNumber "The number of failures by only ipc was: "
                              numberOnlyIpcFail
              , displayNumber "The number of failures by only pepato was: "
                              numberOnlyPepato
              , displayNumber ("The number of files with internal failures " ++
                               "(generally parsing pepato out) was: ")
                              numberInternalErrors
              , displayNumber "The number of common failures was: "
                              numberBothFailed
             ]
      where
      numberFiles          = length results
      numberDis            = Utils.countSatisfy ((/= 0) . comparisonDisputes) 
                                                results
      numberInd            = sum $ map comparisonDisputes results
      numberIpcFailed      = Utils.countSatisfy comparisonIpcFail results
      numberPepatoFailed   = Utils.countSatisfy comparisonPepatoFail results
      numberOnlyIpcFail    = Utils.countSatisfy onlyIpcFail results
      numberOnlyPepato     = numberPepatoFailed - numberBothFailed
      numberInternalErrors = Utils.countSatisfy hasInternalErrs results
      numberBothFailed     = numberIpcFailed - numberOnlyIpcFail

      hasInternalErrs :: ComparisonReport -> Bool
      hasInternalErrs = not . null . comparisonInternalErr

      displayNumber :: String -> Int -> String
      displayNumber s i = s ++ (show i)

      onlyIpcFail :: ComparisonReport -> Bool
      onlyIpcFail rep = (comparisonIpcFail rep) && 
                        (not $ comparisonPepatoFail rep)

 {-
  Compiling a model, by that we mean entirely by ourselves, for the
  other kinds of compilation, for example to a hydra-stage or a prism-stage
  there are appropriately defined functions for that.
  We may however end up writing out C code, compiling that and using that to
  solve our model.
-}
compileModel :: CliOptions a -> FilePath -> IO ExitCode
compileModel options file
  | ".exper" == (takeExtension file) = performExperiment options file
  | any isSteadyOption options       = 
    parseAndMain options file (getSteadyResult options) postSteadyAction
  | any isAverageResponseOpt options =
    parseAndMain options file (getAverageResponse options) postAvgRespAction
  | any isPassageEndOption options   =
    parseAndMain options file (getPassageEndResult options) postPassageEndAction
  | any isStatesSizeOption options   =
    parseAndMain options file (getStatesSpaceResult options) postSizeAction
  | any isEstimateSizeOption options =
    parseAndMain options file (getSizeEstimateResult options) postEstimateAction
  | noPassageIndications  options    =
    parseAndMain options file (getSteadyResult options) postSteadyAction
  | otherwise                        =
    parseAndMain options file (getPassageTimeResult options) postPassageAction
  where
  -- This is called after we have produced the steady state distribution
  postSteadyAction :: SteadyState -> IO ExitCode
  postSteadyAction ss =
    do putStrLn $ showSteadyResults ss
       return ExitSuccess

  -- This is called if we are merely printing out the two state space sizes
  postSizeAction :: StateResult -> IO ExitCode
  postSizeAction ss =
    do putStrLn $ getStatesSizeReport ss
       return ExitSuccess

  -- This is called if we are merely printing out the estimated size of the
  -- model.
  postEstimateAction :: (Int, Int, Int) -> IO ExitCode
  postEstimateAction (est, full, novan) = 
    do putStr $ unlines [ "The estimate was: " ++ (show est)
                        , "   The full was : " ++ (show full)
                        , "   The novan was: " ++ (show novan)
                        ]
       return ExitSuccess

  -- This is called merely for printing out the average response time.
  postAvgRespAction :: Double -> IO ExitCode
  postAvgRespAction i = print i >> return ExitSuccess

  -- This is called after we have retrieved the uniformed matrix and 
  -- calculated the passage-time results.
  -- We are simply writing the passage-time results out to the results file
  -- however we also may print them out to the screen if the user has selected
  -- '--stdout'.
  postPassageAction :: PassageResult -> IO ExitCode
  postPassageAction results
    | getStandardOut options = do putStrLn $ hprintPassageResult results
                                  writePtResultsFiles results
                                  return ExitSuccess
    | otherwise              = do writePtResultsFiles results
                                  return ExitSuccess

  -- All of the strings of CliPassageFunction, so basically this
  -- will contain "pdf" or "cdf" or both, or neither.
  functionOptions = [ s | CliPassageFunction s <- options ]

  -- write the results out to the files file_cdf.csv and/or file_pdf.csv
  -- depending on the command-line options
  writePtResultsFiles :: PassageResult -> IO ()
  writePtResultsFiles ptResults
    -- Okay changing this so that the default is to write out both the pdf
    -- and the cdf files and only one or the other if they are specified.
    -- So if both are specified then write out both results files.
    | elem "pdf" functionOptions
      && elem "cdf" functionOptions = do writePassageCsvFiles passageEnd
                                         plotGraph cumuDistTag [ cdfLine ]
                                         plotGraph probDensityTag [ pdfLine ]
    -- If only pdf is specified then write out only that.
    | elem "pdf" functionOptions    = do writePassagePdfCsvFile passageEnd
                                         plotGraph probDensityTag [ pdfLine ]
    -- if only cdf is specified then write out only that.
    | elem "cdf" functionOptions    = do writePassageCdfCsvFile passageEnd
                                         plotGraph cumuDistTag [ cdfLine ]
    -- If nothing is specified then write out both files.
    | otherwise                     = do writePassageCsvFiles passageEnd
                                         plotGraph cumuDistTag [ cdfLine ]
                                         plotGraph probDensityTag [ pdfLine ]
    where
    -- We make up a passage-end result so we can call the generic function to
    -- output the csv files. Could do the same thing for the graphs, but we
    -- would like to be able to make the name of the line "cdf" and not "-cdf"
    -- 
    -- NOTE: if we change the first part of this, then that will probably
    -- mess with the 'smc' logic to find the correct .csv files after it
    -- has run ipc over all the models.
    passageEnd = ("", ptResults)
    cdfLine = SimpleLine { simpleLineTitle  = Just "cdf"
                         , simpleLinePoints = cdfResult ptResults
                         , simpleLineWidth  = lineWidth                         
                         }
    pdfLine = SimpleLine { simpleLineTitle  = Just "pdf"
                         , simpleLinePoints = pdfResult ptResults
                         , simpleLineWidth  = lineWidth
                         }

  lineWidth = getGraphLineWidth options

  -- The function to plot the graph to a the png file.
  -- This will be called with either of cdf simple line, the
  -- pdf simple line or both.
  -- The first argument is the suffix of the graph output, this might be
  -- in a passage-end calculation something like: "success-cdf.png" which
  -- will mean we look for any option
  -- "--output-file /this/dir/my-success-cdf.png"
  -- and if none is found then we simply write it out according to changing
  -- the original pepa file to have the given suffix.
  plotGraph :: String -> [ SimpleLine ] -> IO ()
  plotGraph suffix slines
    | any isNoGraphs options  = return ()
    | any isAllGraphs options = mapM_ plotGraphOutput allGraphKinds
    | null graphOutputs       = plotGraphOutput CSV
    | otherwise               = mapM_ plotGraphOutput graphOutputs
    where
    graphOutputs = [ x | CliGraphOutput (OneGraph x) <- options ]
    graphTitle  = Just "Passage Time Results" 
    simpleGraph = makeSimpleGraph graphTitle [] slines
    isAllGraphs :: CliOpt a -> Bool
    isAllGraphs (CliGraphOutput AllGraphs) = True
    isAllGraphs _                          = False
    allGraphKinds =  [ CSV, PS ] -- [ PNG, PDF, PS, SVG, CSV ] -- Not window??

    isNoGraphs :: CliOpt a -> Bool
    isNoGraphs (CliGraphOutput NoGraphs) = True
    isNoGraphs _                         = False

    -- Just a wrapper so that we can use 'mapM_' above and work out
    -- the correct file extension
    plotGraphOutput :: GraphOutput -> IO ()
    plotGraphOutput PNG    = plotSimpleGraph PNG simpleGraph $
                             getGraphFile suffix "png" 
    plotGraphOutput PDF    = plotSimpleGraph PDF simpleGraph $
                             getGraphFile suffix "pdf" 
    plotGraphOutput PS     = plotSimpleGraph PS  simpleGraph $
                             getGraphFile suffix "eps" 
    plotGraphOutput SVG    = plotSimpleGraph SVG simpleGraph $
                             getGraphFile suffix "svg" 
    plotGraphOutput CSV    = plotSimpleGraph CSV simpleGraph $ 
                             getGraphFile suffix "csv" 
    plotGraphOutput Window = plotSimpleGraph Window simpleGraph ""

  -- Get an output file with the given suffix.
  getGraphFile :: String -> String -> FilePath
  getGraphFile suffix ext = getGenericOutputFile suffix ext options file

  -- For now we will simply plot the graph but clearly we should also
  -- write out the .csv files.
  postPassageEndAction :: PassageEndResult -> IO ExitCode
  postPassageEndAction ptEndResults =
    do plotGraph "-pdf" $ map makePdfLine ptEndResults
       plotGraph "-cdf" $ map makeCdfLine ptEndResults
       -- isAnyCSV options
       mapM_ writePassageCsvFiles ptEndResults
       return ExitSuccess
    where
    makePdfLine :: (String, PassageResult) -> SimpleLine
    makePdfLine (name, ptResults) = 
      SimpleLine { simpleLineTitle  = Just (name ++ probDensityTag)
                 , simpleLinePoints = pdfResult ptResults
                 , simpleLineWidth  = lineWidth
                 }
    makeCdfLine :: (String, PassageResult) -> SimpleLine
    makeCdfLine (name, ptResults) =  
      SimpleLine { simpleLineTitle  = Just (name ++ cumuDistTag)
                 , simpleLinePoints = cdfResult ptResults
                 , simpleLineWidth  = lineWidth
                 }

  {-
     Write out the comma-separated-files for the pdf and the cdf of
     a passage-time result. (Recall that passage-end results include
     a list of passage-time results so this is used for passage-end
     results as well).
  -}
  writePassageCsvFiles :: (String, PassageResult) -> IO ()
  writePassageCsvFiles res = 
    do writePassagePdfCsvFile res
       writePassageCdfCsvFile res

  writePassagePdfCsvFile :: (String, PassageResult) -> IO ()
  writePassagePdfCsvFile (name, pt) = -- isAnyCSV options
    writeFile pdfFile pdfLines
    where
    pdfFile   = getGenericOutputFile "" pdfName options file
    pdfName   = name ++ probDensityCSVFileSuffix
    pdfLines  = formatCsvPassageResultPdf pt

  writePassageCdfCsvFile :: (String, PassageResult) -> IO ()
  writePassageCdfCsvFile (name, pt) = -- isAnyCSV options
    writeFile cdfFile cdfLines
    where
    cdfFile   = getGenericOutputFile "" cdfName options file
    cdfName   = name ++ cumuDistCSVFileSuffix
    cdfLines  = formatCsvPassageResultCdf pt


{- Used as part of a results file name, so we will get
   something like: <model-name>probDensityTag.png for
   a graph file, and similarly for csv files.
-}
probDensityTag :: String
probDensityTag = "-pdf"

{- See probDensityTag -}
cumuDistTag :: String
cumuDistTag = "-cdf"

probDensityCSVFileSuffix :: String
probDensityCSVFileSuffix = probDensityTag ++ ".csv"

cumuDistCSVFileSuffix :: String
cumuDistCSVFileSuffix = cumuDistTag ++ ".csv"


{-|
  The data type holding the results of a comparison between the
  steady-state analysis as done by ipc and that done by pepato.
-}
data ComparisonReport =
  ComparisonReport { comparisonFile        :: FilePath
                   , comparisonIpcFail     :: Bool
                   , comparisonPepatoFail  :: Bool
                   , comparisonDisputes    :: Int
                   , comparisonInternalErr :: [ String ]
                   , comparisonErrors      :: [ String ]
                   }

showComparisonReport :: ComparisonReport -> String
showComparisonReport comp =
  unlines [ "Report for file: " ++ (comparisonFile comp)
          , "    " ++ failLine ++ " on this file"
          , unlines $ map ("   " ++) allErrors
          ] 
  where
  failLine =
    case ( comparisonIpcFail comp, comparisonPepatoFail comp
         , not $ null internals, not $ null errors) of
      (True, True, _, _)           -> "Both commands failed"
      (True, False, _, _)          -> "Ipc failed"
      (False, True, _, _)          -> "Pepato failed"
      (False, False, True, _)      -> "Internal comparison error"
      (False, False, False, True)  -> "Commands disagree on this file"
      (False, False, False, False) -> "Commands agree on this file"
  allErrors   = errors ++ internals
  errors      = comparisonErrors comp 
  internals   = comparisonInternalErr comp

compareWithPepato :: CliOptions a -> FilePath -> IO ComparisonReport
compareWithPepato options file =
     -- parse the contents of the file into a pepa model
  do parsedResult <- MainControl.runMainControlT $ 
                    FileParser.parseFile FileParser.pepaFile file
     -- steadyReusult is a MainControl value, but no within the IO monad
     let steadyResult = parsedResult >>= getSteadyResult options
     closeMainControlWith successFun errorFun steadyResult
  where
  successFun :: SteadyState -> IO ComparisonReport
  successFun = (compareSteadyWithPepato file) . Just
  -- The error fun of course we have no steady state to pass for comparison
  -- but in this case we'll still be able to run pepato to see whether or
  -- not that fails. It may be a faulty pepa file for example.
  errorFun :: String -> IO ComparisonReport
  errorFun   = (compareSteadyWithPepato file) . const Nothing

compareSteadyWithPepato :: FilePath -> Maybe SteadyState -> IO ComparisonReport
compareSteadyWithPepato filename mSteadyState =
  do -- First process the model with pepato
     (pepOutput, pepResult) <- Utils.getProcessOutput pepatoCommand
     let pepato = Pepato.parseOutput pepOutput
         result =
           case (pepato, pepResult, mSteadyState) of
             -- The pepato command failed but so did ipc
             (_, ExitFailure _, Nothing)               -> 
               ComparisonReport { comparisonFile        = filename
                                , comparisonIpcFail     = True
                                , comparisonPepatoFail  = True
                                , comparisonDisputes    = 0
                                , comparisonErrors      = []
                                , comparisonInternalErr = []
                                }
             -- The pepato command succeeded but the parsing 
             -- of the output failed and also ipc failed
             (Left _, ExitSuccess, Nothing)            -> 
               ComparisonReport { comparisonFile        = filename
                                , comparisonIpcFail     = True
                                , comparisonPepatoFail  = False
                                , comparisonDisputes    = 0
                                , comparisonErrors      = []
                                , comparisonInternalErr = [ parseFailure ]
                                }
             -- Same again, but this time ipc succeeded
             (Left _, ExitSuccess, Just _)             -> 
               ComparisonReport { comparisonFile        = filename
                                , comparisonIpcFail     = False
                                , comparisonPepatoFail  = False
                                , comparisonDisputes    = 0
                                , comparisonErrors      = [ ]
                                , comparisonInternalErr = [ parseFailure ]
                                }
             -- The pepato command failed but the ipc command succeeded
             (_, ExitFailure _, Just _)                -> 
               ComparisonReport { comparisonFile        = filename
                                , comparisonIpcFail     = False
                                , comparisonPepatoFail  = True
                                , comparisonDisputes    = 0
                                , comparisonErrors      = []
                                , comparisonInternalErr = []
                                }
             -- The pepato command completely succeeded including the parsing
             -- of the result but the ipc command failed.
             (Right _, ExitSuccess, Nothing)           -> 
               ComparisonReport { comparisonFile        = filename
                                , comparisonIpcFail     = True
                                , comparisonPepatoFail  = False
                                , comparisonDisputes    = 0
                                , comparisonErrors      = []
                                , comparisonInternalErr = []
                                }
             -- The pepato command succeeded so now we compare
             -- the two outputs
             (Right po, ExitSuccess, Just steadyState) -> 
               ComparisonReport { comparisonFile        = filename
                                , comparisonIpcFail     = False
                                , comparisonPepatoFail  = False
                                , comparisonDisputes    = length errors
                                , comparisonErrors      = errors
                                , comparisonInternalErr = []
                                }
               where
               errors = compareWithPepatoReport steadyState po
     return result
  where
  pepatoCommand = "pepato -q direct " ++ filename
  parseFailure  = "Internal failure parsing of the pepato output"





{-|
   Get the average reponse time from the parsed pepa model.
-}
getAverageResponse :: CliOptions a
                      -- ^ The command-line options
                   -> ParsedModel
                      -- ^ The pepa model to process into steady state
                   -> MainControl Double
                      -- ^ The returned pdf and cdf results
getAverageResponse options parsedModel =
     -- First of all get the state space of the model
     -- we have to choose the space with no vanishing states
  do sResult   <- getStatesSpaceResult options parsedModel
     let space      = chooseNoVanishing sResult
     -- Get the generator matrix from the state space
     -- we are happy to generate a pre-transposed generator matrix
     -- to make solving for the steady state simpler.
     genMatrix <- getGeneratorMatrix True space
     -- Solve the generator matrix to get the steady state
     -- this won't be used (and hence not calculated) if there
     -- is only one source state of the passage that we wish to
     -- compute.
     steady    <- solveForSteadyState genMatrix

     -- Get the throughput of the start actions
     -- Get the probability of being in the probe running state.
     let mSpecs       = procMeasurementSpecs $ srProcessResults sResult
         runCond      = measureRunningCond mSpecs
         startActions = measureStartActs   mSpecs
         startActs    = map Qualified.Unqualified startActions
     return $ calculateAverageReponseTime runCond startActs steady

{-
  Get the passage-time results from the parsed pepa model.
-}
getPassageTimeResult :: CliOptions a
                        -- ^ The command-line options
                     -> ParsedModel
                        -- ^ The pepa model to process into steady state
                     -> MainControl PassageResult
                        -- ^ The returned pdf and cdf results
getPassageTimeResult options parsedModel =
     -- First of all get the state space of the model
     -- we have to choose the space with no vanishing states
  do sResult   <- getStatesSpaceResult options parsedModel
     let space      = chooseNoVanishing sResult
     -- Get the generator matrix from the state space, we do not
     -- wish to obtain a pre-transposed generator matrix because we
     -- will use the matrix to calculate hop values.
     genMatrix <- getGeneratorMatrix False space
     -- Translate the generator matrix into the embedded markov chain
     -- solve the embedded markov chain for a steady-state to weight
     -- the source states of the passage.
     -- This won't be used (and hence not calculated) if there
     -- is only one source state of the passage that we wish to
     -- compute.
     steady    <-  solveForEmbeddedSteadyState genMatrix

     -- Get the source and target conditions and use them to
     -- obtain from the sets of source and target states.
     let mSpecs      = procMeasurementSpecs $ srProcessResults sResult
         targetCond  = measureStopCond  mSpecs
         sources     = getSourceStates mSpecs space
         targets     = getSatisfyingStates targetCond space
     -- Uniformise the generator matrix 
     uniformMatrix <- uniformiseGenMatrix genMatrix
     
     -- Compute the passage-time result.
     getPassageTimes times steady uniformMatrix sources targets
  where
  times        = getMeasurementTimes options


getSourceStates :: MeasureSpecs -> StateSpace -> Set StateId
getSourceStates mSpecs statespace =
  case measureStartCond mSpecs of
    StartCond c -> getSatisfyingStates c statespace
    Initial     -> Set.singleton $ MainControl.fromMainResult
                                $ States.getInitialState statespace

{-
  Get the passage-end results from the parsed pepa model.
-}
getPassageEndResult :: CliOptions a
                        -- ^ The command-line options
                     -> ParsedModel
                        -- ^ The pepa model to process into steady state
                     -> MainControl PassageEndResult
                        -- ^ The returned set of passage-end results
getPassageEndResult options parsedModel = 
     -- First of all get the state space of the model
     -- we have to choose the space with no vanishing states
  do sResult   <- getStatesSpaceResult options parsedModel
     let space      = chooseNoVanishing sResult
     -- Get the generator matrix from the state space, we do not
     -- wish to obtain a pre-transposed generator matrix because we
     -- will use the matrix to calculate hop values.
     genMatrix <- getGeneratorMatrix False space
     -- Translate the generator matrix into the embedded markov chain
     -- solve the embedded markov chain for a steady-state to weight
     -- the source states of the passage.
     -- This won't be used (and hence not calculated) if there
     -- is only one source state of the passage that we wish to
     -- compute.
     steady    <-  solveForEmbeddedSteadyState genMatrix

     -- Get the source and target conditions and use them to
     -- obtain from the sets of source and target states.
     -- First of all get the state space of the model
     -- we have to choose the space with no vanishing states
     let mSpecs      = procMeasurementSpecs $ srProcessResults sResult
         sources     = getSourceStates mSpecs space
         stopActs    = measureStopActs  mSpecs
         targetSets  = map (makeTargetSet space) stopActs

     -- Uniformise the generator matrix 
     uniformMatrix <- uniformiseGenMatrix genMatrix
     
     -- Compute the passage-end result.
     getPassageEndTimes normalise times steady uniformMatrix sources targetSets
  where
  normalise = getPassEndNormalise options
  times     = getMeasurementTimes options

  -- This is at least pretty inefficient that we do this for all the
  -- action names rather than once creating many different sets.
  -- That is 'findTargetedStates' should have type
  -- StateSpace -> [ String ] -> Map String (Set Int)
  -- if you want them all then you can simply do (Set.unions . Map.elems)
  -- alternatively we could have two functions with similar behaviours.
  makeTargetSet :: StateSpace -> String -> (String, Set StateId)
  makeTargetSet space action =
    (action, findTargetedStates [ action ] space)

getPassEndNormalise :: CliOptions a -> Bool
getPassEndNormalise options =
  null [ True | CliNoNormalise <- options ]


-- Gets the measurement times from the command-line options
-- I've kind of written this as a function even though it
-- uses the 'options' already in scope, but it would be simple
-- to refactor this out of scope in case it is needed elsewhere.
getMeasurementTimes :: CliOptions a -> [ Double ]
getMeasurementTimes options =
  getTimes startTime
  where
  -- recusively gets the times from the given time to
  -- the stopTime in increments of timeStep
  getTimes :: Double ->  [ Double ]
  getTimes d
    | d > stopTime = []
    | otherwise    = d : (getTimes $ d + timeStep)

  (startTime, stopTime, timeStep) = getMeasurementTimeSpecs options


getMeasurementTimeSpecs :: CliOptions a -> (Double, Double, Double)
getMeasurementTimeSpecs options =
  (startTime, stopTime, timeStep)
  where
  startTimes  = [ s | CliStartTime s <- options ]
  startTime
    | null startTimes = 0.5
    | otherwise       = head startTimes

  stopTimes   = [ s | CliStopTime s <- options ]
  stopTime
    | null stopTimes = 10.0
    | otherwise      = head stopTimes

  timeSteps   = [ s | CliTimeStep s <- options ]
  timeStep
    | null timeSteps = 0.1
    | otherwise      = head timeSteps



{-
  Produce the steady state distribution as a result from a parsed model.
-}
getSteadyResult :: CliOptions a
                   -- ^ The command-line options
                -> ParsedModel
                   -- ^ The pepa model to process into steady state
                -> MainControl SteadyState
                   -- ^ The returned steady state distribution
getSteadyResult options parsedModel =
     -- First of all get the state space of the model
     -- we have to choose the 
  do space     <- liftM chooseNoVanishing $ 
                        getStatesSpaceResult options parsedModel
     -- Get the generator matrix from the state space
     -- we are happy to generate a pre-transposed generator matrix
     -- to make solving for the steady state simpler.
     genMatrix <- getGeneratorMatrix True space
     -- Solve the generator matrix for the steady state distribution
     solveForSteadyState genMatrix
 

{-|
   State space generation has two main results. The first is the full
   state space generated from the model and the second is the state
   space with the vanishing states removed.
   We also return the results of processing the model as this will be
   of interest to some later analyses. For example the start and stop
   conditions of the probes.
-}
data StateResult = StateResult { srProcessResults :: ProcessedModel
                               , srFullStates     :: StateSpace
                               , srNoVanishing    :: StateSpace
                               }

 {-
  Returning the states space of the given pepa model.
  Note that this operates from a parsed in pepa model, that is
  it does all the simplifying, adding of probes etc.
-}
getStatesSpaceResult :: CliOptions a
                        -- ^ The command-line options
                     -> ParsedModel
                        -- ^ The pepa model to process into a state space
                     -> MainControl StateResult
                        -- ^ The returned result of state space generation
getStatesSpaceResult options parsedModel =
     -- Do all the pepa related processing of the pepa model
     -- obtaining a simplified pepa model with any probes added
  do processResults <- processPepaModel options parsedModel
     -- _ <- error $ hprintPepaModel $ procSimplifiedModel processResults
     -- Calculate the state space of the model, note that we have
     -- already added any probes since the probes may add to the
     -- state space.
     space          <- getModelStateSpace (getSpaceGenerationOptions options) $
                       procSimplifiedModel processResults
     -- Remove the immediate/vanishing states from the model.
     noImmedSpace   <- removeImmediateStates space
     -- Finally return both state spaces while putting the size
     -- of each into the log.
     let stateResult = StateResult { srProcessResults = processResults
                                   , srFullStates     = space
                                   , srNoVanishing    = noImmedSpace
                                   }
         logInformation = getStatesSizeReport stateResult
     valueResult stateResult stateSpaceSizeLogName logInformation


getSpaceGenerationOptions :: CliOptions a -> GenerateOptions
getSpaceGenerationOptions options =
  -- Okay so actually if it is null then probably there should
  -- be no state space limit?
  GenerateOptions { genOptsLimit          = Just limit
                  , genOptsAllowSelfLoops = any isAllowSelfLoopsOption options
                  , genOptsAllowDeadLocks = any isAllowDeadlocksOption options
                  }
  where
  limit
    | null limitOpts = 6000
    | otherwise      = head limitOpts
  limitOpts          = [ i | CliSpaceLimit i <- options ]

getSizeEstimateResult :: CliOptions a
                         -- ^ The command-line options
                      -> ParsedModel
                         -- ^ The pepa model to process into a state space
                      -> MainControl (Int, Int, Int)
                         -- ^ The returned estimated size of the model
getSizeEstimateResult options parsedModel = 
     -- Do all the pepa related processing of the pepa model
     -- obtaining a simplified pepa model with any probes added
  do pModel      <- liftM procSimplifiedModel $ 
                    processPepaModel options parsedModel     
     -- Estimate the size of the PEPA model
     estimated   <- EstimateSize.estimatePepaSize pModel
     -- For now we will also get the *actual* size(es) of the model
     stateResult <- getStatesSpaceResult options parsedModel
     return ( estimated
            , stateSpaceSize $ chooseFullStateSpace stateResult
            , stateSpaceSize $ chooseNoVanishing    stateResult
            )

-- Given two state spaces (the whole state space and the space
-- with the vanishing states removed).
-- This should go in the log.
getStatesSizeReport :: StateResult -> String
getStatesSizeReport sResult =
  unlines [ "The full state space size = " ++ (show sizeWhole)
          , "Once diminishing states have been removed = "
            ++ (show sizeNoVanish)
          ]
  where
  sizeWhole    = stateSpaceSize $ srFullStates sResult
  sizeNoVanish = stateSpaceSize $ srNoVanishing sResult



{-|
  A utility function to choose which state space to use based
  on the command-line options. This will only be used when we are
  outputing a state-space for external processing (such as outputting
  a flattened dnam model or a dot-graph file).
-}
chooseSpace :: CliOptions a -> StateResult -> StateSpace
chooseSpace options 
  | any isNoRemoveVanishing options = chooseFullStateSpace
  | otherwise                       = chooseNoVanishing

{-|
  These two utility functions should probbably be defined in
  'Language.Pepa.Compile.States'
-}
chooseFullStateSpace :: StateResult -> StateSpace
chooseFullStateSpace = srFullStates

chooseNoVanishing :: StateResult -> StateSpace
chooseNoVanishing = srNoVanishing


{-
  Experiments are generally used to complete a single graph.
  In this way the input to 'ipc' can instead be an experiment
  file which will contain in it the pepa model(s) to be
  solved.
-}
data Experiment = 
  Experiment { experGraphFile  :: FilePath
             , experGraphTitle :: String
             , experGraphLines :: [ ExperimentLine ]
             }
  deriving (Show, Read)

data ExperimentLine = 
  ExperimentLine { experPepaModel :: FilePath
                 , experProbeDef  :: String
                 , experLineTitle :: String
                 -- The idea is that if the user does not specify
                 -- these then we can generate them easily from
                 -- the title and the pepa model.
                 , experLineCdf   :: FilePath
                 , experLinePdf   :: FilePath
                 , experLineArgs  :: String
                 }
  deriving (Show, Read)


performExperiment :: CliOptions a -> FilePath -> IO ExitCode
performExperiment _options file =
  do contents <- readFile file
     ipc      <- getProgName
     runExperiment ipc $ parseExperimentFile contents
  where
  runExperiment :: String -> Experiment -> IO ExitCode
  runExperiment ipcCommand exper =
    do mapM_ runLine eLines
       plotGraph pdfGraphLines pdfGCommands pdfGraphName
       plotGraph cdfGraphLines cdfGCommands cdfGraphName
       return ExitSuccess
    where
    cdfGCommands  = [ setKeyBottomRight, setYAxisLabel "Probability" ]
    pdfGCommands  = [ {- setYAxisLabel ""-} ]
    pdfGraphName  = basename ++ "_pdf"
    cdfGraphName  = basename ++ "_cdf"

    setYAxisLabel :: String -> String
    setYAxisLabel = quoteCommand "set ylabel" 

    -- &&& makes a pair out of applying the input to the two
    -- given functions.
    pdfGraphLines = map (experLinePdf &&& experLineTitle) eLines
    cdfGraphLines = map (experLineCdf &&& experLineTitle) eLines
       
    eLines        = map rebaseLine $ experGraphLines exper


    basename       = dropExtension graphFile
    graphFile      = combine direct $ experGraphFile exper 
    direct         = takeDirectory file

    -- rebase essentially adds the base directory to the file within
    -- an experiment line.
    rebaseLine :: ExperimentLine -> ExperimentLine
    rebaseLine eline =
      eline { experPepaModel = rebaseFile $ experPepaModel eline
            , experLineCdf   = rebaseFile $ experLineCdf   eline
            , experLinePdf   = rebaseFile $ experLinePdf   eline
            }
    rebaseFile :: FilePath -> FilePath
    rebaseFile = combine direct 

    -- The strings returned is the arguments to the plot comamnd in gnuplot.
    runLine :: ExperimentLine -> IO ()
    runLine experLine = 
      do system $ unwords [ ipcCommand
                          , experPepaModel experLine
                          , "--probe"
                          , surroundInQuotes $  experProbeDef experLine
                          , "--output"
                          , experLineCdf experLine
                          , "--output"
                          , experLinePdf experLine
                          , experLineArgs experLine
                          ]
         return ()


    -- The second argument here is a list of gnuplot commands, for example
    -- you might like to set the key location.
    plotGraph :: [ (FilePath, String) ] -> [ String ] -> String -> IO ()
    plotGraph files gcommands graphname =
      do writeFile gnuFile gnucontents
         system $ unwords [ "gnuplot", gnuFile ]
         system $ unwords [ "eps2pdf -f", epsFile ]
         return ()
      where
      epsFile     = graphname ++ ".eps"
      gnuFile     = graphname ++ ".gnuplot"
  
      gnucontents = unlines [ "set terminal postscript eps colour dash 20"
                            , quoteCommand "set output" epsFile
                            , ""
                            , titleCommand
                            , quoteCommand "set xlabel" xaxis
                          
                            , unlines gcommands

                            , plotCommand
                            ]
  
      plotCommand  = unwords [ "plot"
                             , Utils.mkCSlist $ map makePlotLine files ]
      titleCommand = quoteCommand "set title" $ experGraphTitle exper
      xaxis        = "Time"
  
      makePlotLine :: (FilePath, String) -> String
      makePlotLine (f, t) =
        unwords [ surroundInQuotes f
                , "title"
                , "'" ++ t ++ "'"
                , "with lines lw 3.0"
                -- , "with linespoints"
                ]
  
    -- Commands used as the second argument to 'plotGraph'
    setKeyBottomRight = "set key bottom right"


-- Produces a gnuplot command where the command is the first string
-- and the argument is the second string. Where the argument is
-- to be surrounded in quotes.
quoteCommand :: String -> String -> String
quoteCommand command arg =
  command ++ " " ++ (surroundInQuotes arg)

surroundInQuotes :: String -> String
surroundInQuotes s = "\"" ++ s ++ "\""


parseExperimentFile :: String -> Experiment
parseExperimentFile contents =
  Experiment { experGraphFile  = graphFile
             , experGraphTitle = graphTitle
             , experGraphLines = graphLines
             }
  where
  -- The format then is: (graphfile, graphtitle, experlines)
  -- (graphFile, graphTitle, elines) :: (FilePath, String, [ [ (String, String) ] ])
  (graphFile, graphTitle, elines) = read contents

  graphLines = map readExperLine elines

  readExperLine :: [ ( String, String ) ] -> ExperimentLine
  readExperLine values =
    ExperimentLine { experPepaModel = modelFile
                   , experProbeDef  = probeDef
                   , experLineTitle = lineTitle
                   , experLineCdf   = lineCdf
                   , experLinePdf   = linePdf
                   , experLineArgs  = ipcArguments
                   }
    where
    modelFile    = fromMaybe (error "No model file for exper line")
                             (lookup "model file" values)
    probeDef     = fromMaybe (error "No probe definition for exper line")
                             (lookup "probe definition" values)
    lineTitle    = fromMaybe (error "No title for exper line")
                             (lookup "title" values)
    lineCdf      = fromMaybe defaultCdf $ lookup "cdf file" values
    linePdf      = fromMaybe defaultPdf $ lookup "pdf file" values
    
    ipcArguments = unwords $ mapMaybe makeOption values

    makeOption :: (String, String) -> Maybe String
    makeOption ("start time", s) = Just $ "--start-time " ++ s
    makeOption ("stop time" , s) = Just $ "--stop-time "  ++ s
    makeOption ("time step" , s) = Just $ "--time-step "  ++ s
    makeOption (option, arg)
      | isPrefixOf "--" option   = Just $ option ++ " " ++ arg
      | otherwise                = Nothing
    

    -- The default pdf and cdf files are based on the pepa model
    -- file and the title (since there may be more than one experiment
    -- for a given pepa model).
    basename   = concat [ dropExtension modelFile
                        , "_"
                        , underlineSpaces lineTitle
                        ]
    defaultCdf = basename ++ "_cdf.csv" 
    defaultPdf = basename ++ "_pdf.csv"
  
    underlineSpaces :: String -> String
    underlineSpaces = map spaceToUnderline

    spaceToUnderline :: Char -> Char
    spaceToUnderline c
      | isSpace c = '_'
      | otherwise = c


{-
  Compilation for hydra. We wish to compile to the relevant stage and
  then call hydra to solve the rest of the model.
-}
compileForHydra :: String
                   -- ^ The hydra stage in question
                -> CliOptions a
                   -- ^ The command-line options
                -> FilePath
                   -- ^ The Pepa file to compile
                -> IO ExitCode
                   -- ^ The io action we return.
compileForHydra "flat-mod"     = compileForFlatModHydra
compileForHydra "hier-mod"     = compileForHierHydra
compileForHydra stage          =
  error $ "Unrecognised hydra stage: " ++ stage
  

compileForFlatModHydra :: CliOptions a
                          -- ^ The command-line options
                       -> FilePath
                          -- ^ The Pepa file to compile
                       -> IO ExitCode
                          -- ^ The io action we return.
compileForFlatModHydra options file = 
  parseAndMain options file (getFlatHydraModFile options) postAction
  where
  postAction :: String -> IO ExitCode
  postAction = processDnamModelFile options file


getFlatHydraModFile :: CliOptions a
                       -- ^ The command-line options
                    -> ParsedModel
                       -- ^ The pepa model to process
                    -> MainControl String
                       -- ^ The returned hydra mod file
getFlatHydraModFile options parsedModel =
     -- First of all get the state space of the model
     -- we have to choose the space with no vanishing states
  do sResult        <- getStatesSpaceResult options parsedModel
     let space      = chooseNoVanishing sResult
         -- Now create a flat hydra model from the novanishing
         -- state space. We may consider doing this with the full
         -- state space and allowing hydra to remove the vanishing
         -- states, but if the states space size is large then
         -- we should be using the hierarchical version.
         hModel      = hydraFlatModel space
         -- Get the source and target conditions and use them to
         -- to add a passage measurement spec to the hydra model
         mSpecs      = procMeasurementSpecs $ srProcessResults sResult
         sourceCond  = case measureStartCond mSpecs of
                         Initial     -> error "source-initial not supported by hydra"
                         StartCond c -> c
         targetCond  = measureStopCond  mSpecs
         hFile       = addHydraPassageSpecs hModel 
                                            solControl 
                                            sourceCond
                                            targetCond
                                            times
         header      = unlines [ ipcBanner
                               , "This file was generated by ipc"
                               , "from the following pepa model"
                               -- I should be showing some of the
                               -- processed pepa models here, though
                               -- maybe that is better in the log?
                               ]
     -- Finally return the printed out hydra model
     return $ printDnamModelFile Print.hprintActionIdentList header hFile
  where
  times        = getMeasurementTimeSpecs options
  solControl   = getSolutionControl      options


{-
  Compiling to hydra for a non-flat hierarchical model file.
  This was the original method of compilation and currently suffers
  from the problem that rate expressions become too large.
  I think this can be solved by outputting a customized C function
  to compute the apparent rate of a transition but of course that will
  require some extra work.
-}
compileForHierHydra :: CliOptions a
                       -- ^ The command-line options
                    -> FilePath
                       -- ^ The Pepa file to compile
                    -> IO ExitCode
                       -- ^ The io action we return.
compileForHierHydra options file = 
  parseAndMain options file (getHierHydraModFile options) postAction
  where
  postAction :: String -> IO ExitCode
  postAction = processDnamModelFile options file
             
getHierHydraModFile :: CliOptions a
                       -- ^ The command-line options
                    -> ParsedModel
                       -- ^ The pepa model to process
                    -> MainControl String
                       -- ^ The returned hydra mod file
getHierHydraModFile options parsedModel =
     -- First of all process the pepa model adding all the
     -- probes etc.
  do preResult    <- processPepaModel options parsedModel
     -- Turn the processed model into a hierarchical hydra model
     hModel       <- hydraHierModel $ procSimplifiedModel preResult
         -- Get the source and target conditions from the results of
         -- processing the PEPA model
     let mSpecs      = procMeasurementSpecs preResult
         -- Get the source and target conditions and use them to
         -- add a passage measurement specification to the hydra model
         sourceCond  = case measureStartCond mSpecs of
                         Initial     -> error "source-initial not supported by hydra"
                         StartCond c -> c
         targetCond  = measureStopCond  mSpecs
         hFile       = addHydraPassageSpecs hModel 
                                            solControl 
                                            sourceCond
                                            targetCond
                                            times
         header      = unlines [ ipcBanner
                               , "This file was generated by ipc"
                               , "from the following pepa model"
                               -- I should be showing some of the
                               -- processed pepa models here, though
                               -- maybe that is better in the log?
                               ]
     -- Finally return the printed out hydra model file
     return $ printDnamModelFile Print.hprintActionIdentList header hFile
  where
  times        = getMeasurementTimeSpecs options
  solControl   = getSolutionControl      options



  
{-
  A common function to obtain the post processing of a dnam model file.
  Obviously much more to do here.
-}
processDnamModelFile :: CliOptions a
                        -- ^ The command-line options
                     -> FilePath
                        -- ^ The original pepa file we compiled from
                     -> String
                        -- ^ The contents of the dnam model file
                     -> IO ExitCode
                        -- ^ The io action we return.
processDnamModelFile options file modFileContents = 
     -- First of all write out the contents to the .mod file
  do writeFile modFile modFileContents
     -- Run hydra-s over the .mod file
     exitCode <- system $ unwords [ hydra, modFile ]
     -- perform what ever post hydra processing the command-line options
     -- demand. Only if the hydra command succeeded.
     -- Important since if a previous hydra-s command on the same .mod file
     -- succeeded then we may run the perf or uniform command over old
     -- results and it may look to the user like a success.
     case exitCode of
       ExitSuccess -> postHydra
       _otherwise  -> do putStrLn "hydra-s command failed"
                         return exitCode
  where
  -- The hydra .mod file to output to (and run hydra over obviously)
  modFile  = getModFile options file
  -- The basename which is an option to the later stages of hydra
  baseName = dropExtension modFile
  -- The pt results file which we will output to in the case that we are
  -- doing a passage-time or transient analysis. Note that we do not take
  -- steps to set this just rely on how hydra works it out from the name
  -- of the ".mod" file. This is only used to print out the results to
  -- standard out.
  ptResultsFile = addExtension baseName ".CDF_RESULTS"

  -- The post hydra command depends on the kind of measurement we are doing
  postHydra =
    case getPostHydra options of
      PostHydraPassage    -> runUniform
      PostHydraTransient  -> runUniform
      PostHydraSteady     -> system perfCommand
      PostHydraCount      -> system perfCommand
      PostHydraNone       -> return ExitSuccess

  -- The hydra and hydra-uniform commands may be specified on the command-line
  (hydra, hydraUniform) = getHydraCommands options  

  -- The uniform command also depends on whether the user wishes to obtain
  -- a pdf or cdf of the results.
  uniformCommand        = unwords [ hydraUniform, baseName, cdf ]
  cdf
    | any isPdfOption options = ""
    | otherwise               = "-cdf"

  -- The perf command actually output by the hydra program and its location
  -- depends on the location of the .mod file.
  perfCommand     = unwords [ perfProgram, baseName ]
  perfProgram     = Utils.addLeadingDotSlash $ 
                    Utils.switchExtension ".mod" ".perf" modFile


  -- run uniform just runs the uniform command, but we also check if we
  -- are to print out to std out and in this case we read the results
  -- file and print it out.
  runUniform :: IO ExitCode
  runUniform 
    | getStandardOut options = do exitCode <- system uniformCommand
                                  contents <- readFile ptResultsFile
                                  putStrLn contents
                                  return exitCode
    | otherwise              = system uniformCommand

{-|
  Not quite sure what to call this function.
  It is essentially the main function of all the possible compilations
  (or at least could/should be).
  This function parses the given file as a pepa model into a 'MainControl'
  structure. It then uses the given function to process the pepa model
  into some data structure (be it a result string, the contents of
  a file for further external processing such as with hydra or whatever).
  Finally we exit from the main control structure, possibly giving errors
  but in the case that we actually have a result we do the further processing
  actions given as the final argument.
  The further processing actions may just be to print the results string
  to the screen but may be a complex sequence of post-external processing
  such as calling hydra or dot to create a graph file or something.

  NOW, I would like this function to instead return a IOMainControl ExitCode.
-}
parseAndMain :: CliOptions a
                -- ^ The command-line options.
             -> FilePath
                -- ^ The file containing the PEPA model
             -> (ParsedModel -> MainControl b)
                -- ^ The compilation or transformation function
             -> (b -> IO ExitCode)
                -- ^ The action to perform after compilation/transformation
             -> IO ExitCode
                -- ^ The returned IO action
parseAndMain options file getResult postAction =
     -- parse the contents of the file into a pepa model
  do parsedResult      <- MainControl.runMainControlT $ 
                          FileParser.parsePepaFile file
     -- process the parsed model file into whatever result we expect
     let result = parsedResult >>= getResult
     -- Exit from the main control - and perform whatever post
     -- action is to be performed.
     exitFromMainControl options file postAction result


{-|
  Processing the pepa model we return the pepa model at most of the stages
  as this may be useful to output say in a .mod file.
-}
data ProcessedModel =
  ProcessedModel { procParsedModel      :: ParsedModel
                 , procProbedModel      :: ParsedModel
                 , procSimplifiedModel  :: ParsedModel
                 , procMeasurementSpecs :: MeasureSpecs
                 }

{-|
  Processing of a pepa file.
  By this we mean perform any static analysis, 
  add any performance measurement probes
  and finally simplify the model.
-}
processPepaModel :: CliOptions a
                    -- ^ The command-line options
                 -> ParsedModel
                    -- ^ The parsed model within a 'MainControl'
                 -> MainControl ProcessedModel
                    -- ^ The returned model
processPepaModel options parsedModel =
     -- parse the probe definitions given on the command-line
 do  probes           <- mapM parseProbeMC probeStrings
     -- We parse in the transformation rules and then apply
     -- those to the overRidden model. What might be nice later
     -- is to see if we can just turn the overrides into rewrite rules
     -- we only need to apply this and not override the rate defs.
     rules            <- mapM Rules.Parser.parseRule cliRules
     let ruleSet      = Rules.Syntax.makeRuleSet rules
     transformedModel <- Rules.Apply.applyRulesToModel overRiddenModel ruleSet
     -- Type check the parsed model so that we can give error
     -- messages to the user referring to the input model.
     -- However it is the overRiddenModel and then transformed
     -- model that we analyse, because it might be that the overriding 
     -- and/or tranformation rules cause the model to fail
     -- one of the analyses. For example we may override a rate with
     -- a rate expression which violates the typing.
     typedModel   <- staticAnalyseModel probes transformedModel
     -- Add the probes to the model.
     ( probedModel,
       measureSpecs) <- addProbesToModel options probes typedModel
     -- If we wish here we could add something like:
     -- If log-probed add hprinted probe model to the log.
  
     -- Simplify the probed model (which should probably return
     -- a 'MainControl')
     simplifyResult  <-  Simplify.simplify options probedModel
     let simpModel     = Simplify.simplifiedModel simplifyResult
         simpMapping   = Simplify.simplifiedRateMapping simplifyResult
         qMeasureSpecs = qualifyMeasureSpecs simpMapping measureSpecs
     return ProcessedModel { procParsedModel      = parsedModel
                             , procProbedModel      = probedModel
                             , procSimplifiedModel  = simpModel
                             , procMeasurementSpecs = qMeasureSpecs
                             }
  where
  -- The probe strings defined on the command-line.
  probeStrings = getProbeSpecs options
  -- parse a probe and turn the result from an either into a MainControl
  -- note that we must wrap it in 'PepaParser.wholeParser' otherwise it
  -- may not catch a parse error and return only a prefix of the entire probe.
  parseProbeMC = FileParser.mainControlParse $ 
                 FileParser.wholeParser probeDefParser

  cliRules     = [ r | CliTransformRule r <- options ]

  -- We make the new model by first performing any of the overrides
  overRiddenModel
    | null rateOverrides = parsedModel
    | otherwise          = overrideRateDefinitionList parsedModel rateOverrides
  rateOverrides     = map makeRateDef $ getRateOptions options
  makeRateDef :: (RateIdentifier, Double) -> RateSpec
  makeRateDef = second Rates.realRateExp
   

  labelsOfProbe :: ProbeDef -> [ String ]
  labelsOfProbe (_, probe) = 
     map showOrig $ probeLabelsList probe

  staticAnalyseModel :: [ ProbeDef ]  -> ParsedModel -> MainControl ParsedModel
  staticAnalyseModel probes pModel
      -- If the options specify no static analysis then
      -- we just return the model given to us.
    | not $ getStaticAnalysis options = return pModel
      -- If there are no errors then we can return the result with
      -- the warnings (of which there may also be none)
    | null analysisErrors             = resultWarning pModel 
                                                      allWarnings
                                                      "static-analysis"
                                                      logInformation
      -- If there are any errors then we must return an error
      -- NOTE: it's a shame that in this case we throw away the warnings
      -- perhaps 'resultError' should also accept a list of warnings
      -- alternatively we could return the list of all warnings and errors
      -- (as strings).
    | otherwise                       = resultError $ unlines analysisErrors
    where
    logInformation = unwords [ "Static analysis discovered:"
                             , show $ length allWarnings
                             , "warnings"
                             ]

    -- The analysis report this contains all the information about 
    -- used/defined process/rate names etc. It also contains the
    -- analysis warnings and errors.
    analysis :: AnalysisReport
    analysis = analyseModel pModel

    -- All the messages (warnings and errors) of the analysis report
    -- as a pair with the first being all the errors and the second
    -- being all the warnings.
    analysisMessages  = splitMessages $ messagesOfReport analysis
    analysisErrors    = map hprintAnResult $ fst analysisMessages
    analysisWarnings  = map hprintAnResult $ snd analysisMessages

    -- All the warnings are those of the analysis report as well as
    -- those warnings from the command-line options (see below)
    allWarnings       = allProbeWarnings ++ analysisWarnings

    -- warnings from the probe definitions, these are warnings such as the 
    -- probe attempts to observe an action which the model does not perform.
    -- Currently we are just issueing warnings for the count measures,
    -- if we attempt to measure an activity which is not performed
    -- by the model then we are wrong.
    allProbeWarnings = concat [ countMeasureWarns ]
  
    -- The actions which occur in @--count-measure@ options
    countActions      = concat [ a | CliCount a <- options ]
    countMeasureWarns = mapMaybe makeCMeasureError countActions

    -- The actual names as written in the model of all the actions performed.
    -- Note that they may never actually be performed, but they do occur
    -- in the model.
    modelActionNames  = map showOrig $ usedActionNames analysis
    actionsAndLabels  = probeLabels ++ modelActionNames
    probeLabels       = nub $ concatMap labelsOfProbe probes

    -- For a given action in a count measure if it is not performed by
    -- the model then we should give a warning. Note that we should also
    -- take into account the communication actions of the probes.
    makeCMeasureError :: String -> Maybe String
    makeCMeasureError s 
      | elem s actionsAndLabels = Nothing
      | otherwise               = 
        Just $ unwords [ "action" , s
                       , "occurs in a count-measure option"
                       , "but does not occur in the model."
                       , "The model performs the actions: "
                       , Utils.mkCSlist actionsAndLabels 
                       ]


compileForDot :: CliOptions a -> FilePath -> IO ExitCode
compileForDot options file =
  parseAndMain options file (getStatesSpaceResult options) postAction
  where
  -- The post action writes out the state space to a 
  -- dot file and runs dot over it.
  postAction :: StateResult -> IO ExitCode
  postAction spaces =
    do writeFile dotFile $ stateSpaceToDot space
       system $ unwords [ "dot", "-Tsvg", "-o", svgFile, dotFile ]
    where
    -- The state space to draw a graph of
    space   = chooseSpace options spaces
    -- The dot file to output the state space to
    dotFile = getDotFile options file
    -- The svg file to convert the dot file to.
    svgFile  = Utils.switchExtension ".dot" ".svg" dotFile

compileForPddl :: CliOptions a -> FilePath -> IO ExitCode
compileForPddl options file =
  parseAndMain options file (getStatesSpaceResult options) postAction
  where
  -- The post action writes out the state space to a 
  -- dot file and runs dot over it.
  postAction :: StateResult -> IO ExitCode
  postAction spaces =
    do writeFile pddlFile $ Pddl.statespaceToPddl space
       return ExitSuccess
    where
    -- The state space to draw a graph of
    space    = chooseSpace options spaces
    -- The dot file to output the state space to
    pddlFile = getGenericOutputFile "" ".pddl" options file


{- This version and the one below in comments should be distinguished
   via a --flat or --hier flag and that should also be used for hydra.
-}
compileForJavaSimulator :: CliOptions a -> FilePath -> IO ExitCode
compileForJavaSimulator options file =
  parseAndMain options file getJavaSimulatorResult postAction
  where
  sconfig      = SimulatorConfig { sconfigSimulations = sRuns
                                 , sconfigTimes       = times
                                 }
  times        = getMeasurementTimes options

  sRuns        = Utils.safeList 10000 head $ 
                 [ i | CliSimulations i <- options ]

  -- We should check what we are doing, passage time or steady for example.
  getJavaSimulatorResult :: ParsedModel -> MainControl String
  getJavaSimulatorResult parsedModel = 
    do preResult    <- processPepaModel options parsedModel
       -- Turn the PEPA model into a model representation
       -- Turn the processed model into a hierarchical hydra model
       model        <- Model.pepaToModel $ procSimplifiedModel preResult
           -- Get the source and target conditions from the results of
           -- processing the PEPA model
       let mSpecs      = procMeasurementSpecs preResult
           -- Get the source and target conditions and use them to
           -- add a passage measurement specification to the hydra model
           sourceCond  = case measureStartCond mSpecs of
                           Initial     -> error "source-initial not supported by java"
                           StartCond c -> c
           targetCond  = measureStopCond  mSpecs
           -- So go ahead and actually generate the java simulator
           javaSim     = JavaSimulator.modelToPassSim className
                                                      sconfig 
                                                      targetCond
                                                      model
       return javaSim

  -- The java file to output the state space to
  -- Stupid java hates dashes in names for some reason
  javaFile  = filter (/= '-') $ getGenericOutputFile "" ".java" options file
  className = filter (/= '-') $ File.takeBaseName javaFile

  postAction :: String -> IO ExitCode
  postAction javaContents =
    do writeFile javaFile javaContents
       system ("javac " ++ javaFile)
       let directory = File.takeDirectory javaFile
       when (not $ null directory) $ Directory.setCurrentDirectory directory 
       system ("java " ++ className)


compileForFlatJavaSimulator :: CliOptions a -> FilePath -> IO ExitCode
compileForFlatJavaSimulator options file =
  parseAndMain options file (getStatesSpaceResult options) postAction
  where
  postAction
    | any isSteadyOption options = steadyPostAction
    | otherwise                  = passagePostAction

  -- The java file to output the state space to
  javaFile  = getGenericOutputFile "" ".java" options file
  className = File.takeBaseName javaFile

  passagePostAction :: StateResult -> IO ExitCode
  passagePostAction spaces =
    do writeFile javaFile javaContents
       system ("javac " ++ javaFile)
    where
    javaContents = JavaSimulator.statespaceToPassSim sources 
                                                     targets 
                                                     className 
                                                     space
    space        = chooseNoVanishing spaces
    -- Get the source and target conditions and use them to
    -- obtain from the sets of source and target states.
    mSpecs       = procMeasurementSpecs $ srProcessResults spaces
    targetCond   = measureStopCond  mSpecs
    sources      = getSourceStates mSpecs space
    targets      = getSatisfyingStates targetCond space 


  -- The post action writes out the state space to a 
  -- dot file and runs dot over it.
  steadyPostAction :: StateResult -> IO ExitCode
  steadyPostAction spaces =
    do writeFile javaFile $ JavaSimulator.statespaceToSimulator className space
       system ("javac " ++ javaFile)
       -- return ExitSuccess
    where
    -- The state space to draw a graph of
    space     = chooseSpace options spaces

  


compileForFsp :: CliOptions a -> FilePath -> IO ExitCode
compileForFsp options file = 
  parseAndMain newOptions file getFspResult postAction
  where
  -- Important point we must add to the options the
  -- hide non-cooperating. This will cause the simplification
  -- of the model to rename any action names which components
  -- do not cooperate over. This is important because in fsp
  -- component cooperate over all shared activity names.
  -- Thus if we have
  -- P = (a, r) . (b, r) . P ;
  -- P < > P
  -- This would translate to an fsp model which cooperated over
  -- 'a' and 'b'. However if we hide (and hence rename) the
  -- non-cooperating activities then this is not true.
  newOptions = CliHideNonCoop : options

  getFspResult :: ParsedModel -> MainControl String
  getFspResult parsedModel =
       -- Do all the pepa related processing of the pepa model
       -- obtaining a simplified pepa model with any probes added
    do pModel         <- liftM procSimplifiedModel $ 
                           processPepaModel options parsedModel
       -- Translate the model to fsp, pretty simple now that
       -- we do not have cooperating components performing
       -- the same activity unless they do cooperate over it.
       let fspModel    = pepaModelToFspModel pModel
       return $ printFspModel fspModel

  -- Writing out the fsp model to an fsp file
  writeFspFile :: String -> IO ()
  writeFspFile contents
    | getStandardOut options = putStrLn contents
    | otherwise              = writeFile fspFile contents

  postAction :: String -> IO ExitCode
  postAction contents = writeFspFile contents >> (return ExitSuccess)
  fspFile = getFspFile options file


  

{-
  Compile for prism, we wish to compile to the relevant stage of
  prism (there are only really a flat explicit model or a hierarchical
  model to choose from) and then call prism to do the rest for us.
-}
compileForPrism :: String
                   -- ^ The prism stage in question
                -> CliOptions a
                   -- ^ The command-line options
                -> FilePath
                   -- ^ The PEPA file to compile
                -> IO ExitCode
                   -- ^ The io action we return.
compileForPrism "trans"     = compileForTransPrism
compileForPrism "explicit"  = compileForTransPrism
compileForPrism "model"     = compileForHierPrism
compileForPrism stage          =
  error $ "Unrecognised prism stage: " ++ stage


compileForTransPrism  :: CliOptions a -> FilePath -> IO ExitCode
compileForTransPrism options file =
  parseAndMain options file getPrismResult postPrism
  where
  -- Get the main prism result, this is a flat model state space
  -- represented as all the transitions in the system.
  getPrismResult :: ParsedModel -> MainControl String
  getPrismResult parsedModel =
        -- First of all get the state space of the model
        -- we have to choose the state space with the vanishing
        -- states removed as prism doesn't support them (to my knowledge).
     do space     <- liftM chooseNoVanishing $ 
                        getStatesSpaceResult options parsedModel
        -- And then simply return the explicit prism model space generated
        -- from the state space.
        return $ getPrismExplictModel space

  -- The post prism action is to print out the contents of the
  -- explicit prism model to a file and then run prism over it.
  -- If however the output is to be redirected to standard out then
  -- we of course don't bother to run prism (since there will be no
  -- prism model to run it over).
  postPrism :: String -> IO ExitCode
  postPrism contents
    | getStandardOut options = (putStrLn contents) >> (return ExitSuccess)
    | otherwise              = 
      do writeFile prismFile contents
         system prismCommand

  -- The prism command to run
  prismCommand  = unwords $ "prism" : prismOptions
  -- All of the options to the prism command
  prismOptions  = prismStandard ++ prismCli
  -- The standard options to the prism command which we must give to
  -- every prism invocation, eg we *must* give the transition file.
  prismStandard = [ "-importtrans", prismFile, "-ctmc" ]
  -- The command-line options for prism specified on the command line.
  prismCli      = [ s | CliPrismOptions s <- options ]
  -- The prism file to output to.
  prismFile     = getPrismTransFile options file



compileForHierPrism :: CliOptions a -> FilePath -> IO ExitCode
compileForHierPrism options file =
  parseAndMain options file getPrismResult postPrism
  where
  -- Get the main prism result
  getPrismResult :: ParsedModel -> MainControl String
  getPrismResult parsedModel = 
       -- Do all the pepa related processing of the pepa model
       -- obtaining a simplified pepa model with any probes added
    do pModel         <- liftM procSimplifiedModel $ 
                           processPepaModel options parsedModel
       -- translate to a prism model
       let prismModel     = prismFromPepa reduceRates pModel
           prismModelFile = makePrismFile prismModel
       -- And return the pretty printed string of the prism model.
       return $ printPrismModelFile prismModelFile 

  -- Whether or not to reduce the rate expressions.
  reduceRates     = not $ any isNoReduceRates options

  -- The post prism action is to print out the contents of the
  -- explicit prism model to a file and then run prism over it.
  -- If however the output is to be redirected to standard out then
  -- we of course don't bother to run prism (since there will be no
  -- prism model to run it over).
  postPrism :: String -> IO ExitCode
  postPrism contents
    | getStandardOut options = (putStrLn contents) >> (return ExitSuccess)
    | otherwise              = 
      do writeFile prismFile contents
         system prismCommand

  -- The prism command to run
  prismCommand  = unwords $ "prism" : prismOptions
  -- All of the options to the prism command
  prismOptions  = prismStandard ++ prismCli
  -- The standard options to the prism command which we must give to
  -- every prism invocation, eg we *must* give the transition file.
  prismStandard = [ prismFile ]
  -- The command-line options for prism specified on the command line.
  prismCli      = [ s | CliPrismOptions s <- options ]

  -- The prism file to output to, note not a prism transitions file
  -- as it was above in 'compileForTransPrism'
  prismFile    = getPrismFile options file


compileForDizzy :: CliOptions a -> FilePath -> IO ExitCode
compileForDizzy options file =
  parseAndMain newOptions file getDizzyResult postAction
  where
  -- Unless the user has specifically said not too aggregate then
  -- we must aggregate otherwise the (generally large) process arrays
  -- will be expanded out. I hope in the future that aggregation is
  -- the default option anyway but for nwo I will add this.
  -- Note that in general the user won't specify --no-aggregate as it's
  -- generally silly to for dizzy.
  newOptions
    | any isNoAggregateOption options = options
    | otherwise                       = CliAggregate : options

  getDizzyResult :: ParsedModel -> MainControl String
  getDizzyResult parsedModel =
       -- Do all the pepa related processing of the pepa model
       -- obtaining a simplified pepa model with any probes added
    do pModel         <- liftM procSimplifiedModel $ 
                           processPepaModel options parsedModel
       -- translate to a dizzy model
       dizzyModel     <- pepaToDizzy pModel
       let dizzyModelFile = printDizzyModelFile dizzyModel
       return dizzyModelFile

  -- Writing out the fsp model to an fsp file
  writeDizzyFile :: String -> IO ()
  writeDizzyFile contents
    | getStandardOut options = putStrLn contents
    | otherwise              = writeFile dizzyFile contents

  postAction :: String -> IO ExitCode
  postAction contents = writeDizzyFile contents >> return ExitSuccess
  dizzyFile = getDizzyFile options file


{-| Measurement specifications.
    These are computed during the addition of the probes.
    Because the measurement specifications will refer to
    the states of the probes.
-}
data MeasureSpecs =
  MeasureSpecs { measureStartCond   :: StartCondition
               , measureStopCond    :: StateCondition
               , measureRunningCond :: StateCondition
               , measureStartActs   :: [ String ]
               , measureStopActs    :: [ String ]
               }
data StartCondition = Initial | StartCond StateCondition


qualifyMeasureSpecs :: Map String Rates.RateExpr -> MeasureSpecs -> MeasureSpecs
qualifyMeasureSpecs mapping mspecs =
  mspecs { measureStartCond   = startCond
         , measureStopCond    = qualifyCond $ measureStopCond    mspecs
         , measureRunningCond = qualifyCond $ measureRunningCond mspecs
         }
  where
  startCond = case measureStartCond mspecs of
                Initial     -> Initial
                StartCond c -> StartCond $ qualifyCond c
  qualifyCond :: StateCondition -> StateCondition
  qualifyCond = Rates.remapRateExpr mapping

{-
  Adds the probe definitions to the given parsed model.
  Note that I think this function should also take in a result of
  static analysis. This would allow us to analyse the probes being
  added.
-}
addProbesToModel :: CliOptions a
                    -- ^ The command-line options
                 -> [ ProbeDef ]
                    -- ^ The probes definitions to add to add
                 -> ParsedModel
                    -- ^ The model to which to add any probe definitions
                 -> MainControl (ParsedModel, MeasureSpecs)
                    -- ^ The resulting model plus the measurement specs
addProbesToModel options probes model =
  do  -- add the probe definitions ot the the model
      probedModel <- addProbeDefs trFlags probes model
      -- Add the master probe to the model, note that this may *not* add
      -- any master probe depending on the command-line options 
      let modelAndSpecs    = addMaster probedModel
          finalProbedModel = fst modelAndSpecs
          logString        = hprintPepaModel finalProbedModel
      valueResult modelAndSpecs "probed-model" logString
  where
  -- We want to minimise the self-loops.
  trFlags      = [ MinSelfLoops ]
  -- Obviously we do not add a master probe if the '--no-master' option
  -- is set, however we also do not add a master probe if
  -- there are no start/stop actions  and no probe specifications, 
  -- since in that case we assume that the user is
  -- just wanting to translate the unchanged model.
  -- We do not however need to check for probe strings since the
  -- start and stop actions will only be null if probe strings is
  -- as well
  -- TODO: we should write a function for finding a new probe name
  -- basically one that checks if the one we wish to add is already
  -- defined and if so just add a number or something.
  addMaster :: ParsedModel -> (ParsedModel, MeasureSpecs)
  addMaster pModel
    | shouldAddMaster              = 
      -- So if there are probes and such and it looks like we'll
      -- need the passage probe then that is the model that we
      -- return
      ( passageModel, measureSpecs )
    | (not $ null cliStartConds)
      && (not $ null cliStopConds) = 
      -- Otherwise if the user is specifying their own start and stop
      -- conditions then they can't be relying on the probes being there
      -- since they cannot know the name of the probe names so we just
      -- return the model as is. This is separate from the below case
      -- as I might slightly change my mind about this case.
      ( pModel, measureSpecs )
    | otherwise                    = 
      -- But certainly if the user hasn't specified any probes or
      -- source/target conditions we can assume they want to analyse
      -- the raw model
      ( pModel, measureSpecs )
    where
    -- So add the master probe to the mode l
    masterModel  = addMasterProbe  masterStopped  masterRunning 
                                   startActs      
                                   stopActs 
                                   pModel
    -- Add the passage probe to the model which has already had the
    -- master probe added to it. However note that we only add
    -- the passage probe if we detect that we are doing a passage
    -- measurement.
    passageModel
      -- If the user explicitly states that they want a passage
      -- measurement then we must add the passage probe
      | any isPassageOption      options = passProbed
      | any isPassageEndOption   options = passProbed
      -- If the user explicitly states that they want some
      -- kind of measurement other than a passage-time measurement
      -- then don't add the passage probe.
      | any isAverageResponseOpt options = masterModel
      | any isSteadyOption       options = masterModel
      -- Note that here the user probably wants the state space size
      -- without the passage probe. If they want that then they
      -- can specify "--passage" which will add the passage-probe
      -- because it is checked for above here.
      | any isStatesSizeOption   options = masterModel
      -- finally we seem to be in major doubt so just add the
      -- passage probe.
      | otherwise                        = passProbed
    passProbed   = addPassageProbe passageStopped passageRunning 
                                   startActs masterModel
    measureSpecs = MeasureSpecs { measureStartCond   = startCond
                                , measureStopCond    = stopCond
                                , measureRunningCond = runningCond
                                , measureStartActs   = startActs
                                , measureStopActs    = stopActs
                                }
    startCond
      | any isSourceInitialOption options = Initial
      | null cliStartConds = 
        StartCond $ Rates.Cident $ Qualified.Unqualified passageRunning
      | otherwise          = StartCond $ cliStartCond
    stopCond
      | null cliStopConds  =
        Rates.Cident $ Qualified.Unqualified masterStopped
      | otherwise          = cliStopCond
    runningCond  = Rates.Cident $ Qualified.Unqualified masterRunning
  shouldAddMaster = ( getMasterBool options )
                     && ( (not $ null startActs)
                     || ( not $ null stopActs ))

  masterStopped  = "yyMasterProbeStopped"
  masterRunning  = "yyMasterProbeRunning"
  passageStopped = "yyPassageProbeStopped"
  passageRunning = "yyPassageProbeRunning"
  -- If the user specifies a probe specification they need not
  -- specify start and stop actions since we can assume that the
  -- probe performs the 'start' and 'stop' communication actions.
  -- They user can though of course override the default 'start'
  -- and 'stop' if they wish something else (for example if they
  -- have no probe specification).
  startActs
      -- If the user has specified the start actions then obviously
      -- that is what we use.
    | not $ null flagStartActs = flagStartActs
      -- If the user has not specified any start actions OR probes
      -- then probably the user isn't doing any measurement
      -- (at least not using probes so we have no start actions.
    | null probes              = []
      -- But if there *are* probes *and* the user hasn't specified
      -- what the start and stop actions are then they of course
      -- default to just the 'start' action.
    | otherwise                = [ "start" ]
  flagStartActs                = getStartActions options
  stopActs
      -- If the user has specified the stop actions then obviously
      -- that is what we use.
    | not $ null flagStopActs  = flagStopActs
      -- If the user has not specified any stop actions OR probes
      -- then probably the user isn't doing any measurement
      -- (at least not using probes) so we have no stop actions
    | null probes              = []
      -- But if there *are* probes *and* the user hasn't specified
      -- what the start and stop actions are then they of course
      -- default to just the 'stop' action.
    | otherwise                = [ "stop" ]
  flagStopActs                 = getStopActions options

  -- The foldr1 are safe since the *cond should not be used if *conds is null.
  cliStartCond     = foldr1 Rates.Cand cliStartConds
  cliStartConds    = [ parseCExpr s | CliStartCond s <- options ]
  cliStopCond      = foldr1 Rates.Cand cliStopConds
  cliStopConds     = [ parseCExpr s | CliStopCond s <- options ]

  -- as usual we should pass back a 'MainControl' rather than use a 'blindRun'
  parseCExpr :: String -> Rates.RateExpr
  parseCExpr = PepaParser.blindRun parseErr PepaParser.exprParser 

  parseErr  = unwords [ "Ipc.getPassageTimeResult: you have passed in as a state"
                      , "measure but the condition on the state cannot be parsed"
                      ]

{-
  Here is some broken code for invoking hydra but not from the start.


{-
  Print out the state space (and the other files required for hydra
  to begin at the state space stage) and then call hydra as necessary.
-}
fromStates :: CliOptions a -> String -> StateSpace -> IO ()
fromStates _options baseName states =
  do prepareHydraStateFiles states
     system $ "hydra-steady " ++ baseName
     return ()

{-
  Print the matrix to a file a the call the rest of hydra to solve
  the model for us.
-}
fromMatrix :: CliOptions a -> String -> GeneratorMatrix -> IO ()
fromMatrix _options baseName matrix =
  do writeFile matrixFile matrixContents
     system $ "hydra-func " ++ baseName
     return ()
  where
  -- Get the output matrix file
  matrixFile     = addExtension baseName "MATRIX"

  -- Get the matrix file from the generator matrix
  matrixContents = prepareMatrixFile matrix

-}

{- Utility functions over command line options -}
isNoReduceRates :: CliOpt a -> Bool
isNoReduceRates CliNoReduceRateExps = True
isNoReduceRates _                   = False

isAllowSelfLoopsOption :: CliOpt a -> Bool
isAllowSelfLoopsOption (CliAllowSelfLoops) = True
isAllowSelfLoopsOption _                   = False

isAllowDeadlocksOption :: CliOpt a -> Bool
isAllowDeadlocksOption (CliAllowDeadLocks) = True
isAllowDeadlocksOption _                   = False

isPdfOption :: CliOpt a -> Bool
isPdfOption (CliPassageFunction "pdf") = True
isPdfOption _                          = False

isSteadyOption :: CliOpt a -> Bool
isSteadyOption CliSteady = True
isSteadyOption _         = False

isPassageOption :: CliOpt a -> Bool
isPassageOption CliPassage = True
isPassageOption _          = False

isPassageEndOption :: CliOpt a -> Bool
isPassageEndOption CliPassageEnd = True
isPassageEndOption _             = False

isAverageResponseOpt :: CliOpt a -> Bool
isAverageResponseOpt CliAverageResponse = True
isAverageResponseOpt _                  = False

isStatesSizeOption :: CliOpt a -> Bool
isStatesSizeOption CliStatesSize = True
isStatesSizeOption _             = False

isEstimateSizeOption :: CliOpt a -> Bool
isEstimateSizeOption CliEstimateSize = True
isEstimateSizeOption _               = False

isFspOption :: CliOpt a -> Bool
isFspOption CliFsp = True
isFspOption _      = False

isDizzyOption :: CliOpt a -> Bool
isDizzyOption CliDizzy = True
isDizzyOption _        = False

isComparePepato :: CliOpt a -> Bool
isComparePepato CliComparePepato = True
isComparePepato _                = False

-- isExperimentalFlag :: CliOpt a -> Bool
-- isExperimentalFlag CliExperimental = True
-- isExperimentalFlag _               = False

-- isStateSizeOption :: CliOpt a -> Bool
-- isStateSizeOption CliStatesSize = True
-- isStateSizeOption _             = False

isNoAggregateOption :: CliOpt a -> Bool
isNoAggregateOption (CliNoAggregate) = True
isNoAggregateOption _                = False

isShowLogOption :: CliOpt a -> Bool
isShowLogOption (CliShowLog) = True
isShowLogOption _            = False

isDotOption :: CliOption a -> Bool
isDotOption (CliDot) = True
isDotOption _        = False

isPddlOption :: CliOption a -> Bool
isPddlOption (CliPddl) = True
isPddlOption _         = False

isJavaOption :: CliOption a -> Bool
isJavaOption (CliJavaSimulator) = True
isJavaOption _                  = False

isSourceInitialOption :: CliOption a -> Bool
isSourceInitialOption (CliSourceInitial) = True
isSourceInitialOption _                  = False

isNoRemoveVanishing :: CliOption a -> Bool
isNoRemoveVanishing (CliNoReduceVanishing) = True
isNoRemoveVanishing _                      = False

{-|
  We try to do all of *our* processing in the 'MainControl' monad.
  But eventually at some point we'll have to exit from that monad
  and actually do something with the result.
  This function does the necessary exiting from the 'MainControl'
  monad and displaying any errors. It (should but doesn't yet) deal
  with writing the log information to a file (or standard out) and
  exiting gracefully in the case that there is no result.
  The caller provides a function (which creates a IO action) to perform
  should there be a result in the 'MainControl'.
  
  Often the result will be a string which represents the contents of
  some file and the action will write that contents out to the file
  but also perhaps do some post-processing of the file such as for
  example calling 'hydra'.
-}
exitFromMainControl :: CliOptions a 
                       -- ^ The command-line options
                    -> FilePath 
                       -- ^ The pepa file (used for working out the log file)
                    -> (b -> IO ExitCode) 
                       -- ^ The IO action to take.
                    -> MainControl b 
                       -- ^ The main control from which we are exiting.
                    -> IO ExitCode -- ^ The io action returned with an exit code
exitFromMainControl options file action result =
  case closeOrError result of
    Right (a, [], loginfo)
      | Map.null loginfo   -> action a
      | otherwise          -> do displayLog  loginfo
                                 writeLog loginfo
                                 action a
    Left (err,  loginfo)   -> do hPutStrLn stderr errorMessage
                                 hPutStrLn stderr errorSuggest
                                 hPutStrLn stderr separatorLine
                                 writeLog loginfo
                                 hPutStrLn stderr err
                                 return $ ExitFailure 1
    Right (a, warns, loginfo)
      | getStaunch options -> do writeLog loginfo
                                 displayLog  loginfo
                                 hPutStrLn stderr staunchWarn
                                 hPutStrLn stderr separatorLine
                                 hPutStrLn stderr $ unlines warns
                                 hPutStrLn stderr staunchWarn
                                 action a
      | otherwise          -> do writeLog loginfo
                                 displayLog  loginfo
                                 hPutStrLn stderr staunchSuggest
                                 hPutStrLn stderr separatorLine
                                 hPutStrLn stderr $ unlines warns
                                 return $ ExitFailure 1
  where
  separatorLine = "------------------------------------"
  separateTitle t = unlines [ separatorLine, t, separatorLine ]

  -- Display the log to standard out.
  displayLog :: LogInfo -> IO ()
  displayLog loginfo
    | any isShowLogOption options = 
      do putStrLn $ separateTitle " Logging information"
         putStrLn logString
         putStrLn $ separateTitle " End of logging information"
    | otherwise                   = return ()
    where
    logString = showLog loginfo

  
  -- Currently 'showLog' just shows all the logging information
  -- however eventually we wish to provide command-line options to
  -- allow the user to specify specific parts of the log or log levels.
  showLog :: LogInfo -> String
  showLog loginfo
    | elem "all" logNameOptions = 
      unlines $ map getLogEntryString allLogEntries
    | otherwise                 =
      unlines $ map getLogEntryString namedEntries
    where
    -- Create a string from a name and log entry pair.
    getLogEntryString :: (String, LogEntry) -> String
    getLogEntryString (name, logentry) = 
      unlines [ separateTitle name
              , logEntryData logentry
              ]

    -- All of the entries in the log, will be used if the '--log all'
    -- option is specified.
    allLogEntries = Map.toList loginfo
                                 
    -- The list of named entries will be used if '--log all' is not
    -- specified. Note that if 'logNameOptions' is null then we won't
    -- do this at all.
    namedEntries  = filter isNamed allLogEntries

    -- Returns true if the given name is within the list of names specified
    -- by the --log option.
    isNamed :: (String, LogEntry) -> Bool
    isNamed (name, _) = 
      case lookup name logNameGroups of
        Just s  -> elem name logNameOptions || elem s logNameOptions
        Nothing -> elem name logNameOptions

  -- The list of --log options 
  logNameOptions :: [ String ]
  logNameOptions = [ s | CliLog s <- options ]

  -- If the relevant flag is set then write the log to a file
  writeLog :: LogInfo -> IO ()
  writeLog loginfo
    | not $ null logNameOptions = writeFile logFile $ showLog loginfo
    | otherwise                 = return ()

  logFile = getLogFile options file

  staunchWarn    = "There were some warnings - attempting to compile anyway"
  staunchSuggest = "There were some warnings - compilation cancelled" ++
                   " - to compile anyway use --staunch"
  errorMessage   = "There were some errors, compilation cancelled"
  errorSuggest   = "To attempt to compile anyway use --no-static-analysis"


{-
  The log name groups map specific log-item names to groups.
  This way if the user specifies '--log user' we can easily
  check if the specific log item's name should be output to the log.
-}
logNameGroups :: [ ( String, String ) ]
logNameGroups = [ ( stateSpaceSizeLogName   , "user" )
                , ( "static-analysis"       , "user" )

                , ( "uniform-hops"          , "developer" )
                , ( "vanishingStatesRemoved", "developer" )
                ]

stateSpaceSizeLogName :: String
stateSpaceSizeLogName = "state-space-size"

{-
  This function is intended to be a generic method for making
  an output file based on the input pepa file.
  Each possible output generally has a way for the user to specify
  it explicity (such as --dizzy-file or --mod-file) so we need to
  take in a function that may return one from the options.

  Where there is no explicit flag for the option then you can call
  this function with (\_ -> Nothing) as the second argument.

  See below at 'getModFile' for a suitable way to specialise this function.

  TODO: this should also taken into account an --output-dir flag.
-}
getOutputFile :: (CliOption a -> Maybe FilePath)
              -> String    -- ^ first part of the suffix to go before the '.'
              -> String    -- ^ The extension (not including the '.')
              -> CliOptions a
              -> FilePath
              -> FilePath
getOutputFile getO suffix extension options file
  -- If there are no suitable output options then just change
  -- the suffix of the pepa file.
  | null suitable          = 
    File.addExtension ((dropPepaExt file) ++ suffix) extension
  -- If there is exactly one then use it. 
  | [ one ] <- suitable    = one
  -- If there is more than one, then we're humped.
  | otherwise              = 
    error $ "Can only specify one output file of type: " ++ suffix
  where
  outputFiles = [ s | CliOutput s <- options ]
  suitable    = explicit ++ (filter (isSuffixOf suffix) outputFiles)
  -- The explicit options determined by the given function
  explicit    = [ s | Just s <- map getO options ]

  dropPepaExt :: FilePath -> FilePath
  dropPepaExt f
    | (File.takeExtension f) == ".pepa" = File.dropExtension f
    | otherwise                        = f

{-
  A slight specialisation of 'getOutputFile' for the cases where there are
  no specific flags.
-}
getGenericOutputFile :: String 
                        -- ^ As above the first part of the suffix
                     -> String
                        -- ^ The new extension after the '.'
                     -> CliOptions a 
                        -- ^ The command-line options
                     -> FilePath
                        -- ^ The pepa file in question
                     -> FilePath
                        -- ^ The returned filepath
getGenericOutputFile = getOutputFile (const Nothing)

---------------------------------------------
-- Specialisations for 'getOutputFile' and 'getGenericOutputFile'

{-
  Returns the .mod filename we should output to if we are compiling for
  hydra. It is based on 
-}
getModFile :: CliOptions a -> FilePath -> FilePath
getModFile = 
  getOutputFile getMod "" ".mod"
  where
  getMod :: CliOption a -> Maybe FilePath
  getMod (CliModFileName m) = Just m
  getMod _                  = Nothing

{-
  Returns the .dot filename we should output the state space to
  if compiling for dot.
-}
getDotFile :: CliOptions a -> FilePath -> FilePath
getDotFile = getGenericOutputFile "" ".dot"

-- Returns the log file.
getLogFile :: CliOptions a -> FilePath -> FilePath
getLogFile = getGenericOutputFile "" ".log"

-- Returns the prism file
getPrismFile :: CliOptions a -> FilePath -> FilePath
getPrismFile = getGenericOutputFile "" ".sm"

-- Returns the prism transition file
getPrismTransFile :: CliOptions a -> FilePath -> FilePath
getPrismTransFile = getGenericOutputFile "" ".tra"

-- Returns the fsp file
getFspFile :: CliOptions a -> FilePath -> FilePath
getFspFile = getGenericOutputFile "" ".ltsa"

getDizzyFile :: CliOptions a -> FilePath -> FilePath
getDizzyFile = getGenericOutputFile "" ".dizzy"


-- Returns whether or not there are any probes or source/target
-- actions in the command-line options
noPassageIndications :: CliOptions a -> Bool
noPassageIndications options
  -- Of course if the user explictly states that they want
  -- a passage-time measurement then this is a passage indication
  | any isPassageEndOption options  = False
  | any isPassageOption    options  = False
  -- If the user explicitly states that they want 'steady' state
  -- analysis then of course return true.
  | any isNonPassage options        = True
  -- If the user has added something which indicates we want a passage
  -- measurement, then return false.
  | not $ null passageInds          = False
  -- Finally if the user has said nothing they probably want steady state
  | otherwise                       = True
  where
  isNonPassage :: CliOption a -> Bool
  isNonPassage opt = (isSteadyOption opt) ||
                     (isAverageResponseOpt opt) ||
                     (isStatesSizeOption opt)
  
  passageInds = concat [ probes, starts, stops, sourceConds, targetConds ]
  probes      = getProbeSpecs   options
  starts      = getStartActions options
  stops       = getStopActions  options
  sourceConds = [ s | CliStartCond s <- options ]
  targetConds = [ s | CliStopCond s <- options ]

---------------------------------------------
-- End of specialisations for 'getOutputFile'
