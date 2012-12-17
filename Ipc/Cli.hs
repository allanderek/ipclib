{-|
  This module defines the command line options for the main
  @ipc@ executable, but can also be used by most of the example
  programs in the example directory.
-}
module Ipc.Cli
   ( getCliArgs
   , Cli                  ( .. )
   , CliOpt               ( .. )
   , CliVersion
   , CliArg
   , IpcGraphOutput       ( .. )

   , toCli

   -- Exported for @CliDoc.hs@
   , ipcManual
   , printManual

   -- Exported for @Smc/Cli.h@
   , Manual
   , ManualSection
   , ManualDesc
   , ManualEntry
   , optDescsOfManual


   , baseCliOptions

   , getStaticAnalysis
   , getStaunch

   , getProbeSpecs
   , getMasterBool
   , getStartActions
   , getStopActions

   , getRateOptions
   , getProcessRenames
   , getRateRenames
   , getPrioritised

   , containsHideNonCoop
   , getProcessNumber
   , getModFileName
   , getPrismFileName
   , getDizzyFileName
   , getOutputFileName
   , getSolutionControl
   , getMeasurementSpecs
   , getGraphLineWidth

   , PostHydra            ( .. )
   , getPostHydra

   , shouldRunHydra
   , getHydraCommands

   , getStandardOut

   , getNonStandard
   )
where

{- Standard imported libraries -}
import Control.Arrow
  ( first, second )
import Data.Char
   ( isSpace )
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Maybe
   ( mapMaybe )
import System.Console.GetOpt
  ( getOpt
  , usageInfo
  , ArgOrder  ( .. )
  , OptDescr  ( .. )
  , ArgDescr  ( .. )
  )
import qualified System.Environment as Env
import qualified System.FilePath as File
import Text.ParserCombinators.Parsec
   ( ParseError )
{- External Library modules imported -}
{- Local imported libraries -}
import qualified Language.Pepa.Utils as Utils
import Language.Pepa.QualifiedName
  ( QualifiedName     ( .. ) )
import Language.Pepa.Rates
  ( RateExpr          ( .. ) )
import Language.Pepa.Syntax
   ( RateIdentifier
   , ActionIdentifier

   , ParsedPriority
   , defaultIncreasedPriority
   )
import Language.Pepa.Parser
   ( parseCommaSeparatedLowers
   , parseRateOption
   , blindRun
   , exprParser
   )
import Language.Pepa.Transform.Replace
   ( ProcessRename
   , ProcessRenameMap
   , RateRename
   , RateRenameMap
   )
import Ipc.CliParsers
   ( joinParseErrorList
   , parseProcessRename
   , parseRateRename
   , parseCommaActionsMaybeAssigns
   )
import qualified  Ipc.DrawGraph as DrawGraph
{- Hydra stuff required for measurement specifications -}
import Language.Hydra.Syntax
   ( DNAMperformMeasure    ( .. )
   , DNAMpassageMeasure    ( .. )
   , DNAMtransientMeasure  ( .. )
   , DNAMsteadyMeasure     ( .. )
   , DNAMcountMeasure      ( .. )
   , DNAMsteadyEstimator   ( .. )

   , DNAMsolutionControl   ( .. )
   , parseSolutionMethod
   , defaultSolutionMethod

   , DNAMcondition         ( .. )
   )
{- End of module imports -}

{-|
  The main data type for holding the command-line arguments
-}
data CliOpt a = 
   CliVerbose 
 | CliVersion 
 | CliHelp 

 {- Options controlling static analysis -}
 | CliStaunch
 | CliNoStaticAnalysis
 -- Currently this does not switch off the STATIC analysis
 -- but does switch off the detection during state space
 -- exploration. This is different for example a cooperating
 -- array with an activity which may toggle states, if there
 -- are half of them in one state then firing it will result
 -- in the same (aggregated) state.
 -- Additionally though I think that self-loops may be alright
 -- for calculating the apparent rate (we then throw away) the
 -- transitions.
 | CliAllowSelfLoops
 -- Note that this is the default for transient analysis anyway
 | CliAllowDeadLocks

 {- Options for controlling performance measurements -} 
 | CliProbeSpec String
 | CliNoMasterProbe
 | CliStartActions [ String ]
 | CliStopActions  [ String ]
 | CliStartCond    String
 | CliStopCond     String
 | CliSourceInitial

 | CliAverageResponse
 | CliPassage
 | CliPassageFunction String -- specify cdf or pdf
 | CliPassageEnd
 | CliNoNormalise
 | CliSteady
 | CliTransient
 | CliCount        [ String ]
 | CliState        String
 | CliNoMeasurement

 | CliSteadyMean
 | CliSteadyVariance
 | CliSteadyStdDev
 | CliSteadyDistrib

 | CliStartTime Double
 | CliStopTime  Double
 | CliTimeStep  Double
 
 | CliSolver String

 {- Options for pre-modifying the model -}
 | CliRate (RateIdentifier, Double)

 {- Options for replacing names within the model -}
 | CliReplaceProcess String
 | CliReplaceRate    String

 {- This may well be a temporary command-line option, we definitely
    need to allow this in the syntax anyway and maybe as an option
    it doesn't work so well, but this is more or less just a proof
    of concept idea.
    Actually I now believe that we are as well to have a command-line
    option as well, I think it works okay.
 -}
 | CliPrioritise [ (ActionIdentifier, ParsedPriority) ]


 {- Options controlling the model transformations -}
 -- | CliTransformFile FilePath
 | CliTransformRule String
 | CliNoReduceRateExps
 | CliHideNonCoop
 | CliNoReduceVanishing

 | CliProcessNum Int

 {- Options for giving extra output -}
 | CliShowSimplified
 | CliShowProbed

 {- Options controlling state space exploration -}
 | CliAggregate
 | CliNoAggregate
 | CliSpaceLimit  Int

 {- Options for running hydra -}
 | CliRunHydra
 | CliHydra String
 | CliHydraStage String

 {- Options for running Prism -}
 | CliPrism String
 | CliPrismOptions String

 {- Options for producing a java based simulator -}
 | CliJavaSimulator
 | CliSimulations Int

 {- Other compilations -}
 | CliFsp
 | CliDizzy
 | CliDot
 | CliPddl

 {- options for controlling the output locations -}
 | CliModFileName   FilePath
 | CliPrismFileName FilePath
 | CliDizzyFileName FilePath
 | CliOutput        FilePath
 | CliStandardOut

  {- Select which kind of graph output -}
 | CliGraphOutput IpcGraphOutput
 | CliLineWidth   Double

  {- Logging Options -}
 | CliLog           String
 | CliShowLog

  {- Miscellaneous Options -}
 | CliStatesSize
 | CliEstimateSize
 | CliComparePepato

  -- This is a generic flag meaning "enable a new approach/version of .."
  -- Generally only to be used by ipc developers. So for example a new approach
  -- to state space generation, rather than inventing a new flag 
  -- "--use-new-state-space-gen" we can just test for this. If later we decide that
  -- we do actually wish to have both *then* we can invent two flags for it.
 | CliExperimental


  {- Options for other programs which use these options -}
 | CliNonStandard a
 deriving (Show, Eq)


isCliVersion :: CliOpt a -> Bool
isCliVersion CliVersion = True
isCliVersion _          = False

isCliHelp :: CliOpt a -> Bool
isCliHelp CliHelp = True
isCliHelp _       = False

{-| 
   The 'System.Console.GetOpt' library returns a triple giving the flags,
   arguments and errors. 
   This data type translates the triple into a more explicit
   representation of the result of getting the program arguments.
-}
data Cli a =
    CliValid [ CliOpt a ] [ CliArg ] 
  | CliError [ CliOpt a ] [ CliArg ] String
  | CliInfo  [ CliOpt a ] [ CliArg ] String

{-| The type of arguments to the command-line,
    these will generally be pepa model files 
-}
type CliArg = String

{-| The type of the version used by the command-line -}
type CliVersion = String


{-|
  Turn the list of command line options into a 'Cli' description.
  Note that the version string may be different for different
  ipc-related programs.
-}
toCli ::  CliVersion              -- ^ The version string
      -> String                  -- ^ The program name
      -> [ OptDescr (CliOpt a) ] -- ^ The command-line options descriptions
      -> [ CliArg ]              -- ^ The command line arguments
      -> Cli a
toCli version progName cliOptions cliArgs = 
   case getOpt Permute cliOptions cliArgs of
    (options, args, [])   
      | any isCliVersion options -> CliInfo  options args versionOutput
      | any isCliHelp    options -> CliInfo  options args helpOutput
      | otherwise                -> CliValid options args
    (options, args, errs)        -> CliError options args $ errorMsg errs
   where
   versionOutput = addProgName version
   helpOutput    = addProgName usage

   header        = unwords [ "Usage:", progName, "[OPTION...] [PEPAFILE]" ]
   info          = unwords [ "Try `" , progName
                           , " --help' for more information." 
                           ]
   usage         = usageInfo header cliOptions

   errorMsg :: [ String ] -> String
   errorMsg errs = addProgName . unlines $ errs ++ [ header, info ]

   addProgName :: String -> String
   addProgName s = concat [ progName, ": ", s ]

{-|
   A simple wrapper for 'System.Environment.getArgs' which interprets any
   @--argfile file@ options.
-}
getCliArgs :: IO [ String ]
getCliArgs =
  Env.getArgs >>= interpretArgFiles []
  where
  -- The first argument is the list of argument files we have
  -- already seen, we do not wish to enter a loop.
  interpretArgFiles :: [ FilePath ] -> [ String ] -> IO [ String ]
  interpretArgFiles seen ("--argfile" : argfileName : rest)
     | elem argfileName seen         = 
        do putStrLn "Warning: loop in '--argfiles' files"
           interpretArgFiles seen rest
     | otherwise                     =
        do contents <- readFile argfileName
           interpretArgFiles (argfileName : seen) $ 
                 (argsOfFile contents) ++ rest
  interpretArgFiles _seen []         = return []
  interpretArgFiles seen (h : rest)  = 
     do remainder <- interpretArgFiles seen rest
        return $ h : remainder
  
  argsOfFile :: String -> [ String ]
  argsOfFile = (filter (not . (all isSpace))) . parseWords

  -- This is a small parser for argument files.
  parseWords :: String -> [ String ]
  parseWords "" = []
  parseWords ( '#' : rest) =
     parseWords $ ignoreComment rest
  parseWords ( '"' : rest) =
     w : (parseWords remainder)
     where (w, remainder) = parseStringLiteral rest
  parseWords s             =
     w : (parseWords rest)
     where (w, rest) = parseWord $ dropWhile isSpace s

  parseWord :: String -> (String, String)
  parseWord = break endsWord

  endsWord :: Char -> Bool
  endsWord '#' = True
  endsWord '"' = True
  endsWord c   = isSpace c

  ignoreComment :: String -> String
  ignoreComment = dropWhile (not . (== '\n'))

  parseStringLiteral :: String -> (String, String)
  parseStringLiteral ""                   =
     error "string literal not finished"
  parseStringLiteral ( '\\' : '"' : rest) =
     first ( '"' :) $ parseStringLiteral rest
  parseStringLiteral ( '"' : rest)        =
     ("" , rest)
  parseStringLiteral (h : rest)           =
     first ( h :) $ parseStringLiteral rest


{-
  These definitions allow us to generate latex from the option descriptions.
-}
optDescOfManualDesc :: ManualDesc a -> OptDescr (CliOpt a)
optDescOfManualDesc = snd

optDescsOfManualSection :: ManualSection a -> [ OptDescr (CliOpt a) ]
optDescsOfManualSection = (map optDescOfManualDesc) . snd

optDescsOfManual :: Manual a -> [ OptDescr (CliOpt a) ]
optDescsOfManual = concatMap optDescsOfManualSection

-- This top version is very strange it's an uncallable option. Shouldn't occur.
manualPartOfManualDesc :: ManualDesc a -> String
manualPartOfManualDesc ( "", Option "" [] _ _)                = ""
manualPartOfManualDesc ("", optDesc@(Option _ _ _ usage))     =
  unlines [ "\\begin{description}"
          , "\\item[" ++ headerBitOfOptDesc optDesc ++ "]"
          , "No manual entry but the usage information states:"
          , usage
          , "\\end{description}"
          ]
manualPartOfManualDesc (s, optDesc)                           =
  unlines [ "\\begin{description}"
          , "\\item[" ++ headerBitOfOptDesc optDesc ++ "]"
          , s
          , "\\end{description}"
          ]

-- From the actual opt desc we can print out a useful header bit
headerBitOfOptDesc :: OptDescr (CliOpt a) -> String
headerBitOfOptDesc (Option shortNames longNames args _) =
  ( List.intercalate ", " $ concat [ map mkShort shortNames
                                   , map mkLong longNames
                                   ]
  ) ++ " " ++ argument
  where
  mkShort :: Char -> String
  mkShort c = [ '-', c ]
  mkLong :: String -> String
  mkLong s = "\\ipcflag{" ++ s ++ "}"

  argument :: String
  argument = case args of
               NoArg  _   -> ""
               ReqArg _ s -> s
               OptArg _ s -> "<" ++ s ++ ">"
               

printManualSection :: ManualSection a -> String
printManualSection (header, descs) =
  unlines $ header : (map manualPartOfManualDesc descs)

printManual :: Manual a -> String
printManual = unlines . (map printManualSection)

type Manual a        = [ ManualSection a ]
type ManualSection a = (ManualEntry, [ ManualDesc a ] )
type ManualDesc a    = (ManualEntry, OptDescr (CliOpt a))
type ManualEntry     = String


{-|
  The main argument given to 'System.Console.GetOpt.getOpt',
  it describes the flags or options that the program may be given.
  Each element basically describes how the user gives the argument on the
  command line and a function from a possible option argument to the
  above data type. 
  Given this the 'System.Console.GetOpt.getOpt can return as a part of the
  triple list of the above 'CliOpt' data type.

  These are the baseCliOptions which are common to several or all of
  the ipc-related programs (well okay there may be some specific to 
  just ipc itself). The 'toCli' function below takes a list of this
  kind as input rather than simply using this list, this is such that
  each individual program can add to this list before command-line
  parsing.

  These then are the options for the ipc compiler
-}
baseCliOptions :: [ OptDescr (CliOpt a) ]
baseCliOptions = 
  optDescsOfManual ipcManual

ipcManual :: Manual a
ipcManual =
  [ trivialOptionSection
  , outputOptionsSection
  , staticAnalysisSection
  , analysisKindSection
  , probeSpecificationSection
  , aggregateSection
  , runningHydraSection
  , prismModelSection
  , dizzyModelSection
  , extraOutputSection
  , loggingSection
  , miscelleaneousSection
  ]
   

{-
  The options for the trivial invocations of ipc.
-}
trivialOptionSection :: ManualSection a
trivialOptionSection =
  ( header, entries)
  where
  header = unlines [ "\\subsection{Trivial Invocation Options}"
                   , "These options only apply to the trivial"
                   , "invocations of \\commandNameIpcSmc."
                   ]

  entries =
   [
     ( unlines [ "\\indexipcflag{ version }%"
               , "The command-line:\n"
               , "\\showcommandline{\\ipcflag{version}}"
               , "will print the version of the \\commandNameIpcSmc"
               , "compiler and exit."
               ]
     , Option "v"     [ "version" ]      ( NoArg CliVersion )        
        "show version info"
     )
   , ( unlines [ "\\indexipcflag{ help }%"
               , "The command-line\n"
               , "\\showcommandline{\\ipcflag{ help}}"
               , ""
               , "will print an option and usage summary and exit."
               ]
     , Option "h"     [ "help" ]         ( NoArg CliHelp )
        "show options and documentation"
     )
   ]


{-
  These options deal with re-directing the output
-}

outputOptionsSection :: ManualSection a
outputOptionsSection =
  (header, entries)
  where
  header = unlines [ "\\subsection{Output Re-directing Options}"
                   , "By default the \\commandNameIpcSmc\\ command"
                   , "will generate output in the same directory and"
                   , "with a file name based on the input source name"
                   , "The options in this section allow the user to"
                   , "redirect the output from the"
                   , "\\commandNameIpcSmc\\ compiler"
                   ]
  entries =
    [
     ( unlines [ "Sets the output \\texttt{.mod} file which will be"
               , "the input to the run of \\hydra"
               ]
     , Option ""      [ "mod-file" ]       ( ReqArg CliModFileName "FILE" )
       "specify the file to output the mod file to"
     )

   , ( unlines [ "A generic output flag, this works regardless of the kind"
               , "output that \\commandNameIpcSmc\\ is set to produce. For example"
               , "instead of the default \\hydra\\ the compiler may have"
               , "been set to produce a PRISM model file."
               , "The \\ipcflag{output} can be used to set the"
               , "output file name"
               ]
     , Option ""      [ "output" ]         ( ReqArg CliOutput "FILE" )
       "generic flag for specifying the output file"
     )
   , ( unlines [ "When debugging the compiler it can often be useful"
               , "for the output to be redirected to the terminal for"
               , "immediate inspection by the programmer."
               , "The \\ipcflag{stdout} sets the output file to be"
               , "the standard out. This may also prove useful for"
               , "piping the output into further processing tools."
               ]
     , Option ""      [ "stdout" ]         ( NoArg CliStandardOut )
       "instead of an output file write to standard out, useful for debugging"
     )
   , ( unlines [ "When outputting a graph we wish to be able to select"
               , "what kind of output we want."
               , "For this use the \\ipcflag{graph-output} flag"
               ]
     , Option ""      [ "graph-output" ]   (ReqArg parseGraphOutput "GRAPHKIND")
       "Specify the kind of graph: all, none, pdf, png, svg, ps, window"
     )
   , ( unlines [ "When drawing a line graph we can select the width of a line"
               , "using the \\ipcflag{line-width} flag"
               ]
     , Option ""      [ "line-width" ]    (ReqArg (CliLineWidth . read) "WIDTH")
       "Specify the width of lines drawn in the output graphs (if any)"
     )
   ]
   where
   parseGraphOutput :: String -> CliOpt a
   parseGraphOutput "all"    = CliGraphOutput AllGraphs
   parseGraphOutput "none"   = CliGraphOutput NoGraphs
   parseGraphOutput "pdf"    = CliGraphOutput $ OneGraph DrawGraph.PDF
   parseGraphOutput "ps"     = CliGraphOutput $ OneGraph DrawGraph.PS
   parseGraphOutput "eps"    = CliGraphOutput $ OneGraph DrawGraph.PS
   parseGraphOutput "svg"    = CliGraphOutput $ OneGraph DrawGraph.SVG
   parseGraphOutput "png"    = CliGraphOutput $ OneGraph DrawGraph.PNG
   parseGraphOutput "csv"    = CliGraphOutput $ OneGraph DrawGraph.CSV
   parseGraphOutput "window" = CliGraphOutput $ OneGraph DrawGraph.Window
   parseGraphOutput _        = error "Parsing graph output option"

data IpcGraphOutput = AllGraphs
                    | OneGraph DrawGraph.GraphOutput
                    | NoGraphs
                    deriving (Show, Eq)

{-
  These are the options effecting static analysis.
-}
staticAnalysisSection :: ManualSection a
staticAnalysisSection =
  (header, entries)
  where
  header = unlines [ "\\subsection{Detecting Errors}"
                   , "These options control the way that"
                   , "static analysis is performed."
                   ]
  entries =
   [ 
     ( unlines [ "The flag \\ipcflag{staunch} allows the compiler"
               , "to ignore warnings."
               , "By default this flag is off and when \\commandNameIpcSmc\\ performs"
               , "any static-analys is over the input \\pepa\\ model."
               , "The default behaviour is to treat warnings as errors so"
               , "if there are any warnings it will cause the compiler"
               , "to cancel compilation. This behaviour can be suppressed with"
               , "the \\ipcflag{staunch} flag. This will cause the warnings"
               , "to still be emitted but compilation will proceed anyway."
               ]
     , Option ""      [ "staunch" ]      ( NoArg CliStaunch )
       "perform the static analysis but compile even in the face of warnings"
     )
   , ( unlines [ "The flag \\ipcflag{no-static-analysis} causes"
               , "\\commandNameIpcSmc\\ to avoid performing the static analysis over"
               , "the \\pepa\\ model. It will therefore produce no"
               , "warnings or errors. Because of this compilation may fail"
               , "mysteriously and hence the user is advised only to use this"
               , "flag if they know exactly what they are doing and expect"
               , "their model to fail static-analysis for some reason but"
               , "wish to proceed to compilation anyway."
               ]
     , Option ""      [ "no-static-analysis" ] ( NoArg CliNoStaticAnalysis )
       "do not perform static analysis over the model"
     )
   , ( unlines [ "The flag \\ipcflag{allow-self-loops} does exactly as it"
               , "is named. We allow a model containing self-loops."
               , "This can allow the correct calculation of apparent"
               , "rates, but note that the self-looping activities"
               , "will be dropped (hence your throughput may be wrong."
               ]
     , Option ""     [ "allow-self-loops" ]    ( NoArg CliAllowSelfLoops )
       "do not fail if we detect a self-loop"
     )
   , ( unlines [ "The flag \\ipcflag{allow-deadlocks} does exactly as it"
               , "is named. We allow a model which has deadlocked states."
               , "Mostly this is only useful for transient analysis so for"
               , "transient analysis it is the default. It is however also"
               , "possible to use this (usefully) for passage-time/end"
               , "analyses in which there is a single source state"
               ]
     , Option ""    [ "allow-deadlocks" ]      (NoArg CliAllowDeadLocks)
       "Allow deadlocked states, generally only useful for transient analysis"
     )
   ]


{-
  Options for specifying the kind of performance analysis we wish to perform.
-}
analysisKindSection :: ManualSection a
analysisKindSection =
  (header, entries)
  where
  header = unlines [ "\\subsection{Performance Analysis Kind}"
                   , "In this section the options for specifying the"
                   , "different kinds of performance analysis are discussed."
                   ]
  entries =
    [ ( unlines [ "Use this option to calculate the average response time"
                , "The response-time will be the time that the probe is"
                , "in the running state. This probe may be specified via"
                , "the \\ipcflag{probe} argument or implicitly from a set"

                , "of source actions to a set of target actions."
                ]
      , Option ""     [ "average-response" ] ( NoArg CliAverageResponse )
        "Calculate the average response-time"
      )
    , ( unlines [ "Specifies that we should perform a passage-time measurement." 
                , "This is the default. It also means that the passage-probe"
                , "is added to the model. This probe is in addition to the"
                , "master probe. It is used as the start condition of passage."
                , "The passage-probe has two states an 'on' and 'off' state."
                , "It will be in the 'on' state for exactly one state after the"
                , "master-probe has switched from the 'Stopped' to the 'Running'"
                , "state and 'off' otherwise."
                ]
      , Option ""      [ "passage" ]       ( NoArg CliPassage )
        "perform a passage-time performance measurement (default)"
      )

    , ( unlines [ "When specifying a passage-time measurement state that the"
                , "probability density function should be calculated"
                ]

      , Option ""      [ "pdf" ]           ( NoArg $ CliPassageFunction "pdf" )
        ( "Calculate the probability density function of a passage" )
      )

    , ( unlines [ "When specifying a passage-time measurment state that"
                , "the cummulative distribution function of the passage"
                , "should be calculated."
                , "This is the default however should you desire both the cdf"
                , "and the pdf, then --cdf --pdf is what you want."
                ]
      , Option ""      [ "cdf" ]           ( NoArg $ CliPassageFunction "cdf" )
        ( "Calculate the cummulative distribution function of a passage" )
      )

    , ( unlines [ "Specifies a special kind of passage-time calculation"
                , "in which we compare the likelihood of completing a passage"
                , "by the different stop-actions at each time."
                , "For example we may say that if we complete a request by"
                , "time t then we have a ninety-percent chance that we"
                , "completed the request via a cache but at time 2t there is a"
                , "fifty percent chance that any request completed by then has"
                , "been served by the cache"
                ]
      , Option ""     [ "passage-end" ]   ( NoArg CliPassageEnd )
        ( "Perform a passage-end measurement" )
      )

    , ( unlines [ "A passage-end measurement is usually normalised against"
                , "the probability of completing the passage at all."
                , "To suppress this and produce non-normalised results use"
                , "the \\ipcflag{no-normalise} flag."
                , "This will mean for example that you will get cdfs which"
                , "do not climb to one."
                ]
      , Option  ""     [ "no-normalise" ] (NoArg CliNoNormalise )
        ( "Do not normalise passage-end probability functions" )
      )
    , ( unlines [ "Specifies that we should perform a steady-state analysis."
                , "By default this will measure the probability that the probe"
                , "is in the 'Running' state."
                ]
      , Option ""      [ "steady"  ]       ( NoArg CliSteady )
        "perform a steady-state measurement"
      )

    , ( unlines [ "Specifies that we should perform a transient analysis" ]
      , Option ""      [ "transient" ]     ( NoArg CliTransient )
        "perform a transient analysis measurement"
      )

    , ( unlines [ "Specifies that we should perform a 'count' measure."
                , "This option takes as argument the (comma separated) list of"
                , "action name we are expected to count."
                , "This will cause hydra to return the average rate that the model"
                , "performs any of the specified actions."
                , "Note that the specified action can be a communication label sent"
                , "by a probe specified by the user."
                ]
      , Option ""      [ "count-measure" ] ( ReqArg countActions "ACTIONS" )
        "perform a count measure over the given actions"
      )
       
    , ( unlines [ "When performing a steady-state measurement rather than"
                , "specifying the state of interest via a sequence of"
                , "actions (ie, with a probe) sometimes it is desirable to"
                , "specify it with reference to the actual states of the"
                , "components within the model. The argument to this flag"
                , "is the a state expression usually something like: "
                , "$P3 > 0 \\&\\& P2 == 0$."
                ]
      , Option ""      [ "state-measure" ] (ReqArg CliState "C-Expression" )
        "perform a steady state analysis with the given state expression"
      )

    , ( unlines [ "Suppresses the output of any measurement specification"
                , "This is generally useful if the user wishes to hand-modify"
                , "the output \\texttt{.mod} file before running hydra over it."
                , "% Therefore this option is generally run in tandem with the"
                , "% --no-run-hydra option."
                ]
      , Option ""      [ "no-measurement" ]  ( NoArg CliNoMeasurement )
        "do not output a performance measurement specification"
      )
    ]
  countActions = CliCount . (parseCommaSeparatedLowers countError)
  countError   = "error whilst parsing a list of specified count actions"


{-
  The options for probe specification.
-}
probeSpecificationSection :: ManualSection a
probeSpecificationSection =
  (header, entries)
  where
  header = unlines [ "\\subsection{Probe Specification Options}"
                   , "Measurement of models is performed using automatically"
                   , "generated process algebra components called"
                   , "\\emph{stochastic probes}"                   
                   , "This section describes the options which control"
                   , "the performance measure specification probes which"
                   , "are added to the model."
                   , "% For more information on this please see section ..."
                   ]
  entries =
   [
     ( unlines [ "The flag \\ipcflag{ probe } with the shortened version"
               , "\\texttt{p} is used to give a full probe specification."
               , "% In the probe specification language given in section ??"
               ]
     , Option "p"     [ "probe" ]        ( ReqArg CliProbeSpec "PROBESPEC" )
       "specify a performance evaluation probe"
     )
   , ( unlines [ "The flag \\ipcflag{ no-master } is used to specify that"
               , "no master probe % as described in section ..."
               , "should be automatically added to the model."
               ]
     , Option ""      [ "no-master" ]    ( NoArg CliNoMasterProbe )
       "do not automatically generate and add a master start stop probe"
     )
   , ( unlines [ "The flags \\ipcflag{ source } and \\ipcflag{ target }"
               , "with the short versions \\texttt{s} and \\texttt{t}"
               , "respectively, set the state switching actions used"
               , "in the master probe. If other probes are added using the"
               , "\\ipcflag{ probe } option then they may perform immediate"
               , "communication actions which are specified in the source and"
               , "target action list."
               , "Both the \\ipcflag{ source } and \\ipcflag{ target } flags" 
               , "accept as argument a comma separated list of action names."
               ]
     , Option "s"     [ "source" ]       ( ReqArg startActions "ACTIONS" )
       "specify a list of actions to start the master probe (default 'start')"
     )
   , ( "see \\texttt{--source}"
     , Option "t"     [ "target" ]       ( ReqArg stopActions "ACTIONS" )
       "specify a list of actions to stop the master probe (default 'stop')"
     )
   , ( unlines [ "The flags \\ipcflag{source-cond} and \\ipcflag{target-cond}"
               , "allow the specification of a passage-time measurement "
               , "with respect to state conditions rather than action"
               , "observations."
               ]
     , Option ""      [ "source-cond" ]  ( ReqArg CliStartCond "CONDITION" )
       "specify a source state condition"
     )
   , ( "see \\texttt{--source-cond}"
     , Option ""      [ "target-cond" ]  ( ReqArg CliStopCond "CONDITION"  )
       "specify a target state condition"
     )
   , ( unlines[ "Similar to the flag \\ipcflag{source-cond} except that we specify"
              , "that the source condition is just the initial condition of"
              , "the model"
              ]
     , Option ""      [ "source-initial" ] (NoArg CliSourceInitial)
       "Specify that the initial state is the one and only source state"
     )
   ]
   where
   startActions = CliStartActions . (parseCommaSeparatedLowers startError)
   startError   = "error whilst parsing a list of specified start actions"
   stopActions  = CliStopActions  . (parseCommaSeparatedLowers stopError)
   stopError    = "error whilst parsing a list of specified stop actions"


{-
  The command-line options for aggregation
-}
aggregateSection :: ManualSection a
aggregateSection =
  (header, entries)
  where
  header = unlines [ "\\subsection{Aggregation}"
                   , "The options in this section control the aggregation"
                   , "of components."
                   , "Currently in \\commandNameIpcSmc\\ a component may"
                   , "only be aggregated if the user explicitly writes it"
                   , "as a component array of the form $P[N]$."
                   ]
  -- Note it is the module @Language.Transform.Simplify@ in the @simplify@
  -- function which 'sets' the default behaviour. Either it defaults to
  -- non-aggregation and checks for the @--aggregate@ flag or it defaults
  -- to aggregation and checks for the @--no-aggregate@ flag. Currently as
  -- I write this, it is the former, but please check in that module.
  entries =
    [ ( unlines [ "Tells \\commandNameIpcSmc\\ to aggregate process arrays" ]
      , Option "" [ "aggregate" ]   (NoArg CliAggregate)
        "Tells \\commandNameIpcSmc\\ to aggregate process arrays"
      )
      , ( unlines [ "Tells \\commandNameIpcSmc\\ \\textbf{not} to aggregate"
                  , "process arrays. Therefore $P[3][a]$ will be translated"
                  , "into the form $P <a> P <a> P$."
                  ]
      , Option "" [ "no-aggregate" ] (NoArg CliNoAggregate)
        "Tells \\commandNameIpcSmc\\ *not* to aggregate"
      )
    , ( unlines [ "Tells the state space generator to quit after reaching a"
                , "given limit in the state space size"
                ]
      , Option "" [ "limit" ] (ReqArg (CliSpaceLimit . read) "SIZE")
        "The state space generator will quit once 'SIZE' states are reached"
      )
    ]


{-
  The command-line options for running hydra automatically.
-}

runningHydraSection :: ManualSection a
runningHydraSection =
  (header, entries)
  where
  header = unlines [ "\\subsection{Running Hydra}"
                   , "This section details options used for running"
                   , "the hydra tool after processing of the model by \\ipc."
                   ]
  entries =
   [ ( unlines [ "The flag \\ipcflag{run-hydra} causes \\commandNameIpcSmc\\ to"
               , "automatically run the hydra tool on the produced"
               , "\\texttt{.mod} file."
               , "This option has been deprecated please use the"
               , "\\ipcflag{hydra-stage} option described below."
               ]
     , Option ""      [ "run-hydra" ]      (NoArg CliRunHydra )
       "DEPRECATED: After compilation to a mod file, run hydra on the mod file"
     )
   , ( unlines [ "If the hydra tool is not installed in a standard"
               , "location the path to the \\hydra\\ executable can"
               , "be given as an argument to the \\ipcflag{hydra} option."
               ]
     , Option ""       [ "hydra" ]          (ReqArg CliHydra "PATH")
       "Provide the path to hydra if it is not in the system path"
     )
   , ( unlines [ "Choose at which stage we should stop using ipc"
               , "and switch over to hydra."
               , " This option replaces the older \\ipcflag{run-hydra} option."
               , "There are currently two stages which may be specified:"
               , "\\begin{itemize}"
               , "\\item 'mod' this outputs a hydra model file in the original"
               , "format. The full state space is not computed by \\ipc."
               , "\\item 'flat-mod' here the hydra model file contains a full"
               , "state space and as such does not suffer from a problem of"
               , "very large rate expressions."
               , "\\end{itemize}"
               ]
     , Option ""       [ "hydra-stage" ]    ( ReqArg CliHydraStage "STAGE" )
       "Tell \\commandNameIpcSmc\\ which stage to stop and run hydra at"
     )
   ]

{- 
  Some options for generating prism models.
-}
prismModelSection :: ManualSection a
prismModelSection =
  (header, entries)
  where
  header = unlines [ "\\subsection{Prism Model Files}"
                   , "At some point we wish to be able to generate prism"
                   , "model files from Pepa model descriptions."
                   , "All of the options described in this section should be"
                   , "considered experimental and not for general use."
                   ]
  entries =
   [ ( unlines [ "\\ipcflag{prism} this is an experimental option to produce"
               , "a prism model."
               , "When using this one specifies a stage to which we with to"
               , "to compile the pepa model to."
               , "Currently there are two supported options \"trans\" and"
               , "\"model\". The 'model' option should be considered not to be"
               , "working since it is in a very early stage of development"
               , "The 'trans' stage will compile the model down to an"
               , "explicit state space which prism can then import."
               ]
     , Option ""      [ "prism" ]          ( ReqArg CliPrism "STAGE" )
       "Generate a prism model at the stage given; model or trans"
     )
   , ( unlines [ "The \\ipcflag{prism-file} option only has an effect if the"
               , "\\texttt{--prism} flag is set. It redirects the output"
               , "prism model to the specified file."
               ]
     , Option ""      [ "prism-file" ]     ( ReqArg CliPrismFileName "FILE" )
       "specify the file to output the prism model to"
     )
   , ( unlines [ "The \\ipcflag{prism-options} flag has the effect to"
               , "append the provided string on to the end of the"
               , "the prism command"
               ]
     , Option ""     [ "prism-options" ]   ( ReqArg CliPrismOptions "OPTIONS" )
       "specify options to the prism command"
     )
               
   ]


{-
  Some options for generating dizzy models
-}
dizzyModelSection :: ManualSection a
dizzyModelSection =
  (header, entries)
  where
  header = unlines [ "\\subsection{Dizzy Model Files}"
                   , "At some point we wish to be able to generate dizzy"
                   , "model files from Pepa model descriptions."
                   , "The options in this section describe the control of"
                   , "the \\pepa\\ $\\rightarrow$ dizzy translation."
                   ]
  entries =
   [ ( unlines [ "\\ipcflag{dizzy} this is an experimental option to produce"
               , "a dizzy model. This should not be considered working"
               ]
     , Option ""      [ "dizzy" ]          ( NoArg CliDizzy )
       "Generate a dizzy model"
     )
   , ( unlines [ "The \\ipcflag{dizzy-file} option only has an effect if the"
               , "\\texttt{--dizzy} flag is set. It redirects the output"
               , "dizzy model to the specified file."
               ]
     , Option ""      [ "dizzy-file" ]     ( ReqArg CliDizzyFileName "FILE" )
       "specify the file to output the dizzy model to"
     )    
   ]


{-
  Some options for generating extra output (generally in comments)
  to the output file.
-}
extraOutputSection :: ManualSection a
extraOutputSection =
  (header, entries)
  where
  header = unlines [ "\\subsection{Extra Output}"
                   , "The model undergoes various transformations and"
                   , "augmentations on its way to being compiled."
                   , "Sometimes it is helpful to see these intermediate"
                   , "stages. The options in this section allow the user"
                   , "to specify that \\commandNameIpcSmc\\ should"
                   , "include a particular intermediate model in the output,"
                   , "generally inside comments of the output file."
                   ]
  entries =
    [ ( unlines [ "Specifies that \\commandNameIpcSmc\\ should show the"
                , "simplified model. This is a model without any of the"
                , "syntactic sugar which the user may use for convenience"
                , "but are unnecessary for the compilation procedure"
                ]
      , Option ""      [ "show-simplified" ] (NoArg CliShowSimplified)
        "Show the simplified model in the comments of the compiled file"
      )
    , ( unlines [ "Specifies that the model with the measurement probe"
                , "components added and then simplified is shown."
                ]
      , Option ""     [ "show-probed" ]     (NoArg CliShowProbed)
        "Show the model with the measurement probes added"
      )
    ]

{-
  Some options for controlling what gets logged and how the logging
  works (ie to a file or standard out)
-}
loggingSection :: ManualSection a
loggingSection =
  (header, entries)
  where
  header = unlines [ "\\subsection{Logging Options}"
                   , "The options in this section affect what, if anything"
                   , "is logged and how it is logged"
                   ]
  entries =
    [ ( unlines [ "Specifies that the logging information should be shown to stdout"
                ]
      , Option ""      [ "show-log" ] (NoArg CliShowLog)
        "Show the logging information on standard out"
      )
    , ( unlines [ "The flag \\ipcflag{log} specifies that a log should be kept"
                , "Each log entry is associated with a name and the argument"
                , "to the \\ipcflag{log} flag is a name specifying that that"
                , "entry should be output to the log file."
                , "The name 'all' specifies that all log entries"
                , "should be recorded in the log-file."
                , "Additionally the 'user' name outputs to the log items"
                , "deemed relevant to the user while 'developer' is a set"
                , "of log items relevant for the developer."
                ]
      , Option ""      [ "log" ] (ReqArg CliLog "categories")
        "Log the default selection of intermediate data structures and properties"
      )
    ]

{-
  A bunch of miscelleanous entries which don't fit anywhere else.
  Or more rather which have yet to be fitted in elsewhere.
-}
miscelleaneousSection :: ManualSection a
miscelleaneousSection =
  (header, entries)
  where
  header = unlines [ "\\subsection{Miscellaneous Options}"
                   , "This section describes options which do not fit"
                   , "under any of the previous sub-sections."
                   ]
  entries =
   [ ( ""
     , Option ""      [ "steady-mean" ]     ( NoArg CliSteadyMean )
       "use the 'mean' estimator in a steady-state measure"
     )
   
   , ( "" 
     , Option ""      [ "steady-variance" ] ( NoArg CliSteadyVariance )
       "use the 'variance' estimator in a steady-state measure"
     )

   , ( ""
     , Option ""      [ "steady-stddev" ]   ( NoArg CliSteadyStdDev )
       "use the 'stddev' estimator in a steady-state measure"
     )

   , ( ""
     , Option ""      [ "steady-distrib" ]  ( NoArg CliSteadyDistrib )
       "use the 'distribution' estimator in a steady-state measure"
     )

   , ( ""
     , Option ""      [ "start-time" ]   ( ReqArg (CliStartTime . read) "TIME" )
       "specify a time at which to start a performance measure eg passage-time"
     )

   , ( ""
     , Option ""      [ "stop-time" ]   ( ReqArg (CliStopTime . read) "TIME" )
       "specify a time at which to stop a performance measure eg passage-time"
     )

   , ( ""
     , Option ""      [ "time-step" ]   ( ReqArg (CliTimeStep . read) "TIME" )
       "specify a the time steps for a performance measurement"
     )

   , ( ""
     , Option ""      [ "solver" ]      ( ReqArg CliSolver "SOLVER" )
       "specify which solution method to use/specify to hydra"
     )

   , ( ""
     , Option ""      [ "rate" ]        ( ReqArg parseRateOpt "DOUBLE" )
       "Override/specify a rate value on the command-line"
     )

   , ( ""
     , Option ""      [ "rename-proc" ]  (ReqArg CliReplaceProcess "P=s" )
       "cause a renaming on the given process within the model"
     )
   
   , ( ""
     , Option ""      [ "rename-rate" ]  (ReqArg CliReplaceRate "r=s" )
       "cause a renaming on the given rate within the model"
     )

{-     
   , ( unlines [ "Provide a file with transformation rules to change"
               , "to automatically transform the PEPA model"
               ]
     , Option ""      [ "transform-file" ] (ReqArg CliTransformFile "FILE")
       "Provide a transformation rules file"
     )
-}
   , ( unlines [ "Provide a transformation rule with which"
               , "to automatically transform the PEPA model"
               ]
     , Option ""      [ "transform-rule" ] (ReqArg CliTransformRule "RULE")
       "Provide a transformation rule"
     )

   , ( ""
     , Option ""      [ "prioritise" ] ( ReqArg priorityActions "ACTIONS" )
       "increase the priority of the given actions"
     )
     
   , ( "Produce a .dot file of the state space and run the dot program over it"
     , Option ""     [ "dot-file" ]   (NoArg CliDot)
       "Produce a graph of the state space using 'dot'"
     )

   , ( "Produce a .pddl file of the state space"
     , Option ""     [ "pddl" ] (NoArg CliPddl)
       "Produce a .pddl of the state space"
     )

   , ( "Produce a java class file implementing a custom simulation"
     , Option ""     [ "java-simulator" ] (NoArg CliJavaSimulator)
       "Produce a java class file implementing a custom simulation"
     )

   , ( "When producing a java simulator specify the number of runs"
     , Option ""     [ "simulations" ] (ReqArg (CliSimulations. read) "RUNS" )
       "When producing a java simulator specify the number of runs"
     )


   , ( unlines [ "When producing a state space for external output"
               , "do not remove the vanishing states"
               ]
     , Option ""     [ "no-reduce-vanishing" ] (NoArg CliNoReduceVanishing)
       "Do not remove the vanishing states from the output state space"
     )

   , ( ""
     , Option ""      [ "no-reduce-rate-exps" ] ( NoArg CliNoReduceRateExps )
       "do not reduce the rate expressions"
     )
   
   , ( ""
     , Option ""      [ "hide-non-coop" ]       ( NoArg CliHideNonCoop )
       ("hide any activities which a component performs " ++
         "but does not cooperate over")
     )

   , ( ""
     , Option ""      [ "process-num" ]   (ReqArg (CliProcessNum . read) "NUM" )
       "provide a process number which is used to select rates and processes"
     )
   
   , ( unlines [ "The \\ipcflag{fsp} flag is an experimental option to produce"
               , "an LTSA model. This should not be considered working"
               ]
     , Option ""      [ "fsp" ]            ( NoArg CliFsp )
       "Generate a finite state processes (FSP) model"
     )
   , ( unlines [ "The flag \\ipcflag{states-size} informs \\commandNameIpcSmc\\ to"
               , "print out the size of the state space of the given model"
               ]
     , Option ""     [ "states-size" ]     ( NoArg CliStatesSize )
       "Display the size of the state-space for the given model"
     )
   , ( unlines [ "The flag \\ipcflag{estimate-size} asks \\commandNameIpcSmc\\ to simply"
               , "estimate the final state space size of the input model"
               ]
     , Option ""     [ "estimate-size" ]   ( NoArg CliEstimateSize )
       "Display the *estimated* final state-space size of the model"
     )
   , ( unlines [ "The flag \\ipcflag{compare-pepato} informs \\commandNameIpcSmc\\ to"
               , "produce a steady-state analysis and run pepato over"
               , "the model to also produce a steady-state analysis and"
               , "compare the two reports."
               ]
     , Option ""     [ "compare-pepato" ]  ( NoArg CliComparePepato )
       "Compare a steady-state analysis with pepato output"
     )
   , ( unlines [ "The flag \\ipcflag{expermental} is for developers only"
               , "This is a generic flag meaning:"
               , "\"enable a new approach/version of ..\""
               , "Generally only to be used by ipc developers."
               , "So for example a new approach to state space generation,"
               , "rather than inventing a new flag"
               , "\"--use-new-state-space-gen\" we can just test for this."
               , "If later we decide that we do actually wish to have both"
               , "*then* we can invent two flags for it."
               ]
     , Option ""     [ "experimental" ] (NoArg CliExperimental)
       "Use experimental new features"
     )
   ]
   where  
   parseRateOpt :: String -> CliOpt a
   parseRateOpt = CliRate . parseRateOption

   priorityActions :: String -> CliOpt a
   priorityActions = CliPrioritise . (map defaultise) .
                      (parseCommaActionsMaybeAssigns prioritiseErr)
   prioritiseErr   = "error whilst parsing a priority option"
   defaultise :: (ActionIdentifier, Maybe ParsedPriority)
              -> (ActionIdentifier, ParsedPriority)
   defaultise = second $ Maybe.fromMaybe defaultIncreasedPriority



{-| Returns whether the /staunch/ probe is set or not -}
getStaunch :: [ CliOpt a ] -> Bool
getStaunch options =
   not $ null [ True | CliStaunch <- options ]

{-| Returns whether or not static analysis is to be performed on the model -}
getStaticAnalysis :: [ CliOpt a ] -> Bool
getStaticAnalysis options =
   null [ True | CliNoStaticAnalysis <- options ]

{-| Return the probe specifications from a list of @CliOpt@ s -}
getProbeSpecs :: [ CliOpt a ] -> [ String ]
getProbeSpecs = 
   mapMaybe isProbe 
   where
   isProbe :: CliOpt a -> Maybe String
   isProbe ( CliProbeSpec s) = Just s
   isProbe _                 = Nothing

{-| Returns whether or not a master start stop probe should be generated -}
getMasterBool :: [ CliOpt a ] -> Bool
getMasterBool = 
   not . (any isCliNoMaster)


isCliNoMaster :: CliOpt a -> Bool
isCliNoMaster CliNoMasterProbe = True
isCliNoMaster _                = False

{-| Returns whether or not *ipc* should add any measurement
    probes\/specifications.
   Basically it's a bit poor that I have to have this here,
   it sort of underhandedly predicts whether or not ipc will
   add a Master Probe, if not then there is no way to know
   what to measure for.
-}
shouldAddMeasurements :: [ CliOpt a ] -> Bool
shouldAddMeasurements options =
   (getMasterBool options) &&
   any isMeasurementRelated options

isMeasurementRelated :: CliOpt a -> Bool
isMeasurementRelated ( CliProbeSpec _ )    = True
isMeasurementRelated ( CliStartActions _ ) = True
isMeasurementRelated ( CliStopActions _ )  = True
isMeasurementRelated ( CliCount _ )        = True
isMeasurementRelated ( CliState _ )        = True
isMeasurementRelated ( CliStartCond _)     = True
isMeasurementRelated ( CliStopCond _)      = True
isMeasurementRelated _                     = False

{-| Returns the list of specified start actions -}
getStartActions :: [ CliOpt a ] -> [ String ]
getStartActions options =
   concat [ a | CliStartActions a <- options ]

{-| Returns the list of specified stop actions -}
getStopActions :: [ CliOpt a ] -> [ String ]
getStopActions options =
   concat [ a | CliStopActions a <- options ]


{-| Returns the list of rate overrides -}
getRateOptions :: [ CliOpt a ] -> [ (RateIdentifier, Double) ]
getRateOptions options =
   [ spec | CliRate spec <- options ]



{-| Returns all the process replacements -}
getProcessRenames :: [ CliOpt a ] -> Either ParseError ProcessRenameMap
getProcessRenames =
   joinParseErrorList . (mapMaybe makeRename)
   where
   makeRename :: CliOpt a -> Maybe (Either ParseError ProcessRename)
   makeRename (CliReplaceProcess s) = 
      Just $ parseProcessRename "command-line" s
      
   makeRename _                     = Nothing

{-| Returns all the rate name replacements -}
getRateRenames :: [ CliOpt a ] -> Either ParseError RateRenameMap
getRateRenames =
   joinParseErrorList . (mapMaybe makeRename)
   where
   makeRename :: CliOpt a -> Maybe (Either ParseError RateRename)
   makeRename (CliReplaceRate s) =
      Just $ parseRateRename "command-line" s
   makeRename _                  = Nothing


{-| Return the list of actions which should be prioritised in the model -}
getPrioritised :: [ CliOpt a ] -> [ (ActionIdentifier, ParsedPriority) ]
getPrioritised options =
   concat [ a | CliPrioritise a <- options ]

{-| Returns whether or not the option to hide all non cooperating
    activities is set
-}
containsHideNonCoop :: [ CliOpt a ] -> Bool
containsHideNonCoop =
   any isHideNonCoop
   where
   isHideNonCoop :: CliOpt a -> Bool
   isHideNonCoop CliHideNonCoop = True
   isHideNonCoop _              = False

{-| Returns the process number option closest to the front of the list -}
getProcessNumber :: [ CliOpt a ] -> Maybe Int
getProcessNumber []                     = Nothing
getProcessNumber (CliProcessNum i : _ ) = Just i
getProcessNumber (_ : rest)             = getProcessNumber rest

{-| Returns the output mod file, based on the command line options and
    the input pepa file. That is if the command line options specify a
    mod-file to output to then use that, otherwise replace the .pepa
    suffix with the .mod suffix
-}
getModFileName :: FilePath -> [ CliOpt a ] -> FilePath
getModFileName pepaFileName options =
   case [ f | CliModFileName f <- options ] ++
        [ f | CliOutput f <- options ] 
   of
      []      -> Utils.switchExtension ".pepa" ".mod" pepaFileName
      [ one ] -> one
      _       -> error "More than one .mod file specified on the command line"

{-| Returns the output prism-file name based on the command line options
   and on the input pepa-file. Exactly equivalent to 
   'getModFileName'
-}
getPrismFileName :: FilePath -> [ CliOpt a ] -> FilePath
getPrismFileName pepaFileName options =
   case [ f | CliOutput f <- options ] ++
        [ f | CliPrismFileName f <- options ] 
   of
      []      -> Utils.switchExtension ".pepa" ".sm" pepaFileName
      [ one ] -> one
      _       -> error "more than one output prism file specified"


{-| Returns the output dizzy-file name based on the command line options
   and on the input pepa-file. Exactly equivalent to 
   'getModFileName'
-}
getDizzyFileName :: FilePath -> [ CliOpt a ] -> FilePath
getDizzyFileName pepaFileName options =
   case [ f | CliOutput f <- options ] ++
        [ f | CliDizzyFileName f <- options ] 
   of
      []      -> Utils.switchExtension ".pepa" ".dizzy" pepaFileName
      [ one ] -> one
      _       -> error "more than one output dizzy file specified"


{-| Gets the generic @--output@ flag's argument if there is one -}
getOutputFileName :: [ CliOpt a ] -> Maybe FilePath
getOutputFileName options =
   case [ name | CliOutput name <- options ] of
      [ one ] -> Just one
      []      -> Nothing
      _       -> 
         error "more than one output file specified on the command-line"

{-|
   Returns a solution control based on the command line options.
-}
getSolutionControl :: [ CliOpt a ] -> DNAMsolutionControl
getSolutionControl options = 
   DNAMsolution { solutionMethod = method }
   where
   method = case [ s | CliSolver s <- options ] of
               []    -> defaultSolutionMethod
               [ s ] -> parseSolutionMethod s
               _     -> error "More than one solution method given"

{-
isCliSolutionControl :: CliOpt a -> Bool
isCliSolutionControl (CliSolver _) = True
isCliSolutionControl _             = False
-}

{-| 
   Returns the performance measurement specifications by examining
   the command-line arguments. The type of this will likely change
   when we make it possible to specify measurements other than a
   single passage-time measurement, for now though this will do.
-}
getMeasurementSpecs :: [ CliOpt a ] -> [ DNAMperformMeasure ]
getMeasurementSpecs options
   | any isCliNoMeasure          options = []
   | not $ shouldAddMeasurements options = []
   | any isCliTransient          options =
      -- Should transient maybe use the same start and stop conditions as
      -- passage-time analysis? if so then we need to remember to add the
      -- passage-probe.
      [ DNAMtransient 
        DNAMtransientMeasure { transientSourceCond = DNAMprocpresent runningId
                             , transientTargetCond = DNAMprocpresent stoppedId
                             , transientStartTime  = startTime
                             , transientStopTime   = stopTime
                             , transientStepTime   = timeStep
                             }
      ]
   | any isCliSteady options       =
      [ DNAMsteady
        DNAMsteadyMeasure { steadyMeasureName = Unqualified "steady_measure"
                          , steadyEstimator   = estimator
                          , steadyCondition   = DNAMprocpresent runningId
                          }
      ]
   | not $ null countActions       =
      [ DNAMcount 
        DNAMcountMeasure { countMeasureName = Unqualified "count_measure"
                         , countTransitions = countActions }
      ]
   
   | not $ null stateMeasures      =
      [ DNAMsteady 
        DNAMsteadyMeasure { steadyMeasureName = Unqualified "state_measure"
                          , steadyEstimator   = estimator
                          , steadyCondition   = DNAMcond stateCond
                          }
      ]

   | (not $ null cliStartConds) &&
     (not $ null cliStopConds)     =
      [ DNAMpassageCond 
        DNAMpassageMeasure { passageSourceCond = cliStartCond
                           , passageTargetCond = cliStopCond
                           , passageStartTime  = startTime
                           , passageStopTime   = stopTime
                           , passageStepTime   = timeStep
                           }
      ]
   | otherwise                     =
      -- The default is a passage time measurement
      [ DNAMpassage 
        DNAMpassageMeasure { passageSourceCond = passageStartCond
                           , passageTargetCond = passageStopCond
                           , passageStartTime  = startTime
                           , passageStopTime   = stopTime
                           , passageStepTime   = timeStep
                           }
      ]
   where
   -- Careful, these must match up with those defined in
   -- Language.Pepa.Probes.AddProbes, in fact that should
   -- really export them, the point is though I think that
   -- they should be 'specifiable' on the command-line, so
   -- they shouldn't really be defined there.
   stoppedId        = Unqualified "ProbeStopped"
   runningId        = Unqualified "ProbeRunning"
   -- passageRunId     = Unqualified "PassageProbeRunning"

   passageStartCond = DNAMprocpresent runningId
   passageStopCond  = DNAMprocpresent stoppedId


   -- If the user creates these themselves then great, slightly easier for us.
   -- The foldr1 are safe since the *cond should not be used if *conds is null.
   cliStartCond     = DNAMcond $ foldr1 Cand cliStartConds
   cliStartConds    = [ parseCExpr s | CliStartCond s <- options ]
   cliStopCond      = DNAMcond $ foldr1 Cand cliStopConds
   cliStopConds     = [ parseCExpr s | CliStopCond s <- options ]

   -- If none of the time options are specified we should have defaults of
   -- start-time = 1.0
   -- stop-time  = 10.0
   -- time-step  = 0.5
   startTime       = case List.find isCliStartTime options of
                        Just (CliStartTime d)  -> d
                        Just _                 -> error weirdError
                        Nothing                -> 1.0
   stopTime        = case List.find isCliStopTime options of
                        Just (CliStopTime d)   -> d
                        Just _                 -> error weirdError
                        Nothing                -> 10.0 * startTime
   timeStep        = case List.find isCliTimeStep options of
                        Just (CliTimeStep d)   -> d
                        Just _                 -> error weirdError
                        Nothing                -> defaultTimeStep
   defaultTimeStep = (stopTime - startTime) / 18.0

   countActions    = map Unqualified $ concat [ s | CliCount s <- options ]

   stateMeasures   = [ s | CliState s <- options ]
   stateCond       = foldr1 Cand $ map parseCExpr stateMeasures

   -- as usual we should pass back a 'MainControl' rather than use a 'blindRun'
   parseCExpr :: String -> RateExpr
   parseCExpr = blindRun parseErr exprParser 

   parseErr  = unwords [ "Cli.getMeasurementSpecs: you have passed in as a state"
                       , "measure but the condition on the state cannot be parsed"
                       ]

   estimator
      | any isCliSteadyMean     options = EstimatorMean
      | any isCliSteadyVariance options = EstimatorVariance
      | any isCliSteadyStdDev   options = EstimatorStdDeviation
      | any isCliSteadyDistrib  options = EstimatorDistribution
      | otherwise                       = EstimatorMean

   weirdError      = unwords [ "Cli.getMeasurementSpecs:"
                             , "strange error we have searched for"
                             , "a given kind of options but the one"
                             , "found was not of the correct kind"
                             , "I am flabbergasted if this error comes up"
                             , "please do mail me."
                             ]
                             


isCliStartTime :: CliOpt a -> Bool
isCliStartTime (CliStartTime _ ) = True
isCliStartTime _                 = False

isCliStopTime :: CliOpt a -> Bool
isCliStopTime (CliStopTime _ )   = True
isCliStopTime _                  = False

isCliTimeStep :: CliOpt a -> Bool
isCliTimeStep (CliTimeStep _ )   = True
isCliTimeStep _                  = False


isCliSteadyMean :: CliOpt a -> Bool
isCliSteadyMean CliSteadyMean = True
isCliSteadyMean _             = False

isCliSteadyVariance :: CliOpt a -> Bool
isCliSteadyVariance CliSteadyVariance = True
isCliSteadyVariance _                 = False

isCliSteadyStdDev :: CliOpt a -> Bool
isCliSteadyStdDev CliSteadyStdDev = True
isCliSteadyStdDev _               = False

isCliSteadyDistrib :: CliOpt a -> Bool
isCliSteadyDistrib CliSteadyDistrib = True
isCliSteadyDistrib _                = False

getGraphLineWidth :: [ CliOpt a ] -> Double
getGraphLineWidth options
    | ( l : _) <- [ x | CliLineWidth x <- options ] = l
    | otherwise                                     = 1.0 -- default line width

{-|
   The data type returned to indicate which kind of performance
   measurements are to be performed on the model which also indicates
   what should be run after hydra.
-}
data PostHydra =
   PostHydraPassage
 | PostHydraTransient
 | PostHydraSteady
 | PostHydraCount
 | PostHydraNone


{-|
   Returns the kind of analysis that the command-line options specify
   to be performed on the model.
-}
getPostHydra :: [ CliOpt a ] -> PostHydra
getPostHydra options
   | any isCliTransient options = PostHydraTransient
   | any isCliSteady    options = PostHydraSteady
   | any isCliState     options = PostHydraSteady
   | any isCliCount     options = PostHydraCount
   | any isCliNoMeasure options = PostHydraNone
   | otherwise                  = PostHydraPassage


{-| Returns whether or not the command-line options specify that we
    should run hydra after producing the .mod file.
-}
shouldRunHydra :: [ CliOpt a ] -> Bool
shouldRunHydra =
   any suggestsRunHydra
   where
   suggestsRunHydra :: CliOpt a -> Bool
   suggestsRunHydra (CliHydra _)  = True
   suggestsRunHydra (CliRunHydra) = True
   suggestsRunHydra _             = False

{-| Returns the two hydra commands which may have had their paths
   specified on the command-line
-}
getHydraCommands :: [ CliOpt a ] -> (FilePath, FilePath)
getHydraCommands options =
   case [ file | CliHydra file <- options ] of
      [ ]     -> ("hydra-s", "hydra-uniform")
      [ one ] -> (one, File.replaceFileName one "hydra-uniform")
      _       -> error "--hydra specified more than once"


isCliTransient :: CliOpt a -> Bool
isCliTransient CliTransient = True
isCliTransient _            = False

isCliSteady :: CliOpt a -> Bool
isCliSteady CliSteady = True
isCliSteady _         = False

isCliState :: CliOpt a -> Bool
isCliState (CliState _) = True
isCliState _            = False

isCliCount :: CliOpt a -> Bool
isCliCount (CliCount _) = True
isCliCount _            = False

isCliNoMeasure :: CliOpt a -> Bool
isCliNoMeasure CliNoMeasurement = True
isCliNoMeasure _                = False


{-| Returns true if the output is to go to standard out -}
getStandardOut :: [ CliOpt a ] -> Bool
getStandardOut options = not $ null [ True | CliStandardOut <- options ]


{-| Returns all the non-standard arguments, these are those which must have
    been added by the individual program.
-}
getNonStandard :: [ CliOpt a ] -> [ a ]
getNonStandard []                          = []
getNonStandard ( CliNonStandard a : rest ) = a : (getNonStandard rest)
getNonStandard ( _ : rest )                = getNonStandard rest


