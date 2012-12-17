module Language.Ptrees.Evaluate
  ( evaluatePtreeFile
  , evaluatePtreeString
  , evaluateAndDisplay
  , evaluateRootPtree
  , emptyEnvironment
  )
where

{- Standard Library Modules Imported -}
import Control.Arrow
  ( second )
import Control.Monad
  ( liftM )
import Control.Monad.Trans
  ( liftIO )
import qualified Data.List as List
import Data.List
  ( intercalate )
import qualified Data.Map as Map
import Data.Map
  ( Map )
import qualified Data.Maybe as Maybe
import Data.Maybe
  ( fromJust )
import Data.Set
  ( Set )
import qualified System.Directory as Dir
import System.Exit
  ( ExitCode    ( .. ) )
import qualified System.FilePath as File
import System.IO
  ( openFile
  , hClose
  , IOMode    ( .. )
  , hPutStr
  )
{- External Library Modules Imported -}
import qualified Horddes.Odes as Horddes
import qualified Horddes.Solve
import Horddes.Solve
  ( TimeConfig      ( .. ) )
{- Local Modules Imported -}
import Ipc.Cli
  ( IpcGraphOutput  ( .. )
  , CliOpt          ( .. )
  )
import qualified Ipc.Ipc as Ipc
import Ipc.Ipc
  ( MeasureSpecs    ( .. ) )
import qualified Language.Pepa.MainControl as MainControl
import Language.Pepa.MainControl
  ( MainControl
  , IOMainControl
  , liftMC
  )
import qualified Language.Pepa.QualifiedName as Qualified
import qualified Language.Ptrees.Syntax as Ptrees
import Language.Ptrees.Syntax
  ( Result           ( .. )
  , Ptree            ( .. )
  , CompileTarget    ( .. )
  , OdeConfig        ( .. )
  , TranslateTarget  ( .. )
  , Pfunction        ( .. )
  , PlotOptions      ( .. )
  , PlotKind         ( .. )
  , PassageTree
  , PassageQuery     ( .. )
  , AveragePassageQuery ( .. )
  , PassageEndQuery  ( .. )
  , PassageTimes     ( .. )
  , TransientQuery   ( .. )
  , SteadyQuery      ( .. )
  , ThroughPutQuery  ( .. )
  , ModelTree        ( .. )
  , ModelFile        ( .. )
  , relateModelFile
  , RateSpecTree
  , EventTree        ( .. )
  , ActionTree       ( .. )
  , ActionName
  , TransformTree
  , ProbeTree
  , VariableTree     ( .. )
  , Variable
  , Value            ( .. )
  , DataBaseTree     ( .. )
  , GraphInstruction ( .. )
  )
import qualified Language.Ptrees.Print as PtreesPrint
import qualified Language.Ptrees.CandleStick as CandleStick
import qualified Language.Ptrees.TimeSeries as TimeSeries
import qualified Language.Ptrees.Against as Against
import Language.Ptrees.Against
  ( AgainstDataBase )
import qualified Language.Ptrees.DataBase as DataBase
import Language.Ptrees.DataBase
  ( DataBase
  , DataBaseResult ( .. )
  )
import qualified Language.Pepa.Syntax as Pepa
import Language.Pepa.Syntax
  ( ParsedModel )
import qualified Language.Pepa.Utils as Utils
import qualified Language.Pepa.Analysis.Analysis as Analysis
import qualified Language.Pepa.Rates as Rates
import qualified Language.Pepa.Probes.AddProbes as ProbesApply
import qualified Language.Pepa.Transform.Rules.Apply as RulesApply
import qualified Language.Pepa.Transform.Replace as Replace
import qualified Language.Ptrees.Parser as PtreeParser
import qualified Language.Pepa.Print as PepaPrint
import qualified Language.Pepa.FileParser as FileParser
import qualified Language.Pepa.Transform.Simplify as Simplify
import qualified Language.Pepa.Compile.States as States
import qualified Language.Pepa.Compile.GenMatrix as GenMatrix
import qualified Language.Pepa.Compile.EmbeddedMarkov as EmbeddedMarkov
import qualified Language.Pepa.Compile.Uniformise as Uniformise
import Language.Pepa.Compile.Uniformise
  ( uniformiseGenMatrix
  , getPassageTimes 
  )
import Language.Pepa.Compile.MarkovChain
  ( Probability )
import qualified Language.Pepa.Compile.Odes as Odes
import qualified Language.Pepa.Compile.JavaSimulator as JavaSimulator
import qualified Ipc.DrawGraph as DrawGraph
import Ipc.DrawGraph
  ( SimpleLine      ( .. )
  , GraphOutput     ( .. )
  )
import qualified Language.Pepa.Compile.Pddl as Pddl
import qualified Language.BioPepa.Syntax        as BioPepa
import qualified Language.BioPepa.PepaTranslate as BioTranslate
import qualified Language.BioPepa.OdesTranslate as BioPepaOdes
import qualified Language.BioPepa.Parser        as BioParser
{- End of Module Imports -}

typingError :: String -> IOMainControl a
typingError s = fail $ "Typing error: " ++ s


{-| An Environment represents the 'world' in which we are evaluating
    the performance tree. This could for example include some command-line
    options. Currently we hold a possible file path indicating from where
    we got the ptree. This allows the performance tree to speak about
    a file in a directory relative to itself and does not require us to
    be in the same directory as the performance tree file in order to run it.

    Most importantly however the environment stores the mapping from variable
    names to values.

    The 'envExprName' is a concatenation of all the named experimentation values
    contained so far within the tree. This can be used to create a file name.
-}
data Environment a  = Environment { envPtreeFile :: Maybe FilePath
                                  , envMapping   :: Map Variable Value
                                  , envCliOpts   :: Ipc.CliOptions a
                                  , envExprName  :: String
                                  }

emptyEnvironment :: Environment a
emptyEnvironment = Environment { envPtreeFile = Nothing
                               , envMapping   = Map.empty
                               , envCliOpts   = []
                               , envExprName  = ""
                               }
envInsert :: Variable -> Value -> Environment a -> Environment a
envInsert var val env = 
  env { envMapping = Map.insert var val $ envMapping env }

envAddName :: String -> Environment a -> Environment a
envAddName name env
  | null oldName = env { envExprName = name }
  | otherwise    = env { envExprName = concat [ oldName
                                              , "-"
                                              , name
                                              ]
                       }
  where
  oldName = envExprName env

evaluatePtreeFile :: Ipc.CliOptions a -> FilePath -> IO ExitCode
evaluatePtreeFile options file =
  result >>= Ipc.exitFromMainControl options file outputResult
  where
  result :: IO (MainControl Result)
  result = MainControl.runMainControlT $
           PtreeParser.parsePtreeFile file >>=  evaluatePtree initialEnv

  initialEnv = Environment { envPtreeFile = Just file
                           , envMapping   = Map.empty
                           , envCliOpts   = options
                           , envExprName  = ""
                           }
{-| Evaluate a performance tree represented as a string, hence we
    must parse it first
-}
evaluatePtreeString :: Ipc.CliOptions a -> String -> IOMainControl Result
evaluatePtreeString options inputString =
  do ptree <- liftMC $ PtreeParser.parsePtreeString inputString
     evaluateRootPtree Nothing options ptree

evaluateAndDisplay :: Maybe FilePath 
                   -> Ipc.CliOptions a
                   -> Ptree 
                   -> IO ExitCode
evaluateAndDisplay mFile options ptree =
  result >>= Ipc.exitFromMainControl options file outputResult
  where
  file   = Maybe.fromMaybe "" mFile
  result :: IO (MainControl Result)
  result = MainControl.runMainControlT $   
           evaluateRootPtree mFile options ptree

evaluateRootPtree :: Maybe FilePath -> Ipc.CliOptions a
                  -> Ptree -> IOMainControl Result
evaluateRootPtree mFile options ptree = 
  evaluatePtree initialEnv ptree
  where
  initialEnv = Environment { envPtreeFile = mFile
                           , envMapping   = Map.empty
                           , envCliOpts   = options
                           , envExprName  = ""
                           }

evaluatePtree :: Environment a -> Ptree -> IOMainControl Result
evaluatePtree env (Pmodel modelTree)            =
  do reducedModelTree <- evaluateModelTree env modelTree
     (mFile, model)   <- getModel env reducedModelTree
     return $ ModelResult mFile model
evaluatePtree env (Ptranslate modelTree target) = 
  evaluateTranslation env modelTree target
evaluatePtree env (Pthroughput through)         = 
  evaluateThroughput env through
evaluatePtree env (Ppassage passage)
  | compile == CompileToSimulator = 
    evaluatePassageWithJavaSim env passage
  | compile == CompileToCTMC      =
    liftM PassageResult $ evaluatePassage env passage
  | compile == CompileToOdes      =
    fail "Not implemented passage ptrees for compilation to ODEs"
  where
  compile = passageCompile passage
  
evaluatePtree env (PaveragePassage avgQuery)    = 
  liftM AverageResult $ evaluateAveragePassage env avgQuery
evaluatePtree env (Poderesponse times modelTree)= 
  liftM GenericStringResult $ evaluateOdesResponse env times modelTree
evaluatePtree env (Ppassageend passageEnd)      =
  liftM PassageEndResult $ evaluatePassageEndQuery env passageEnd
evaluatePtree env (Ptransient transient)        =
  evaluateTransient env transient
evaluatePtree env (PCdf passageTree)            = 
  liftM GraphData $ evaluateCdf env passageTree
evaluatePtree env (PPdf passageTree)            = 
  liftM GraphData $ evaluatePdf env passageTree
evaluatePtree env (Psteady steady)              =
  evaluateSteady env steady
evaluatePtree env (PStatesSize ptree)           =
  evaluateStateSize env ptree
evaluatePtree env (Pmap pfunction ptree)        =
  do singleResult <- evaluatePtree env ptree
     case singleResult of
       ExperimentResult expresults -> 
         do newResults <- mapM applyExprResult expresults
            return $ ExperimentResult newResults
       MapResult results           ->
         do newResults <- mapM  applyResult results
            return $ MapResult newResults
       _                           ->
         typingError "Cannot map onto given result"
  where
  applyExprResult :: (a, Result) -> IOMainControl (a, Result)
  applyExprResult (a, result) =
    do newResult <- applyResult result
       return (a, newResult)
  applyResult :: Result -> IOMainControl Result
  applyResult (PassageResult pt) =
    case pfunction of
       Pfcdf -> return $ GraphData $ createPassageCdfLines [ ("", pt) ]
       Pfpdf -> return $ GraphData $ createPassagePdfLines [ ("", pt) ]
  applyResult _ =
    typingError "Wrong kind of result to map a passage-time function"
-- A temporary special case of Pplot
evaluatePtree env (Pplot Surface _pOptions (Ppassageend passageend)) =
  do result  <- evaluatePassageEndQuery env passageend
     let contents = createCSVFile result
     return $ FileResult "results-surface.csv" contents
  where
  createCSVFile :: Uniformise.PassageEndResult -> String
  createCSVFile = unlines . map createBlock . zip [0 ..]

  -- This makes a lot of assumptions such as that all of the passage
  -- end results have the same order of 'targets' and that all the
  -- targets have the same times. Of course these *should* be true
  -- but we really need a nicer way to represent passage-end results.
  createBlock :: (Int, (String, Uniformise.PassageResult)) -> String
  createBlock (i, (_name, passageResult)) =
    unlines $ map createLine $ Uniformise.pdfResult passageResult
    where
    createLine :: (Uniformise.Time, Probability) -> String
    createLine (time, prob) =
      intercalate ", " [ show i
                       , show time
                       , show prob
                       ] 
-- A temporary special case of Pexperiment
evaluatePtree env (Pexperiment var values (Ppassageend passageend)) =
  do results  <- mapM getValueResult values
     let contents = createCSVFile results
     return $ FileResult "results.csv" contents
  where
  getValueResult :: (String, Value) 
                 -> IOMainControl ( (Variable, Value)
                                  , Uniformise.PassageEndResult )
  getValueResult (name, value) =
    do r <- evaluatePassageEndQuery innerEnv passageend
       return ((var, value), r)
    where
    innerEnv = envAddName name $ envInsert var value env

  createCSVFile :: [ ((Variable, Value), Uniformise.PassageEndResult) ] -> String
  createCSVFile = unlines . map createBlock

  -- This makes a lot of assumptions such as that all of the passage
  -- end results have the same order of 'targets' and that all the
  -- targets have the same times. Of course these *should* be true
  -- but we really need a nicer way to represent passage-end results.
  createBlock :: ((Variable, Value), Uniformise.PassageEndResult) -> String
  createBlock ((_var, _value), [])      =
    error "Empty passage-end result"
  createBlock ((_var, value), (firstResult : rest)) =
    unlines $ map createLine (Uniformise.pdfResult $ snd firstResult)
    where
    createLine :: (Uniformise.Time, Probability) -> String
    createLine (time, prob) =
      intercalate ", " $ [ PtreesPrint.hprintValue value
                         , show time
                         , show prob
                         ] ++ otherProbs
      where
      otherProbs = map (show . fromJust . (lookup time)) cdfs
      cdfs       = map (Uniformise.pdfResult . snd) rest
evaluatePtree env (Pexperiment var values tree) =
  do results <- mapM getValueResult values
     return $ ExperimentResult results
  where
  getValueResult :: (String, Value) -> IOMainControl ((Variable, Value), Result)
  getValueResult (name, value) =
    do r <- evaluatePtree localEnv tree
       return ((var, value), r)
    where
    localEnv = envAddName name $ envInsert var value env
evaluatePtree env (Plet var argTree bodyTree)   =
  do result <- evaluatePtree env argTree
     let localEnv = envInsert var (Ptrees.valueOfResult result) env
     evaluatePtree localEnv bodyTree
evaluatePtree env (Pplot plotkind pOptions ptree) =
  do dataTree <- evaluatePtree env ptree
     graph    <- liftMC $ plotData plotkind pOptions dataTree
     return $ GraphResult graph $ Just outFile
  where
  -- Okay so we attempt to make up the filename by taking the name
  -- given to us in the plot options. If there is none, then we build
  -- it up on the name given explicitly in the experiment which we are
  -- plotting. If this is also null, then we use the name of the ptree file.
  -- Finally if this is also null then we use 'plot'.
  outFile         = maybe experimentFile addDir $ plotOptsOutputFile pOptions
  -- The name of the experiment with the directory of the ptree file added
  experimentFile  = addDir experimentName
  -- The experiment name is either given within the ptree file
  -- or if this has not been set then we will use the name part of the
  -- ptree file itself.
  experimentName
    | null exprName = File.addExtension ptreeName outputSuffix
    | otherwise     = File.addExtension exprName outputSuffix
  exprName        = envExprName env
  -- Whatever happens we want to add the directory of the ptree file to
  -- the name we make up.
  addDir          = File.combine directory
  directory       = File.takeDirectory ptreeFile
  ptreeName       = File.dropExtension $ File.takeFileName ptreeFile
  -- As a last resort if the above three methods of determining a name
  -- fail, we fall back on 'plot'.
  ptreeFile       = Maybe.fromMaybe "plot" $ envPtreeFile env

  outputSuffix 
    | any isNoGraphs options  = error "No graphs specified???"
    | any isAllGraphs options = error "All graphs not implemented"
    | null graphOutputs       = ".png"
    | otherwise               = suffixOfOutput $ head graphOutputs
    where
    graphOutputs = [ x | CliGraphOutput (OneGraph x) <- options ] ++
                   [ x | Just x <- [ plotOptsOutFormat pOptions ] ]
    -- graphTitle   = Just "Results" 
    options      = envCliOpts env
    isAllGraphs :: CliOpt a -> Bool
    isAllGraphs (CliGraphOutput AllGraphs) = True
    isAllGraphs _                          = False
    -- allGraphKinds = [ PNG, PDF, PS, SVG, CSV ] -- Not window??

    isNoGraphs :: CliOpt a -> Bool
    isNoGraphs (CliGraphOutput NoGraphs) = True
    isNoGraphs _                         = False
    suffixOfOutput :: GraphOutput -> String
    suffixOfOutput PNG    = ".png"
    suffixOfOutput PDF    = ".pdf"
    suffixOfOutput SVG    = ".svg"
    suffixOfOutput CSV    = ".csv"
    suffixOfOutput PS     = ".eps"
    suffixOfOutput Window = error "sorry window has no file extension"

evaluatePtree env (Pplots plots)                  =
  do results <- mapM evaluatePlot plots
     return $ MapResult results
  where
  evaluatePlot :: (PlotKind, PlotOptions, Ptree) -> IOMainControl Result
  evaluatePlot (plotKind, pOptions, ptree) =
    do dataTree <- evaluatePtree env ptree
       graph    <- liftMC $ plotData plotKind pOptions dataTree
       return $ GraphResult graph $ plotOptsOutputFile pOptions
evaluatePtree env (PData dbTree [ AllCandleStick ]) =
  do database <- evaluateDbTree dbTree evaluateThunk
     let candles  = CandleStick.makeAllCandle prefix database
     _candlePdfs  <-liftIO $ mapM CandleStick.makeCandlePdf candles
     return $ ExternalResult ExitSuccess
  where
  prefix = takeWhile (/= '.') $ 
           Maybe.fromMaybe "anonymous" $ envPtreeFile env

  evaluateThunk :: DataBaseResult Result 
                -> IOMainControl Uniformise.PassageResult
  evaluateThunk = evaluateDbResult env evaluateResult

  evaluateResult :: Result -> IOMainControl Uniformise.PassageResult
  evaluateResult (PassageResult pt) = return pt
  evaluateResult res                =
    fail $ unlines [ "Expecting passage time result found:"
                   , show res
                   ]

evaluatePtree env (PData dbTree [ AllTimeSeries ])     =
  do database <- evaluateDbTree dbTree evaluateThunk
     ec       <- liftIO $ TimeSeries.makeAllTimeSeriesPdfs prefix database
     return $ ExternalResult ec
  where
  prefix = takeWhile (/= '.') $ 
           Maybe.fromMaybe "anonymous" $ envPtreeFile env

  evaluateThunk :: DataBaseResult Result 
                -> IOMainControl Uniformise.TransientResult
  evaluateThunk = evaluateDbResult env evaluateResult

  evaluateResult :: Result -> IOMainControl Uniformise.TransientResult
  evaluateResult (TransientResult tr) = return tr
  evaluateResult res                  = 
    fail $ unlines [ "Expecting a transient result found:"
                   , show res
                   ]
evaluatePtree env (PData dbTree [ Against plotOpts rateName mRateName2]) =
  do database <- evaluateDbTree dbTree evaluateThunk
     ec       <- liftIO $ makeGraph mRateName2 database
     return $ ExternalResult ec
  where
  prefix = takeWhile (/= '.') $ 
           Maybe.fromMaybe "anonymous" $ envPtreeFile env

  makeGraph :: Maybe String -> AgainstDataBase ->IO ExitCode
  makeGraph Nothing database          = 
    Against.makeAgainstPdf plotOpts prefix rateName database
  makeGraph (Just rateName2) database =
    Against.makeAgainstSurfacePdf plotOpts prefix rateName rateName2 database

  evaluateThunk :: DataBaseResult Result -> IOMainControl Double
  evaluateThunk = evaluateDbResult env evaluateResult

  evaluateResult :: Result -> IOMainControl Double
  evaluateResult (AverageResult ar) = return ar
  evaluateResult res                = 
    fail $ unlines [ "Expected average response results, instead found:"
                   , show res
                   ]
evaluatePtree env (PpddlModel modelTree)                                 =
  evaluatePddlModel env modelTree
evaluatePtree _env dontknow =
  error $ concat [ "I haven't matched against this kind of ptree:\n"
                 , show dontknow
                 ]


evaluateTranslation :: Environment a -> ModelTree 
                     -> TranslateTarget -> IOMainControl Result
evaluateTranslation env modelTree (JavaSimulator) =
  do eModelTree      <- evaluateModelTree env modelTree
     (mFile,model)   <- getModel env eModelTree
     liftMC $ staticAnalyseModel model
     simpResult      <- liftMC $ Simplify.simplify [] model
     let simpModel   =  Simplify.simplifiedModel simpResult
     -- So actually we of course don't actually want the state space.
     fullSpace       <- liftMC $ States.getModelStateSpace genOpts simpModel
     noVanishing     <- liftMC $ States.removeImmediateStates fullSpace
     let javaFile  = Ptrees.resultOfModelFile ".java" mFile
         className = File.takeBaseName javaFile
         contents  = JavaSimulator.statespaceToSimulator className noVanishing
     return $ FileResult javaFile contents
  where
  genOpts = Ipc.getSpaceGenerationOptions $ envCliOpts env

evaluateDbResult :: Environment a
                 -> (Result -> IOMainControl b)
                 -> DataBaseResult Result
                 -> IOMainControl b
evaluateDbResult env evalResult (LazyResult pt)        = 
  evaluatePtree env pt >>= evalResult
evaluateDbResult _env evalResult (ComputedResults res) =
  evalResult res


{-
  This will produce a collated database tree. This is not necessarily
  what we want in the future. In the future we possibly want the
  opportunity to evaluate a lazy database.
-}
evaluateDbTree :: forall a .
                  (Read a, Show a) => 
                  DataBaseTree 
               -> (DataBaseResult Result -> IOMainControl a)
               -> IOMainControl (DataBase Ptrees.DataBaseTag a)
evaluateDbTree (DataBaseFile dbFile) evaluateThunk  =
  do fileExists <- liftIO $ Dir.doesFileExist collatedFile
     if fileExists
        then do modCollated   <- liftIO $ Dir.getModificationTime collatedFile
                modUnCollated <- liftIO $ Dir.getModificationTime dbFile
                if modCollated > modUnCollated
                   then do contents <- liftIO $ readFile collatedFile
                           return $ read contents
                   else getUncollated
        else getUncollated
  where
  collatedFile = File.addExtension dbFile "collated"
  getUncollated :: IOMainControl (DataBase Ptrees.DataBaseTag a)
  getUncollated = 
    do contents  <- liftIO $ readFile dbFile
       let lazyDatabase   = read contents
           dbSize         = DataBase.dataBaseSize lazyDatabase
           evaluateReport :: Int -> DataBaseResult Result 
                           ->IOMainControl (Int, a)
           evaluateReport k result =
             do liftIO $ putStrLn $ unwords [ "doing result:"
                                            , show k
                                            , "of"
                                            , show dbSize
                                            ]
                r <- evaluateThunk result
                return (k+1, r)
       database <- DataBase.convertWithAccum lazyDatabase 1 evaluateReport
       -- SHOULD NOT do the collating here, because we may have already
       -- filtered the database.
       -- Well actually yes we should but we should merge it with
       -- the uncollated one, in a file based way which I'm unsure of.
       liftIO $ writeFile collatedFile (show database)     
       return database
evaluateDbTree (DataFilter filters dbTree) evaluateThunk =
  do database <- evaluateDbTree dbTree evaluateThunk
     return $ DataBase.queryDataBase database condition
  where
  -- So in other words if there is any thing in the filters which is
  -- not in the tag of the entry then the entry does NOT meet the
  -- filter
  condition ::Ptrees.DataBaseTag -> Bool  
  condition = Map.null . (Map.differenceWith compareFun filters)
  compareFun x y
    | x == y    = Nothing
    | otherwise = Just y
  

plotData :: PlotKind -> PlotOptions -> Result -> MainControl DrawGraph.Graph
plotData Lines   = plotMultiLineGraph
plotData Surface = fail "Surface plots now yet working"

plotMultiLineGraph :: PlotOptions -> Result -> MainControl DrawGraph.Graph
plotMultiLineGraph pOptions (PassageResult pt)         =
  return $ DrawGraph.makeSimpleGraph graphTitle options graphLines
  where
  graphTitle = Just "Probability density function graph"  
  graphLines = createPassagePdfLines [ ("pdf", pt) ]
  options    = Ptrees.plotOptionsToGraphOptions pOptions
plotMultiLineGraph pOptions (TransientResult tr)       =
  return $ DrawGraph.makeSimpleGraph graphTitle options graphLines
  where
  graphTitle = Just "Transient Timeseries"  
  graphLines = TimeSeries.createTransientTimeSeries pOptions tr
  options    = Ptrees.plotOptionsToGraphOptions pOptions
plotMultiLineGraph pOptions (GraphData graphLines)     =
  return $ DrawGraph.makeSimpleGraph graphTitle options graphLines
  where
  graphTitle = Nothing
  options    = Ptrees.plotOptionsToGraphOptions pOptions
plotMultiLineGraph pOptions (ExperimentResult results) =
  -- TEMPORARY STUFF
  return $ DrawGraph.makeSimpleGraph graphTitle options graphLines
  where
  graphTitle     = Nothing
  graphLines     = passageLines ++ (concat graphDataLines)
  options        = Ptrees.plotOptionsToGraphOptions pOptions
  passageLines   = createPassagePdfLines passageResults
  passageResults = [ (name, pt) | ((name, _), PassageResult pt) <- results ]
  -- Horrible hack here to get the names correct.
  graphDataLines = [ map (\gl -> gl { simpleLineTitle = Just $ PtreesPrint.hprintValue value }) gd
                    | ((_name, value), GraphData gd) <- results ]
plotMultiLineGraph _ _                                   =
  fail "Sorry I don't know how to plot that result as a multi-line graph"  


{-
createTransientTimeSeries :: PlotOptions -> Uniformise.TransientResult 
                          -> [ SimpleLine ]
createTransientTimeSeries plotOptions transientResult =
  map createLine plottedNames
  where
  steadies     = Uniformise.transientDistributions transientResult
  -- allNames are all the process names mentioned in any one of
  -- the time probability distribution / populations
  allNames     = foldl List.union [] $ 
                 map (Map.keys . GenMatrix.srPopulations . snd) steadies
  -- The plotted names are those names we wish to plot in the time series
  -- this will be the same as 'allNames' unless a plot options specifies
  -- the names to plot. Note also that this means we can specify the order
  -- of the names to plot using the plot options (which shouldn't matter
  -- but for now we cannot change the colours hence this is one way to
  -- make sure they are the same over multiple plots)
  plottedNames = maybe allNames getRealNames $ plotOptsLineNames plotOptions
  -- Problem is that the names given in the plot options are not qualified
  -- names so we have to turn them into qualified names by looking them up
  -- in 'allNames' for one that has the same original name.
  getRealNames = mapMaybe getRealName
  getRealName t = List.find (\a -> t == Qualified.showOrig a) allNames

  -- If the plot options gives a list of names to plot then
  -- we of course must honour that and we return 'Nothing' for
  -- any that aren't in the set of line names to be plotted.
  -- If there are no line name options then we just plot everything
  createLine :: Pepa.ParsedComponentId -> SimpleLine
  createLine name =
    line
    where
    origName = Qualified.showOrig name
    line     = SimpleLine { simpleLineTitle  = origName
                          , simpleLinePoints = map getTimePoint steadies
                          , simpleLineWidth  = lineWidth
                          }
    -- for each time/steadyreport we must get the probability
    -- of being in that state. Note that if it isn't mentioned
    -- then the probability is assumed to be zero
    getTimePoint :: (Uniformise.Time, GenMatrix.SteadyReport) 
                 -> (Uniformise.Time, Probability)
    getTimePoint (time, steady) =
      (time, maybe 0.0 id mProbability)
      where
      mProbability = Map.lookup name $ GenMatrix.srPopulations steady
  lineWidth = 2.0
-}

createPassageCdfLines :: [ (String, Uniformise.PassageResult) ] 
                      -> [ SimpleLine ] 
createPassageCdfLines ptResults =
  graphLines
  where
  graphLines = map createGraphLine ptResults
  createGraphLine :: (String, Uniformise.PassageResult) -> SimpleLine
  createGraphLine (name, pt) =
    SimpleLine { simpleLineTitle  = Just name
               , simpleLinePoints = Uniformise.cdfResult pt
               , simpleLineWidth  = lineWidth
               }
  lineWidth     = 2.0                                 


createPassagePdfLines :: [ (String, Uniformise.PassageResult) ] 
                      -> [ SimpleLine ] 
createPassagePdfLines ptResults =
  graphLines
  where
  graphLines = map createGraphLine ptResults
  createGraphLine :: (String, Uniformise.PassageResult) -> SimpleLine
  createGraphLine (name, pt) =
    SimpleLine { simpleLineTitle  = Just name
               , simpleLinePoints = Uniformise.pdfResult pt
               , simpleLineWidth  = lineWidth
               }
  lineWidth     = 2.0                                 

evaluateCdf :: Environment a -> PassageTree 
            -> IOMainControl [ DrawGraph.SimpleLine ]
evaluateCdf env passage =
  do ptResults <- evaluatePassageTree env passage
     return $ createPassageCdfLines [ ("cdf", ptResults) ]
     
evaluatePdf :: Environment a -> PassageTree 
            -> IOMainControl [ DrawGraph.SimpleLine ]
evaluatePdf env passage =
  do ptResults <- evaluatePassageTree env passage
     return $ createPassagePdfLines [ ("pdf", ptResults) ]
  
evaluatePassageTree :: Environment a -> PassageTree 
                    -> IOMainControl Uniformise.PassageResult
evaluatePassageTree env (Vconcrete passage) =
  evaluatePassage env passage
evaluatePassageTree env (Vvariable var)     =
  do value <- liftMC $ lookupVariable env var
     case value of
       (Vpassage pt) -> return pt
       _             -> fail "Passage variable of incorrect type"

evaluatePassage :: Environment a -> PassageQuery 
                -> IOMainControl Uniformise.PassageResult
evaluatePassage env passage =
  do modelTree         <- evaluateModelTree env $ passageModel passage
     (_mFile,rawModel) <- getModel env modelTree
     source            <- liftMC $ 
                          evaluateEventTree env $ passageSource passage
     target            <- liftMC $ 
                          evaluateEventTree env $ passageTarget passage
     liftMC $ staticAnalyseModel rawModel
     (model,
      measures)        <- liftMC $ createPassageMeasures rawModel source target
     simpResult        <- liftMC $ Simplify.simplify [] model
     let simpModel     =  Simplify.simplifiedModel simpResult
         simpMapping   =  Simplify.simplifiedRateMapping simpResult
     fullSpace         <- liftMC $ 
                          States.getModelStateSpace genOptions simpModel
     noVanishing       <- liftMC $ States.removeImmediateStates fullSpace
                           -- Get the generator matrix, we'll required a
                           -- non-pre-transposed one because we will be using
                           -- the generator matrix to calculate the hop values.
     genMatrix         <- liftMC $ 
                          GenMatrix.getGeneratorMatrix False noVanishing
     let qMeasureSpecs  = Ipc.qualifyMeasureSpecs simpMapping measures
     -- Translate the generator matrix into the embedded markov chain
     -- solve the embedded markov chain for a steady-state to weight
     -- the source states of the passage.
     -- This won't be used (and hence not calculated) if there
     -- is only one source state of the passage that we wish to
     -- compute.
     steady            <- liftMC $ 
                          EmbeddedMarkov.solveForEmbeddedSteadyState genMatrix

     -- Get the source and target conditions and use them to
     -- obtain from the sets of source and target states.
     let targetCond    = measureStopCond  qMeasureSpecs
         sources       = Ipc.getSourceStates qMeasureSpecs noVanishing
         targets       = States.getSatisfyingStates targetCond noVanishing
     -- Uniformise the generator matrix 
     uniformMatrix     <- liftMC $ uniformiseGenMatrix genMatrix
     
     -- Compute the passage-time result.
     liftMC $ getPassageTimes times steady 
                              uniformMatrix sources targets
   where
   genOptions = Ipc.getSpaceGenerationOptions $ envCliOpts env
   times      = getMeasurementTimes pTimes
   pTimes     = passageTimes passage

evaluatePassageWithJavaSim :: Environment a -> PassageQuery 
                            -> IOMainControl Result
evaluatePassageWithJavaSim env passage =
  do modelTree         <- evaluateModelTree env $ passageModel passage
     (mFile,rawModel)  <- getModel env modelTree
     source            <- liftMC $ 
                          evaluateEventTree env $ passageSource passage
     target            <- liftMC $ 
                          evaluateEventTree env $ passageTarget passage
     liftMC $ staticAnalyseModel rawModel
     (model,
      measures)        <- liftMC $ createPassageMeasures rawModel source target
     simpResult        <- liftMC $ Simplify.simplify [] model
     let simpModel     =  Simplify.simplifiedModel simpResult
         simpMapping   =  Simplify.simplifiedRateMapping simpResult
     fullSpace         <- liftMC $ 
                          States.getModelStateSpace genOptions simpModel
     noVanishing       <- liftMC $ States.removeImmediateStates fullSpace
                           -- Get the generator matrix, we'll required a
                           -- non-pre-transposed one because we will be using
                           -- the generator matrix to calculate the hop values.
     genMatrix         <- liftMC $ 
                          GenMatrix.getGeneratorMatrix False noVanishing
     let qMeasureSpecs  = Ipc.qualifyMeasureSpecs simpMapping measures
     -- Translate the generator matrix into the embedded markov chain
     -- solve the embedded markov chain for a steady-state to weight
     -- the source states of the passage.
     -- This won't be used (and hence not calculated) if there
     -- is only one source state of the passage that we wish to
     -- compute.
     -- steady            <- liftMC $ 
     --                      EmbeddedMarkov.solveForEmbeddedSteadyState genMatrix

     -- Get the source and target conditions and use them to
     -- obtain from the sets of source and target states.
     let targetCond    = measureStopCond  qMeasureSpecs
         sources       = Ipc.getSourceStates qMeasureSpecs noVanishing
         targets       = States.getSatisfyingStates targetCond noVanishing
         javaFile      = Ptrees.resultOfModelFile ".java" mFile
         className     = File.takeBaseName javaFile
         contents      = JavaSimulator.statespaceToPassSim sources 
                                                           targets 
                                                           className
                                                           noVanishing
     return $ FileResult javaFile contents
   where
   genOptions = Ipc.getSpaceGenerationOptions $ envCliOpts env
   -- times      = getMeasurementTimes pTimes
   -- pTimes     = passageTimes passage


evaluateAveragePassage :: Environment a -> AveragePassageQuery 
                       -> IOMainControl Double
evaluateAveragePassage env avgPass =
  (evaluateModelTree env $ avgPassModel avgPass) >>= linterpret
  where
  -- local interpret, as in locally interpret this model tree given that
  -- we know we are doing transient analysis.
  linterpret :: ModelTree -> IOMainControl Double
  linterpret (Mmodel mFile model)                  = 
    evaluateAveragePassageCtmc env avgPass mFile model
  linterpret (Modes (Mmodel (Stored file) model))  = 
    evaluateAveragePassageOdes model file
  linterpret (Modes (Mmodel (Related file) model)) = 
    evaluateAveragePassageOdes model file
  linterpret mTree                                 =
    fail $ unlines[ "Wrong kind of model tree in transient node:"
                  , show mTree
                  ]

  stopTime = Maybe.fromMaybe 100.0 $ avgPassStopTime avgPass  

  evaluateAveragePassageOdes file model =
    do source           <- liftMC $ evaluateEventTree env $
                                    avgPassSource avgPass
       case source of
         Eactions [ one ] -> 
            Odes.solvePepatoOdesAveragePassage model file stopTime 
                                               "UserWaiting" $
                                               Qualified.textual one
         _                -> error "Sorry too complicated sources"

evaluateAveragePassageCtmc
  :: Environment a 
  -> AveragePassageQuery 
  -> ModelFile 
  -> ParsedModel
  -> IOMainControl Double
evaluateAveragePassageCtmc env avgPass _mFile rawModel =
  do source           <- liftMC $ evaluateEventTree env $ avgPassSource avgPass
     target           <- liftMC $ evaluateEventTree env $ avgPassTarget avgPass
     -- for debugging liftIO $ putStrLn $ PepaPrint.hprintPepaModel rawModel    
     liftMC $ staticAnalyseModel rawModel
     (model,
      measures)       <- liftMC $ createPassageMeasures rawModel source target
     simpResult       <- liftMC $ Simplify.simplify [] model
     let simpModel    =  Simplify.simplifiedModel simpResult
         simpMapping  =  Simplify.simplifiedRateMapping simpResult
     fullSpace        <- liftMC $ States.getModelStateSpace genOptions simpModel
     noVanishing      <- liftMC $ States.removeImmediateStates fullSpace
                         -- Get the generator matrix, we'll required a
                         -- non-pre-transposed one because we will be using
                         -- the generator matrix to calculate the hop values.
     genMatrix        <- liftMC $ GenMatrix.getGeneratorMatrix False noVanishing
     let qMeasureSpecs = Ipc.qualifyMeasureSpecs simpMapping measures
     -- Translate the generator matrix into the embedded markov chain
     -- solve the embedded markov chain for a steady-state to weight
     -- the source states of the passage.
     -- This won't be used (and hence not calculated) if there
     -- is only one source state of the passage that we wish to
     -- compute.
     steady           <- liftMC $ GenMatrix.solveForSteadyState genMatrix
     -- Get the throughput of the start actions
     -- Get the probability of being in the probe running state.
     let -- mSpecs       = procMeasurementSpecs $ srProcessResults sResult
         runCond      = measureRunningCond qMeasureSpecs
         startActions = measureStartActs   qMeasureSpecs
         startActs    = map Qualified.Unqualified startActions
     return $ GenMatrix.calculateAverageReponseTime runCond startActs steady
   where
   genOptions = Ipc.getSpaceGenerationOptions $ envCliOpts env


evaluateOdesResponse :: Environment a 
                      -> OdeConfig 
                      -> ModelTree 
                      -> IOMainControl String
evaluateOdesResponse env odeConf modelTree =
  (evaluateModelTree env modelTree) >>= linterpret
  where
  times = odeConfTimes odeConf
  -- local interpret, as in locally interpret this model tree given that
  -- we know we are doing transient analysis.
  linterpret :: ModelTree -> IOMainControl String
  linterpret (Mmodel mFile model)                  = 
    do 
       simpResult       <- liftMC $ Simplify.simplify [] model
       let simpModel    =  Simplify.simplifiedModel simpResult
           -- simpMapping  =  Simplify.simplifiedRateMapping simpResult
       odes             <- liftMC $ Odes.convertPepaToOdes simpModel
       let concMap      = Analysis.getInitialConcentrations simpModel
           -- Rather than just use the populations gotten from the
           -- 'getInitialConcentrations' function we must make sure that
           -- we override any that are set on the command-line or in the
           -- performance tree.
           populations  = map makePop $ Map.toList concMap
           makePop :: (Qualified.QualifiedName, Double) -> (String, Double)
           makePop (qName, i) =
             ( name 
             , Maybe.fromMaybe defaultPop $ lookup name odePops
             )
             where
             odePops    = odeConfPopulations odeConf
             name       = Qualified.textual qName
             -- So the default is taken from the model
             defaultPop = i 

           initEnv      = Horddes.Solve.makeInitialEnvironment populations
           odeProblem   = (initEnv, times, odes)
           steadyRes    = Horddes.Solve.solveOdesForSteadyState odeProblem
           timeSeries   = Horddes.Solve.timeSeriesOfSteadyResult steadyRes
           odeOutput    = Horddes.hprintOdes odes
           tsOutput     = Horddes.Solve.formatOdeResult timeSeries
           steadyOut    = Horddes.Solve.formatSteadyResult steadyRes
           output       = unlines [ odeOutput, steadyOut ]
           csvFile      = getCsvFile mFile
       liftIO $ writeFile csvFile tsOutput
       return output
  linterpret mTree                                 =
    fail $ unlines[ "Wrong kind of model tree in OdeResponse node:"
                  , show mTree
                  ]
  getCsvFile :: ModelFile -> FilePath
  getCsvFile = Ptrees.resultOfModelFile "csv"

getMeasurementTimes :: PassageTimes -> [ Uniformise.Time ]
getMeasurementTimes pTimes = 
   getTimes startTime
   where
   startTime = Maybe.fromMaybe 0.0 $ passageStartTime pTimes
   stopTime  = Maybe.fromMaybe (10.0 + startTime) $ passageStopTime pTimes
   timeStep  = Maybe.fromMaybe ((stopTime - startTime) / 20.0)
               $ passageTimeStep pTimes

   -- recusively gets the times from the given time to
   -- the stopTime in increments of timeStep
   getTimes :: Double ->  [ Double ]
   getTimes d
    | d > stopTime = []
    | otherwise    = d : (getTimes $ d + timeStep)


evaluatePassageEndQuery :: Environment a -> PassageEndQuery
                        -> IOMainControl Uniformise.PassageEndResult
evaluatePassageEndQuery env passage =
  do modelTree         <- evaluateModelTree env $ passageEndModel passage
     (_mFile,rawModel) <- getModel env modelTree
     source            <- liftMC $ evaluateEventTree env $ 
                                   passageEndSource passage
     target            <- liftMC $ evaluateEventTree env $ 
                                   passageEndTargets passage
     liftMC $ staticAnalyseModel rawModel
     (model,
      measures)        <- liftMC $ createPassageMeasures rawModel source target
     simpResult        <- liftMC $ Simplify.simplify [] model
     let simpModel     =  Simplify.simplifiedModel simpResult
         simpMapping   =  Simplify.simplifiedRateMapping simpResult
     fullSpace         <- liftMC $ 
                          States.getModelStateSpace genOptions simpModel
     noVanishing       <- liftMC $ States.removeImmediateStates fullSpace
                          -- Get the generator matrix, we'll required a
                          -- non-pre-transposed one because we will be using
                          -- the generator matrix to calculate the hop values.
     genMatrix         <- liftMC $ 
                          GenMatrix.getGeneratorMatrix False noVanishing
     let qMeasureSpecs = Ipc.qualifyMeasureSpecs simpMapping measures
     -- Translate the generator matrix into the embedded markov chain
     -- solve the embedded markov chain for a steady-state to weight
     -- the source states of the passage.
     -- This won't be used (and hence not calculated) if there
     -- is only one source state of the passage that we wish to
     -- compute.
     steady           <-  liftMC $ 
                          EmbeddedMarkov.solveForEmbeddedSteadyState genMatrix

     -- Get the source and target conditions and use them to
     -- obtain from the sets of source and target states.
     -- First of all get the state space of the model
     -- we have to choose the space with no vanishing states
     let sources      = Ipc.getSourceStates qMeasureSpecs noVanishing
         stopActs     = measureStopActs  qMeasureSpecs
         targetSets   = map (makeTargetSet noVanishing) stopActs

     -- Uniformise the generator matrix 
     uniformMatrix    <- liftMC $ uniformiseGenMatrix genMatrix
     
     -- Compute the passage-end result.
     liftMC $ Uniformise.getPassageEndTimes normalise 
                                            times 
                                            steady 
                                            uniformMatrix 
                                            sources 
                                            targetSets
   where
   options    = envCliOpts env
   genOptions = Ipc.getSpaceGenerationOptions options
   times      = getMeasurementTimes pTimes
   pTimes     = passageEndTimes passage
   normalise  = Ipc.getPassEndNormalise options

   -- This is at least pretty inefficient that we do this for all the
   -- action names rather than once creating many different sets.
   -- That is 'findTargetedStates' should have type
   -- StateSpace -> [ String ] -> Map String (Set Int)
   -- if you want them all then you can simply do (Set.unions . Map.elems)
   -- alternatively we could have two functions with similar behaviours.
   makeTargetSet :: States.StateSpace -> String -> (String, Set States.StateId)
   makeTargetSet space action =
     (action, States.findTargetedStates [ action ] space)

evaluateTransient :: Environment a -> TransientQuery -> IOMainControl Result
evaluateTransient env transient =
  (evaluateModelTree env $ transientModel transient) >>= linterpret
  where
  -- local interpret, as in locally interpret this model tree given that
  -- we know we are doing transient analysis.
  linterpret :: ModelTree -> IOMainControl Result
  linterpret (Mmodel _ model)                      = 
    evaluateTransientPepa    env transient model 
  linterpret (Mbiopepa opts _ model)               =  
    evaluateTransientBioPepa env transient opts model
  linterpret (Modes (Mmodel (Stored file) model))  = 
    evaluateTransientPepato  env transient model file
  linterpret (Modes (Mmodel (Related file) model)) = 
    evaluateTransientPepato  env transient model file
  linterpret mTree                                 =
    fail $ unlines[ "Wrong kind of model tree in transient node:"
                  , show mTree
                  ]


evaluateTransientPepa :: Environment a -> TransientQuery 
                      -> ParsedModel -> IOMainControl Result
evaluateTransientPepa env transient model =
  do liftMC $ staticAnalyseModel model
     simpResult      <- liftMC $ Simplify.simplify [] model
     let simpModel   =  Simplify.simplifiedModel simpResult
     fullSpace       <- liftMC $ States.getModelStateSpace genOpts simpModel
     noVanishing     <- liftMC $ States.removeImmediateStates fullSpace
                        -- Get the generator matrix, we'll required a
                        -- non-pre-transposed one because we will be using
                        -- the generator matrix to calculate the hop values.
     genMatrix       <- liftMC $ GenMatrix.getGeneratorMatrix False noVanishing
     source          <- liftMC $ States.getInitialState noVanishing
     -- Uniformise the generator matrix 
     uniformMatrix   <- liftMC $ uniformiseGenMatrix genMatrix
     transientRes    <- liftMC $ Uniformise.computeTransientResults 
                                    noVanishing
                                    times
                                    uniformMatrix
                                    source
     return $ TransientResult transientRes
  where
  -- We explicitly allow deadlocks because for transient analysis
  -- this is frequently okay
  genOpts    = cliGenOpts { States.genOptsAllowDeadLocks = True }
  cliGenOpts = Ipc.getSpaceGenerationOptions $ envCliOpts env
  times      = getMeasurementTimes pTimes
  pTimes     = transientTimes transient

evaluateTransientBioPepa :: Environment a -> TransientQuery 
                         -> BioPepa.ModelOptions -> BioPepa.Model 
                         -> IOMainControl Result
evaluateTransientBioPepa _env transient opts model =
  do (odes, initEnv) <- liftMC $ BioPepaOdes.biopepaToOdes opts model
     let odeProblem  = (initEnv, times, odes)
         odesResult  = Horddes.Solve.solveOdes odeProblem
         tResult     = Odes.transientResultFromOdeResult odesResult
     return $ TransientResult tResult
  where
  -- Obviously then it would be better if the bio-pepa transient time
  -- thing matched up with this time.
  times      = TimeConfig { tcStartTime = 
                               Maybe.fromMaybe 0.0 $ passageStartTime pTimes
                          , tcStopTime  = 
                               Maybe.fromMaybe 10.0 $ passageStopTime  pTimes
                          , tcTimeStep  = 
                               Maybe.fromMaybe 1.0  $ passageTimeStep  pTimes
                          , tcInterval  = 0.1
                          }
  pTimes     = transientTimes transient

evaluateTransientPepato :: Environment a -> TransientQuery 
                        -> ParsedModel -> FilePath -> IOMainControl Result
evaluateTransientPepato _env transient model file =
  liftM TransientResult $ 
  Odes.solvePepatoOdesModel file model startTime stopTime
  where
  startTime = Maybe.fromMaybe 0.0   $ passageStartTime pTimes
  stopTime  = Maybe.fromMaybe 200.0 $ passageStopTime  pTimes
  pTimes     = transientTimes transient

evaluateSteady :: Environment a -> SteadyQuery -> IOMainControl Result
evaluateSteady env steadyQuery =
  do modelTree       <- evaluateModelTree env $ steadyModel steadyQuery
     (_mFile,model)  <- getModel env modelTree
     liftMC $ staticAnalyseModel model
     simpResult      <- liftMC $ Simplify.simplify [] model
     let simpModel   =  Simplify.simplifiedModel simpResult
     fullSpace       <- liftMC $ States.getModelStateSpace genOpts simpModel
     noVanishing     <- liftMC $ States.removeImmediateStates fullSpace
                        -- Get the generator matrix, we're happy with a
                        -- pre-transposed version because we'll only be using
                        -- the steady-state.
     genMatrix       <- liftMC $ GenMatrix.getGeneratorMatrix True noVanishing
     steady          <- liftMC $ GenMatrix.solveForSteadyState genMatrix
     let report      = GenMatrix.getSteadyStateReport steady
     return $ SteadyResult report
  where
  genOpts = Ipc.getSpaceGenerationOptions $ envCliOpts env

evaluateStateSize :: Environment a -> ModelTree -> IOMainControl Result
evaluateStateSize env modelTree =
  do eModelTree      <- evaluateModelTree env modelTree
     (_mFile,model)  <- getModel env eModelTree
     liftMC $ staticAnalyseModel model
     simpResult      <- liftMC $ Simplify.simplify [] model
     let simpModel   =  Simplify.simplifiedModel simpResult
     fullSpace       <- liftMC $ States.getModelStateSpace genOpts simpModel
     noVanishing     <- liftMC $ States.removeImmediateStates fullSpace
     let report      = unlines [ "The full state space size = " ++ sizeWhole
                               , "Once diminishing states have been removed = "
                                 ++ sizeReduced
                               ]
         sizeWhole   = show $ States.stateSpaceSize fullSpace
         sizeReduced = show $ States.stateSpaceSize noVanishing
     return $ StatesSize report
  where
  genOpts = Ipc.getSpaceGenerationOptions $ envCliOpts env

evaluatePddlModel :: Environment a -> ModelTree -> IOMainControl Result
evaluatePddlModel env modelTree =
  do eModelTree      <- evaluateModelTree env modelTree
     (mFile,model)   <- getModel env eModelTree
     liftMC $ staticAnalyseModel model
     simpResult      <- liftMC $ Simplify.simplify [] model
     let simpModel   =  Simplify.simplifiedModel simpResult
     fullSpace       <- liftMC $ States.getModelStateSpace genOpts simpModel
     noVanishing     <- liftMC $ States.removeImmediateStates fullSpace
     return $ PddlModelResult mFile (Pddl.statespaceToPddl noVanishing)
  where
  genOpts = Ipc.getSpaceGenerationOptions $ envCliOpts env

evaluateThroughput :: Environment a -> ThroughPutQuery -> IOMainControl Result
evaluateThroughput env through =
  do modelTree       <- evaluateModelTree env $ throughModel through
     (_mFile,model)  <- getModel env modelTree
     actions         <- liftMC $ evaluateActionTree env $ throughActions through
     liftMC $ staticAnalyseModel model
     simpResult      <- liftMC $ Simplify.simplify [] model
     let simpModel   =  Simplify.simplifiedModel simpResult
     fullSpace       <- liftMC $ States.getModelStateSpace genOpts simpModel
     noVanishing     <- liftMC $ States.removeImmediateStates fullSpace
                        -- Get the generator matrix, we're happy with a
                        -- pre-transposed version because we'll only be using
                        -- the steady-state.
     genMatrix       <- liftMC $ GenMatrix.getGeneratorMatrix True noVanishing
     steady          <- liftMC $ GenMatrix.solveForSteadyState genMatrix
     let report      = GenMatrix.getSteadyStateReport steady
         throughput  = GenMatrix.getThroughPutOfActions report actions
     return $ ThroughputResult throughput
  where
  genOpts = Ipc.getSpaceGenerationOptions $ envCliOpts env

staticAnalyseModel :: ParsedModel -> MainControl ()
staticAnalyseModel pModel
    -- If there are no errors then we can return the result with
    -- the warnings (of which there may also be none)
  | null analysisErrors             = 
    MainControl.resultWarning () analysisWarnings logKey logInformation
    -- If there are any errors then we must return an error
    -- NOTE: it's a shame that in this case we throw away the warnings
    -- perhaps 'resultError' should also accept a list of warnings
    -- alternatively we could return the list of all warnings and errors
    -- (as strings).
  | otherwise                       = 
    MainControl.resultError $ unlines analysisErrors
  where
  logKey         = "static-analysis"
  logInformation = unwords [ "Static analysis discovered:"
                           , show $ length analysisWarnings
                           , "warnings"
                           ]

  -- The analysis report this contains all the information about 
  -- used/defined process/rate names etc. It also contains the
  -- analysis warnings and errors.
  analysis :: Analysis.AnalysisReport
  analysis = Analysis.analyseModel pModel

  -- All the messages (warnings and errors) of the analysis report
  -- as a pair with the first being all the errors and the second
  -- being all the warnings.
  analysisMessages  = Analysis.splitMessages $ 
                      Analysis.messagesOfReport analysis
  analysisErrors    = map Analysis.hprintAnResult $ fst analysisMessages
  analysisWarnings  = map Analysis.hprintAnResult $ snd analysisMessages


{-
  Note that we must also take in and return a model since we may
  have to add a probe to the model in order to obtain the measurement
  specification.
-}
createPassageMeasures :: ParsedModel  -- ^ The model to add to 
                      -> EventTree    -- ^ The source event tree
                      -> EventTree    -- ^ The target event tree
                      -> MainControl (ParsedModel, MeasureSpecs)
createPassageMeasures _model (Evariable _var) _                       =
  fail "A non-fully evaluated event tree"
createPassageMeasures _model _ (Evariable _var)                       =
  fail "A non-fully evaluated event tree"
createPassageMeasures model (Estates sourceCond) (Estates targetCond) =
  MainControl.valueResult result "measured-model" logString
  where
  result       = (model, measureSpecs)
  logString    = PepaPrint.hprintPepaModel model
  startCond    = Ipc.StartCond sourceCond
  measureSpecs = MeasureSpecs { measureStartCond   = startCond
                              , measureStopCond    = targetCond
                              , measureRunningCond = undefined
                              , measureStartActs   = undefined
                              , measureStopActs    = undefined
                              }
createPassageMeasures model (Einitial) (Estates targetCond)           =
  MainControl.valueResult result "measured-model" logString
  where
  result       = (model, measureSpecs)
  logString    = PepaPrint.hprintPepaModel model
  measureSpecs = MeasureSpecs { measureStartCond   = Ipc.Initial
                              , measureStopCond    = targetCond
                              , measureRunningCond = undefined
                              , measureStartActs   = undefined
                              , measureStopActs    = undefined
                              }
createPassageMeasures _model _ Einitial                               =
  fail "Cannot specify the initial condition as the target condition"
createPassageMeasures _model (Eactions _sources) (Estates _targets)   =
  fail "Cannot specify source actions and target conditions"
createPassageMeasures _model (Estates _sources) (Eactions _targets)   =
  fail "Cannot specify source conditions and target actions"
createPassageMeasures _model (Einitial) (Eactions _targets)   =
  fail "Cannot specify source initial and target actions"
createPassageMeasures model (Eactions sources) (Eactions targets)     =
  MainControl.valueResult result "measured-model" logString
  where
  result          = (probedModel, measureSpecs)
  logString       = PepaPrint.hprintPepaModel probedModel
  probedModel     = passProbedModel

  -- So add the master probe to the model
  masterModel     = ProbesApply.addMasterProbe  masterStopped
                                                masterRunning 
                                                sourceNames
                                                targetNames
                                                model
  -- Add the passage probe to the model which has already had the
  -- master probe added to it.
  passProbedModel = ProbesApply.addPassageProbe passageStopped 
                                                passageRunning
                                                sourceNames
                                                masterModel

  -- Make up the measurement specs from the conditions got
  -- by adding the probes.
  measureSpecs    = MeasureSpecs { measureStartCond   = Ipc.StartCond startCond
                                 , measureStopCond    = stopCond
                                 , measureRunningCond = runningCond
                                 , measureStartActs   = sourceNames
                                 , measureStopActs    = targetNames
                                 }
  sourceNames     = map Qualified.getOrigName sources
  targetNames     = map Qualified.getOrigName targets

  startCond       = Rates.Cident $ Qualified.unqualified passageRunning
  stopCond        = Rates.Cident $ Qualified.unqualified masterStopped
  runningCond     = Rates.Cident $ Qualified.unqualified masterRunning

  masterStopped   = "yyMasterProbeStopped"
  masterRunning   = "yyMasterProbeRunning"
  passageStopped  = "yyPassageProbeStopped"
  passageRunning  = "yyPassageProbeRunning"


evaluateEventTree :: Environment a -> EventTree -> MainControl EventTree
evaluateEventTree _env (Evariable _var) =
  fail "A variable cannot yet take on a value appropriate for an event tree"
evaluateEventTree _env etree            = return etree


{-
  I really should get rid of this, everywhere I call this we should
  instead pattern match on the model tree.
-}
getModel :: Environment a -> ModelTree 
         -> IOMainControl (ModelFile, ParsedModel)
getModel _env (Mmodel file model) = return (file, model)
getModel env  (MmodelFile file)   = 
  do model <- FileParser.parsePepaFile modelFile
     return (Stored modelFile, model)
  where
  modelFile = case envPtreeFile env of
                Nothing -> file
                Just f  -> File.combine (File.takeDirectory f) file
getModel _ modelTree              =
  fail $ unlines [ "Some kind of model tree I don't know about yet"
                 , show modelTree
                 ]

{-
  Note that this returns still a model tree, and not a model.
  This is certainly a questionable way to write this function.
  My reasoning is that it allows you to partially evaluate. 
  Anyway if you want the actual model, then you have to use
  the 'getModel' function. 
  do modelTree <- liftMC $ evaluateModelTree ....
     model     <- getModel modelTree
-}
evaluateModelTree :: Environment a -> ModelTree -> IOMainControl ModelTree
evaluateModelTree _env m@(Mmodel _ _)              = return m
evaluateModelTree _env m@(Mbiopepa _ _ _)          = return m
evaluateModelTree  env (MmodelFile file)           = 
  do model <- FileParser.parsePepaFile modelFile
     return $ Mmodel (Stored modelFile) model
  where
  modelFile = case envPtreeFile env of
                Nothing -> file
                Just f  -> File.combine (File.takeDirectory f) file
evaluateModelTree env (MbiopepaFile opts filename) = 
  do bioModel  <- BioParser.parseBioPepaFile modelFile
     return $ Mbiopepa opts (Stored modelFile) bioModel
  where
  modelFile = case envPtreeFile env of
                 Nothing -> filename
                 Just f  -> File.combine (File.takeDirectory f) filename     
evaluateModelTree env (Modes modelTree)            =
  liftM Modes $ evaluateModelTree env modelTree
evaluateModelTree env (Mvariable var)              = 
  case Map.lookup var $ envMapping env of
    Nothing -> fail ("variable not found: " ++ var) -- TODO: better message
    Just (Vmodel mt) -> evaluateModelTree env mt
    Just _           -> fail "Variable not of model tree type"
evaluateModelTree env (Mtranslate mtree)           =
  do eMtree <- evaluateModelTree env mtree
     case eMtree of
       (Mbiopepa opts mBiofile bioModel) ->
         do pepaModel <- liftMC $ BioTranslate.biopepaToPepa opts bioModel
            -- Recall that the file is only related to the model not
            -- necessarily holding the model.
            return $ Mmodel mBiofile pepaModel
       _ {-otherwise-}                  ->
         typingError "We can only translate from a biopepa model"
evaluateModelTree env (Mtransform ttree mtree)    =
  do eTtree <- liftMC $ evaluateTransformTree env ttree
     eMtree <- evaluateModelTree env mtree
     case (eTtree, eMtree) of
       (Vconcrete rules, Mmodel mFile m)   -> 
         do transformed <- liftMC $ RulesApply.applyRulesToModel m rules
            return $ Mmodel (relateModelFile mFile) transformed
       (Vconcrete rules, MmodelFile f) -> 
         do parsedModel <- FileParser.parsePepaFile f
            transformed <- liftMC $ RulesApply.applyRulesToModel 
                                    parsedModel rules
            return $ Mmodel (Related f) transformed
       (_,m)                           ->
         fail $ "You must have a concrete tree" ++
                (PtreesPrint.hprintModelTree m)
         -- return $ Mtransform eTtree eMtree
evaluateModelTree env (Mprobe probeTree mtree)    =
  do ePtree <- liftMC $ evaluateProbeTree env probeTree
     eMtree <- evaluateModelTree env mtree
     case (ePtree, eMtree) of
       (Vconcrete probedef, Mmodel mFile m)      -> 
         -- Note the 'Mmodel mFile' this means we have taken the decision
         -- that the file is ASSOCIATED with the model, NOT that it
         -- necessarily contains the same model.
         do probedModel <- liftMC $ ProbesApply.addProbeDefs [] [probedef] m
            return $ Mmodel mFile probedModel
       (Vconcrete probedef, MmodelFile f)    ->
         -- Note the 'Mmodel (Just f)' this means we have taken the decision
         -- that the file is ASSOCIATED with the model, NOT that it
         -- necessarily contains the same model.
         do parsedModel <- FileParser.parsePepaFile f
            probedModel <- liftMC $ ProbesApply.addProbeDefs [] [probedef]
                                    parsedModel
            return $ Mmodel (Related f) probedModel
       (_, _)                                ->
         return $ Mprobe ePtree eMtree
evaluateModelTree env (Mrateover ratespecstree mtree) =
  do eMtree     <- evaluateModelTree env mtree
     eRateSpecs <- mapM (liftMC . evaluateRateSpec) ratespecstree
     case (eMtree, getConcrete eRateSpecs) of
       -- The rate specifications  are not all concrete
       (_, Nothing)                        -> 
         return $ Mrateover eRateSpecs eMtree
       -- The rates specifications and the model are concrete
       (Mmodel file model, Just ratespecs) ->
         -- Note the 'Mmodel file' this means we have taken the decision
         -- that the file is ASSOCIATED with the model, NOT that it
         -- necessarily contains the same model.
         return $ Mmodel (relateModelFile file) $ 
                  Replace.overrideRateDefinitionList model ratespecs
       -- The model is not concrete
       (_, _)                              ->
         return $ Mrateover eRateSpecs eMtree
  where
  -- Turns a list of (evaluated) rate spec trees into
  -- concrete rate specifications, returns 'Nothing'
  -- if any of them are not concrete.
  getConcrete :: [ RateSpecTree ] -> Maybe [ Pepa.RateSpec ]
  getConcrete []                         = Just []
  getConcrete ((r, Vconcrete re) : rest) =
    do concreteRest <- getConcrete rest
       return $ (r, re) : concreteRest
  getConcrete _                          = Nothing


  evaluateRateSpec :: RateSpecTree -> MainControl RateSpecTree
  evaluateRateSpec (rident, vrateexp) =
    do vt <- evaluateVariableTree env valueToRateExp vrateexp
       return (rident, vt)
    where
    valueToRateExp :: Value -> MainControl (VariableTree Rates.RateExpr)
    valueToRateExp (Vnumber d) = return $ Vconcrete (Rates.Creal d)
    valueToRateExp _           =
      fail "Variable not of the correct type for a rate expression"


evaluateTransformTree :: Environment a -> TransformTree 
                      -> MainControl TransformTree
evaluateTransformTree env varTree =
  evaluateVariableTree env makeTransform varTree
  where
  makeTransform :: Value -> MainControl TransformTree
  makeTransform (Vtransform t) = return $ Vconcrete t
  makeTransform _              =  fail "Wrong type for a transform tree"


evaluateProbeTree :: Environment a -> ProbeTree -> MainControl ProbeTree
evaluateProbeTree env varTree =
  evaluateVariableTree env makeProbe varTree
  where
  makeProbe :: Value -> MainControl ProbeTree
  makeProbe (Vprobe p) = return $ Vconcrete p
  makeProbe _          = fail "Wrong type for a probe tree"


{-
  Evaluate a variable tree given a function to turn a value into
  a variable tree of the correct type.
-}
evaluateVariableTree :: Environment a 
                     -> (Value -> MainControl (VariableTree b)) 
                     -> VariableTree b
                     -> MainControl (VariableTree b)
evaluateVariableTree _env _makeA vTree@(Vconcrete _) = 
  return vTree
evaluateVariableTree env makeA (Vvariable var)       =
  lookupVariable env var >>= makeA
               
lookupVariable :: Environment a -> Variable -> MainControl Value
lookupVariable env var =
  case Map.lookup var $ envMapping env of
    Just v  -> return v
    Nothing -> fail $ "Variable: " ++ var ++ "not found"

evaluateActionTree :: Environment a -> ActionTree 
                   -> MainControl [ ActionName ]
evaluateActionTree _env (Aactions actions) = return actions

{- Printing of the results -}
outputResult :: Result -> IO ExitCode
outputResult (GraphResult graph Nothing)         =
  do DrawGraph.plotSimpleGraph DrawGraph.Window graph "FILEPATH"
     return ExitSuccess
outputResult (GraphResult graph (Just filename)) =
  do DrawGraph.plotSimpleGraph outputFormat graph filename
     return ExitSuccess
  where
  suffix = File.takeExtension filename
  outputFormat
    | suffix == ".pdf" = DrawGraph.PDF
    | suffix == ".svg" = DrawGraph.SVG
    | suffix == ".eps" = DrawGraph.PS
    | suffix == ".ps"  = DrawGraph.PS
    | suffix == ".csv" = DrawGraph.CSV
      -- otherwise, in particular including the case that
      -- the suffix is .png 
    | otherwise        = DrawGraph.PNG
outputResult (FileResult filename contents)      =
  do h       <- openFile filename WriteMode
     hPutStr h contents
     hClose h
     return ExitSuccess
outputResult (MapResult results)                 =
  Utils.collectErrorCodes outputResult results
outputResult (ExperimentResult mapping) =
  -- Obviously not the greatest but:
  Utils.collectErrorCodes outputNamedResult mapping
  where
  outputNamedResult ((variable, value), result) =
     do putStrLn $ concat [ variable
                          , " = "
                          , PtreesPrint.hprintValue value
                          , "------"
                          ]
        outputResult result
outputResult result                              = 
  do putStrLn $ PtreesPrint.hprintResult result
     return ExitSuccess


