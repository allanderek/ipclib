module Language.Ptrees.Syntax
  ( Result           ( .. )
  , Ptree            ( .. )
  , CompileTarget    ( .. )
  , OdeConfig        ( .. )
  , TranslateTarget  ( .. )
  , Pfunction        ( .. )
  , PlotDesc
  , PlotKind         ( .. )
  , PlotOptions      ( .. )
  , defaultPlotOptions
  , plotOptionsToGraphOptions
  , PassageTree
  , PassageQuery     ( .. )
  , PassageTimes     ( .. )
  , defaultPassageTimes
  , AveragePassageQuery ( .. )
  , PassageEndQuery  ( .. )
  , TransientQuery   ( .. )
  , SteadyQuery      ( .. )
  , ThroughPutQuery  ( .. )
  , ModelTree        ( .. )
  , ModelFile        ( .. )
  , relateModelFile
  , resultOfModelFile
  , RateSpecTree
  , EventTree        ( .. )
  , StateTree
  , ActionTree       ( .. )
  , ActionName
  , TransformTree
  , ProbeTree
  , VariableTree     ( .. )
  , Variable
  , Value            ( .. )
  , Time
  , DataBaseTag
  -- , DataBasePtree
  , DataBaseTree     ( .. )
  , GraphInstruction ( .. )
  , valueOfResult
  )
where

{- Standard Library Modules Imported -}
import Data.Map
  ( Map )
import qualified Data.Maybe as Maybe
import System.Exit
  ( ExitCode )
{- External Library Modules Imported -}
import qualified System.FilePath as File
import qualified Horddes.Solve
{- Local Modules Imported -}
import qualified Language.Pepa.Syntax as Pepa
import qualified Language.Pepa.Rates as Rates
import qualified Language.Pepa.Transform.Rules.Syntax as TransformRules
import qualified Language.Pepa.Probes.Syntax as Probes
import qualified Language.Pepa.Compile.States as States
import qualified Language.Pepa.Compile.GenMatrix as GenMatrix
import qualified Language.Pepa.Compile.Uniformise as Uniformise
import qualified Ipc.DrawGraph as DrawGraph
import qualified Language.BioPepa.Syntax as BioPepa

{- End of Module Imports -}

{-|
  Result is the result of evaluating a performance tree.
  TODO: it certainly seems like we should be able to combine
  the 'Result' and 'Value' data types.
-}
data Result = BoolResult Bool
            | PassageResult Uniformise.PassageResult
            | PassageEndResult Uniformise.PassageEndResult
            | TransientResult Uniformise.TransientResult
            | SteadyResult GenMatrix.SteadyReport
              -- Should be a mapping from identifier to throughput
            | ThroughputResult Double
            | AverageResult Double
            | StatesSize String
            | ModelResult ModelFile Pepa.ParsedModel
            | PddlModelResult ModelFile String
            | ExperimentResult [ ((Variable,Value), Result) ]
            | MapResult [ Result ]
            | GraphResult DrawGraph.Graph (Maybe FilePath)
            | GraphData [ DrawGraph.SimpleLine ]
            | FileResult FilePath String
            | GenericStringResult String
            | ExternalResult ExitCode
            deriving (Show, Read)

{-| We often wish to retain the file associated with a model.
    This is useful for some things in particular working out an
    output file based on the current file. However there isn't always
    a file associated with a model so it's tempting to store it as a
    (Maybe FilePath), however this doesn't record whether or not the
    model represented is the same as the one stored in the file.
    For example when we override a rate or add a probe we wish to say
    that the original model file is *related* to the current model so
    that it may then be used to create output file names but that it
    does not hold the same model.    
-}
data ModelFile = Stored FilePath
               | Related FilePath
               | NoFile
               deriving (Show, Read)

{-| Useful for when you are making a model transformation -}
relateModelFile :: ModelFile -> ModelFile           
relateModelFile (Stored f)  = Related f
relateModelFile (Related f) = Related f
relateModelFile NoFile      = NoFile

{-| Get a result file name related to a model file -}
resultOfModelFile :: String -> ModelFile -> FilePath
resultOfModelFile ext (NoFile)    = File.addExtension "results" ext
resultOfModelFile ext (Stored f)  = File.addExtension (File.dropExtension f) ext
resultOfModelFile ext (Related f) = File.addExtension (File.dropExtension f) ext

{-| A performance tree then is like an expression tree syntax.
    We allow parts to be abstracted with the use of a variable.
    This allows us to make several very similar queries which
    may for example perform a sensitivity analysis over one
    or more variables.
-}
data Ptree = Ppassage    PassageQuery
           | Ppassageend PassageEndQuery
           | Ptransient  TransientQuery
           | PCdf        PassageTree
           | PPdf        PassageTree
           -- By steady-state we mean a full steady-state analysis
           -- when we want to be able to report only parts of it
           -- such as the throughput of an action or the population
           -- of a probe-running-component we'll have a different
           -- kind of tree for that.
           | Psteady     SteadyQuery
           | Pthroughput ThroughPutQuery
           -- could have start actions and passage conditions
           | PaveragePassage AveragePassageQuery
           | PStatesSize ModelTree
           | Poderesponse OdeConfig ModelTree
           | Pmodel      ModelTree
           | PpddlModel  ModelTree
           | Ptranslate  ModelTree TranslateTarget
           | Pmap        Pfunction Ptree
           | Pexperiment Variable [ (String, Value) ] Ptree
           -- In order to implement Plet, we really need to merge the
           -- 'Result' and the 'Value' data types. This would automatically
           -- allow us to store in the environment the result of the ptree
           -- evaluation, which may not even been concrete :(
           -- But we could just have a PtreeResult value?? or Thunk as they
           -- are known in the lazy functional world.
           | Plet Variable Ptree Ptree
           | Pplot PlotKind PlotOptions Ptree
           | Pplots [ PlotDesc ]
           | PData  DataBaseTree [ GraphInstruction ]
           deriving (Show, Read)

data CompileTarget = CompileToOdes
                   | CompileToCTMC
                   | CompileToSimulator
                   deriving (Show, Read, Eq)

data OdeConfig = OdeConfig { odeConfTimes       :: Horddes.Solve.TimeConfig
                           , odeConfPopulations :: [ (String, Double) ]
                           }
                           deriving (Show, Read)

data TranslateTarget = JavaSimulator
                       deriving (Show, Read)

data DataBaseTree = DataBaseFile FilePath
                  | DataFilter (Map String String) DataBaseTree
                    deriving (Show, Read)

data GraphInstruction = 
     AllCandleStick
   | AllTimeSeries
   | Against PlotOptions String (Maybe String)
   deriving (Show, Read)

type DataBaseTag = Map String String
-- type DataBasePtree result = DataBase DataBaseTag result

data Pfunction = Pfpdf | Pfcdf deriving (Show, Read)

type PlotDesc    = (PlotKind, PlotOptions, Ptree)
data PlotKind    = Lines | Surface -- probably candlestick etc
                   deriving (Show, Read)
data PlotOptions = 
  PlotOptions { plotOptsLineNames  :: Maybe [ String ]
              , plotOptsOutputFile :: Maybe FilePath
              , plotOptsXLogScale  :: Bool
              , plotOptsYLogScale  :: Bool
              , plotOptsZLogScale  :: Bool
              , plotOptsOutFormat  :: Maybe DrawGraph.GraphOutput
              , plotOptsView       :: Maybe (Int, Int, Int, Int)
              }
  deriving (Show, Read)

defaultPlotOptions :: PlotOptions
defaultPlotOptions =
  PlotOptions { plotOptsLineNames  = Nothing
              , plotOptsOutputFile = Nothing
              , plotOptsXLogScale  = False
              , plotOptsYLogScale  = False
              , plotOptsZLogScale  = False
              , plotOptsOutFormat  = Nothing
              , plotOptsView       = Nothing
              }

plotOptionsToGraphOptions :: PlotOptions -> [ DrawGraph.GraphOption ]
plotOptionsToGraphOptions plotOpts =
  Maybe.catMaybes [ if plotOptsXLogScale plotOpts
                       then Just DrawGraph.Xlogscale else Nothing
                  , if plotOptsYLogScale plotOpts
                       then Just DrawGraph.Ylogscale else Nothing
                  , if plotOptsZLogScale plotOpts
                       then Just DrawGraph.Zlogscale else Nothing
                  ]

type PassageTree = VariableTree PassageQuery

data PassageQuery =
  PassageQuery { passageModel     :: ModelTree
               , passageSource    :: EventTree
               , passageTarget    :: EventTree
               , passageTimes     :: PassageTimes
               , passageCompile   :: CompileTarget
               }
               deriving (Show, Read)

data PassageTimes =
  PassageTimes { passageStartTime :: Maybe Time
               , passageStopTime  :: Maybe Time
               , passageTimeStep  :: Maybe Time
               }
               deriving (Show, Read)

-- | The default passage times to be used when none are provided
--   this is just all of the time specifiers are set to not-defined
--   which will cause either default times to be used or times to be
--   calculated if that is implemented for the particular kind of query.
defaultPassageTimes :: PassageTimes
defaultPassageTimes =
  PassageTimes { passageStartTime = Nothing
               , passageStopTime  = Nothing
               , passageTimeStep  = Nothing
               }

data AveragePassageQuery =
     AveragePassageQuery { avgPassModel    :: ModelTree
                         , avgPassSource   :: EventTree
                         , avgPassTarget   :: EventTree
                         , avgPassStopTime :: Maybe Time
                         }
     deriving (Show, Read)

data PassageEndQuery =
  PassageEndQuery { passageEndModel   :: ModelTree
                  , passageEndSource  :: EventTree
                  , passageEndTargets :: EventTree
                  , passageEndTimes   :: PassageTimes
                  }
                  deriving (Show, Read)                  

data TransientQuery =
  TransientQuery { transientModel :: ModelTree
                 , transientTimes :: PassageTimes
                 -- For now we will have the source
                 -- state as automatically the initial state
                 -- but really we should have it as 
                 }
                 deriving (Show, Read)

type Time = Double


data SteadyQuery = SteadyQuery { steadyModel :: ModelTree }
                   deriving (Show, Read)

{-|
  The specification of a throughput query. I hope to expand this
  a little.
-}
data ThroughPutQuery =
  ThroughPutQuery { throughModel   :: ModelTree
                  , throughActions :: ActionTree
                  }
                  deriving (Show, Read)

data ModelTree = 
    Mvariable    Variable
  | MmodelFile   FilePath
  | Mmodel       ModelFile Pepa.ParsedModel
  | Mtransform   TransformTree ModelTree
  | Mprobe       ProbeTree ModelTree
  | Mrateover    [ RateSpecTree ] ModelTree
  | Mbiopepa     BioPepa.ModelOptions ModelFile BioPepa.Model
  | MbiopepaFile BioPepa.ModelOptions FilePath
  | Modes        ModelTree
  -- For now we just assume that you wish to translate
  -- INTO a pepa model.
  | Mtranslate   ModelTree
  deriving (Show, Read)

type RateSpecTree = (Pepa.RateIdentifier, VariableTree Rates.RateExpr)



data EventTree = Evariable Variable
               | Estates   States.StateCondition
               | Einitial
               | Eactions  [ ActionName ]
               deriving (Show, Read)

type StateTree  = VariableTree States.StateCondition

data ActionTree = Aactions [ ActionName ] deriving (Show, Read)

type ActionName = Pepa.ActionIdentifier

type TransformTree = VariableTree TransformRules.Rules

type ProbeTree     = VariableTree Probes.ProbeDef

-- A variable tree represents any kind of tree that may be
-- a variable or a concrete representation. 
-- Note that we cannot use this for trees whose concrete representation
-- may have a variable within them.
-- So we don't use this for 'ModelTree' but only things which can simply
-- be either the whole representation or a variable.
data VariableTree a = Vvariable Variable
                    | Vconcrete a
                    deriving (Show, Read)

{-| The type of a variable used within a performance tree to
    abstract a part of it.
-}
type Variable = String

{-| The type of a value that a variable within a performance tree
    may take
-}
data Value = Vnumber     Double
           | Vmodel      ModelTree
           | Vtransform  TransformRules.Rules 
           | Vprobe      Probes.ProbeDef
           | Vpassage    Uniformise.PassageResult
           | Vresult     Result
           | Pthunk      Ptree
           deriving (Show, Read)

valueOfResult :: Result -> Value
valueOfResult (ModelResult file model) = Vmodel $ Mmodel file model
valueOfResult res                      = Vresult res
