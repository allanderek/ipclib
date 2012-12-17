module Language.Ptrees.Print
  ( hprintPtree
  , hprintModelTree
  , hprintValue
  , hprintResult
  )
where

{- Standard Library Modules Imported -}
import Control.Monad
  ( liftM )
import qualified Data.Map as Map
import Data.Map
  ( Map )
import qualified Text.PrettyPrint.HughesPJ as Pretty
import Text.PrettyPrint.HughesPJ
  ( Doc )
{- External Library Modules Imported -}
{- Local Modules Imported -}
import Ipc.DrawGraph
  ( GraphOutput      ( .. ) )
import qualified Language.Ptrees.Syntax as Ptrees
import Language.Ptrees.Syntax
  ( Result           ( .. )
  , Ptree            ( .. )
  , CompileTarget    ( .. )
  , Pfunction        ( .. )
  , PlotKind         ( .. )
  , PlotOptions      ( .. )
  , AveragePassageQuery ( .. )
  , PassageQuery     ( .. )
  , PassageTimes     ( .. )
  , PassageEndQuery  ( .. )
  , TransientQuery   ( .. )
  , SteadyQuery      ( .. )
  , ThroughPutQuery  ( .. )
  , ModelTree        ( .. )
  , RateSpecTree
  , EventTree        ( .. )
  , ActionTree       ( .. )
  , VariableTree     ( .. )
  , Variable
  , Value            ( .. )
  , DataBaseTree     ( .. )
  , GraphInstruction ( .. )
  )
import qualified Language.Pepa.Syntax as Pepa
import qualified Language.Pepa.Print as PepaPrint
import Language.Pepa.QualifiedName
  ( pprintQualifiedName )
import qualified Language.Pepa.Transform.Rules.Syntax as TransformRules
import qualified Language.Pepa.Probes.Syntax as Probes
import qualified Language.Pepa.Compile.States as States
import qualified Language.Pepa.Compile.Uniformise as Uniformise
import qualified Language.Pepa.Compile.GenMatrix as GenMatrix
import qualified Language.Pepa.Utils as Utils
import qualified Language.BioPepa.Syntax as BioPepa
import Language.BioPepa.Syntax
  ( ModelOptions   ( .. ) )
import qualified Language.BioPepa.Print as BioPepa.Print
{- End of Module Imports -}

hprintPtree :: Ptree -> String
hprintPtree = Pretty.render . pprintPtree

pprintPtree :: Ptree -> Doc
pprintPtree (PStatesSize mTree)             =
  pprintPrefix "StatesSize" $ pprintModelTree mTree
pprintPtree (PaveragePassage avgQuery)      =
  Pretty.sep [ Pretty.text "AveragePassage"
             , sourceDoc
             , targetDoc
             , pprintModelTree $ avgPassModel avgQuery
             ]
  where
  sourceDoc = pprintPrefix "Source" $
                            pprintEventTree $ avgPassSource avgQuery
  targetDoc = pprintPrefix "Target" $
                           pprintEventTree $ avgPassTarget avgQuery
pprintPtree (Ppassage passage)              =
  Pretty.sep [ Pretty.text "Passage"
             , pprintPassageQuery passage
             ]
pprintPtree (Ppassageend pendQuery)         =
  Pretty.sep [ Pretty.text "PassageEnd"
             , pprintPassageEndQuery pendQuery
             ]
pprintPtree (Ptransient transient)          = 
  Pretty.sep [ Pretty.text "Transient"
             , pprintTransientQuery transient
             ]
pprintPtree (PCdf        passageTree)       =
  Pretty.sep [ Pretty.text "Cdf"
             , Pretty.text "Passage"
             , pprintVariableTree pprintPassageQuery passageTree
             ]
pprintPtree (PPdf        passageTree)       =
  Pretty.sep [ Pretty.text "Pdf"
             , Pretty.text "Passage"
             , pprintVariableTree pprintPassageQuery passageTree
             ]
pprintPtree (Pmap mFun pTree)               =
  Pretty.sep [ Pretty.text "Map"
             , mFunDoc
             , pprintPtree pTree
             ]
  where
  mFunDoc = case mFun of
              Pfcdf -> Pretty.text "Cdf"
              Pfpdf -> Pretty.text "Pdf"
pprintPtree (Plet var ptree1 ptree2)        =
  Pretty.sep [ Pretty.text "Let"
             , pprintVariable var
             , Pretty.text "="
             , pprintPtree ptree1
             , pprintPtree ptree2
             ]
pprintPtree (Pthroughput tpQuery)           =
  pprintPrefix "Throughput" pprintThroughQuery
  where
  pprintThroughQuery :: Doc
  pprintThroughQuery =
    Pretty.sep [ Pretty.text "Throughput"
               , pprintActionTree $ throughActions tpQuery
               , pprintModelTree $ throughModel tpQuery
               ]
pprintPtree (Psteady     steadyQuery)       =
  Pretty.sep [ Pretty.text "Steady"
             , pprintSteadyQuery steadyQuery
             ]
  where
  pprintSteadyQuery :: SteadyQuery -> Doc
  pprintSteadyQuery = pprintModelTree . steadyModel
-- pprintPtree (Pthroughput ThroughPutQuery)
-- pprintPtree (PStatesSize ModelTree)
pprintPtree (Pmodel modelTree)              =
  pprintModelTree modelTree
-- pprintPtree (Pmap        Pfunction Ptree)
pprintPtree (Pexperiment var values ptree)  =
  Pretty.sep [ Pretty.text "Experiment"
             , pprintVariable var
             , Pretty.brackets $ Pretty.sep valueDocs
             , pprintPtree ptree
             ]
  where
  valueDocs = Pretty.punctuate Pretty.comma $ map pprintNameValue values
  pprintNameValue :: (String, Value) -> Doc
  pprintNameValue (name, value) = 
    Pretty.hsep [ Pretty.text name
                , Pretty.comma
                , pprintValue value
                ]
 
pprintPtree (Pplot plotKind plotOpts ptree) =
  Pretty.sep [ Pretty.text "Plot"
             , pprintPlotKind plotKind
               , pprintPlotOptions plotOpts
             , pprintPtree ptree
             ]
pprintPtree (Pplots plots)                  =
  Pretty.sep [ Pretty.text "Plots"
             , Pretty.brackets $ Pretty.sep plotDocs
             ]
  where
  plotDocs = Pretty.punctuate Pretty.comma $ map printPlot plots
  printPlot (plotKind, plotOptions, ptree) =
    Pretty.sep [ pprintPlotKind plotKind
               , pprintPlotOptions plotOptions
               , pprintPtree ptree
               ]
pprintPtree (PData dbTree graphInstrs)      =
  Pretty.sep [ Pretty.text "Data"
             , pprintDataBaseTree dbTree
             , Pretty.brackets $ 
               Pretty.sep $ 
               map pprintGraphInstruction graphInstrs
             ]

pprintCompileTarget :: CompileTarget -> Doc
pprintCompileTarget (CompileToOdes)      = Pretty.text "Odes"
pprintCompileTarget (CompileToCTMC)      = Pretty.text "Ctmc"
pprintCompileTarget (CompileToSimulator) = Pretty.text "Sim"

pprintDataBaseTree :: DataBaseTree -> Doc
pprintDataBaseTree (DataBaseFile file)     =
  pprintPrefix "DataBaseFile" $ pprintFileName file
pprintDataBaseTree (DataFilter filters db) =
  pprintPrefix "DataFilter" $
  Pretty.sep [ Pretty.brackets $ pprintFilter filters
             , pprintDataBaseTree db
             ]

pprintFilter :: Map String String -> Doc
pprintFilter mapping =
  Pretty.sep $  map pprintEntry $ Map.toList mapping
  where
  pprintEntry (l,r) = Pretty.sep [ pprintStringLiteral l
                                 , Pretty.text "="
                                 , pprintStringLiteral r
                                 ]
               
pprintGraphInstruction :: GraphInstruction -> Doc
pprintGraphInstruction AllCandleStick             = 
  Pretty.text "AllCandleStick"
pprintGraphInstruction AllTimeSeries              = 
  Pretty.text "AllTimeSeries"
pprintGraphInstruction (Against pOpts s1 mString) = 
  Pretty.sep [ Pretty.text "Against"
             , pprintPlotOptions pOpts
             , Pretty.text s1
             , Utils.pprintMaybe Pretty.text mString
             ]

pprintPlotKind :: PlotKind -> Doc
pprintPlotKind Lines   = Pretty.text "Lines"
pprintPlotKind Surface = Pretty.text "Surface"

pprintPlotOptions :: PlotOptions -> Doc
pprintPlotOptions plotOptions
  | null optionDocs = Pretty.empty
  | otherwise       = Pretty.brackets $ Pretty.sep optionDocs
  where
  optionDocs = punctuateEmpty Pretty.comma [ namesDoc, outputDoc, formatDoc ]
  outputDoc  = pprintOptAssign "output" $ 
               liftM pprintFileName $ plotOptsOutputFile plotOptions
  formatDoc  = pprintOptAssign "format" $ 
               liftM pprintOutputFormat $ plotOptsOutFormat plotOptions
  namesDoc   = pprintOptAssign "names" $ 
               liftM pprintNames $ plotOptsLineNames plotOptions
  pprintNames :: [ String ] -> Doc
  pprintNames = Pretty.sep . 
                (Pretty.punctuate Pretty.comma) .
                (map Pretty.text)
  pprintOutputFormat :: GraphOutput -> Doc
  pprintOutputFormat Window = Pretty.text "window"
  pprintOutputFormat PNG    = Pretty.text "png"
  pprintOutputFormat PS     = Pretty.text "eps"
  pprintOutputFormat PDF    = Pretty.text "pdf"
  pprintOutputFormat SVG    = Pretty.text "svg"
  pprintOutputFormat CSV    = Pretty.text "csv"
  
pprintTransientQuery :: TransientQuery -> Doc
pprintTransientQuery transient =
  Pretty.sep [ tOptionsDoc
             , pprintModelTree $ transientModel transient
             ]
  where
  tOptionsDoc  = Pretty.brackets $ Pretty.sep optionDocs 
  optionDocs   = pprintPassageTimes times
  times        = transientTimes transient 

{-
  TODO: print out the source and target events if there are any
-}
pprintPassageQuery :: PassageQuery -> Doc
pprintPassageQuery passage =
  Pretty.sep [ compileDoc
             , sourceDoc
             , targetDoc
             , tOptionsDoc
             , pprintModelTree $ passageModel passage
             ]
  where
  tOptionsDoc  = Pretty.brackets $ Pretty.sep optionDocs 
  optionDocs   = pprintPassageTimes times
  times        = passageTimes passage
  compileDoc   = pprintCompileTarget $ passageCompile passage  

  sourceDoc = pprintPrefix "Source" $
                            pprintEventTree $ passageSource passage
  targetDoc = pprintPrefix "Target" $
                           pprintEventTree $ passageTarget passage


pprintPassageEndQuery :: PassageEndQuery -> Doc
pprintPassageEndQuery passend =
  Pretty.sep [ tOptionsDoc
             , sourceDoc
             , targetDoc
             , pprintModelTree $ passageEndModel passend
             ]
  where
  tOptionsDoc  = Pretty.brackets $ Pretty.sep optionDocs 
  optionDocs   = pprintPassageTimes times
  times        = passageEndTimes passend

  sourceDoc = pprintPrefix "Source" $
                            pprintEventTree $ passageEndSource passend
  targetDoc = pprintPrefix "Target" $
                           pprintEventTree $ passageEndTargets passend

pprintPassageTimes :: PassageTimes -> [ Doc ]
pprintPassageTimes times = 
  punctuateEmpty Pretty.comma [ startTimeDoc
                              , timeStepDoc
                              , stopTimeDoc
                              ]
  where
  startTimeDoc = pprintOptAssign "start-time" $ 
                 liftM Utils.pprintDouble (passageStartTime times)
  stopTimeDoc  = pprintOptAssign "stop-time"  $ 
                 liftM Utils.pprintDouble (passageStopTime  times)
  timeStepDoc  = pprintOptAssign "time-step"  $ 
                 liftM Utils.pprintDouble (passageTimeStep  times) 

pprintEventTree :: EventTree -> Doc
pprintEventTree (Evariable n)       = pprintVariable n
pprintEventTree (Estates cond)      = 
  Pretty.brackets $ States.pprintStateCondition cond
pprintEventTree (Eactions actNames) =
  Pretty.brackets $ pprintActionNameList actNames


pprintActionTree :: ActionTree ->Doc
pprintActionTree (Aactions acts) = pprintActionNameList acts

pprintActionNameList :: [ Pepa.ActionIdentifier ] -> Doc
pprintActionNameList =
  Pretty.sep . 
  Pretty.punctuate Pretty.comma .
  map PepaPrint.pprintActionIdentifier

punctuateEmpty :: Doc -> [ Doc ] -> [Doc]
punctuateEmpty sep =
  (Pretty.punctuate sep) . (filter (not . Pretty.isEmpty))

pprintOptAssign :: String -> Maybe Doc -> Doc
pprintOptAssign _name Nothing  = Pretty.empty
pprintOptAssign name  (Just d) = pprintNameAssign name d

pprintAssign :: Doc -> Doc -> Doc
pprintAssign a b = 
  Pretty.hsep [ a, Pretty.text "=", b ]

pprintNameAssign :: String -> Doc ->Doc
pprintNameAssign = pprintAssign . Pretty.text

hprintModelTree :: ModelTree -> String
hprintModelTree = Pretty.render . pprintModelTree

pprintModelTree :: ModelTree -> Doc
pprintModelTree (Mvariable var)       = pprintVariable var
pprintModelTree (Mmodel _ model)      = 
  Pretty.sep [ Pretty.text "Model"
             , Pretty.parens $ PepaPrint.pprintPepaModel model
             ]
pprintModelTree (Modes modelTree)     =
  Pretty.sep [ Pretty.text "Odes"
             , pprintModelTree modelTree
             ]
pprintModelTree (MmodelFile filename) =
  Pretty.sep [ Pretty.text "ModelFile"
             , pprintFileName filename
             ]
pprintModelTree (Mbiopepa options _file model)  =
  Pretty.sep [ Pretty.text "BioPepa"
             , pprintBioPepaOptions options
             , Pretty.parens $ BioPepa.Print.pprintBioPepaModel model
             ]

pprintModelTree (MbiopepaFile options file)     =
  Pretty.sep [ Pretty.text "BioPepaFile"
             , pprintBioPepaOptions options
             , pprintFileName file
             ]
pprintModelTree (Mtransform ttree modelTree)    =
  Pretty.sep [ Pretty.text "ModelTransform"
             , pprintVariableTree pprintTransformRules ttree
             , pprintModelTree modelTree
             ]
  where
  pprintTransformRules :: TransformRules.Rules -> Doc
  pprintTransformRules = Pretty.parens . TransformRules.pprintRules
pprintModelTree (Mtranslate mTree)              =
  Pretty.sep [ Pretty.text "ModelTranslate"
             , pprintModelTree mTree
             ]
pprintModelTree (Mprobe probeTree mTree)        =
  Pretty.sep [ Pretty.text "ModelProbe"
             , Pretty.parens $ pprintProbeTree probeTree
             , pprintModelTree mTree
             ]
  where
  pprintProbeTree = pprintVariableTree Probes.pprintProbeDef
pprintModelTree (Mrateover rateSpecs modelTree) =
  Pretty.sep [ Pretty.text "ModelRate"
             , Pretty.brackets $ Pretty.sep rateDocs
             , pprintModelTree modelTree
             ]
  where
  rateDocs = Pretty.punctuate Pretty.comma $ map printRateSpec rateSpecs
  printRateSpec :: RateSpecTree -> Doc
  printRateSpec (rIdent, r) = Pretty.hsep [ pprintQualifiedName rIdent
                                          , Pretty.text "="
                                          , printRateExprVar r
                                          ]
  printRateExprVar (Vvariable var)   = pprintVariable var
  printRateExprVar (Vconcrete rExpr) = PepaPrint.pprintRateExpr rExpr

pprintBioPepaOptions :: ModelOptions -> Doc
pprintBioPepaOptions options =
  Pretty.brackets optionsDoc
  where
  optionsDoc = 
    Pretty.sep [ maybe Pretty.empty stepsDoc $ modelOptsStepSize options
               , pprintNameAssign "rate-params" paramsDoc
               , pprintNameAssign "max-concentrations" maxConc
               , pprintNameAssign "initial-concentrations" initConc
               , pprintNameAssign "rate-calc" rateAssignDoc
               ]
  stepsDoc :: Int -> Doc
  stepsDoc   = pprintNameAssign "stepsize" . Pretty.int
  paramsDoc  = Pretty.brackets $ Pretty.sep $ map pprintRateAssign $
                  modelOptsRateParams options
  pprintRateAssign ::BioPepa.RateParameter -> Doc
  pprintRateAssign (ratename, value) = 
    pprintAssign (pprintQualifiedName ratename) 
                 (Utils.pprintDouble value)
  maxConc    = Pretty.brackets . 
               Pretty.sep . 
               map pprintInitConc .
               modelOptsMaxConcs $ options
  initConc   = Pretty.brackets . 
               Pretty.sep . 
               map pprintInitConc .
               modelOptsInitConcs $ options
  pprintInitConc (name, value) =
    pprintAssign (pprintQualifiedName name)
                 (Pretty.int value)
  rateAssignDoc
    | modelOptsRateCalc options = Pretty.text "full"
    | otherwise                 = Pretty.text "simple"

pprintVariable :: Ptrees.Variable -> Doc
pprintVariable v = Pretty.text $ '?' : v

pprintVariableTree :: (a -> Doc) -> VariableTree a -> Doc
pprintVariableTree _f (Vvariable var) = pprintVariable var
pprintVariableTree  f (Vconcrete a)   = f a

pprintFileName :: FilePath -> Doc
pprintFileName = pprintStringLiteral

pprintStringLiteral :: String -> Doc
pprintStringLiteral = Pretty.doubleQuotes . Pretty.text

pprintValue :: Value -> Doc
pprintValue (Vnumber d)        = Utils.pprintDouble d
pprintValue (Vmodel modelTree) = pprintModelTree modelTree
pprintValue (Vtransform trans) = 
  Pretty.sep [ Pretty.text "Transform"
             , Pretty.parens $ TransformRules.pprintRules trans
             ]
pprintValue (Vprobe probeDef)  = Probes.pprintProbeDef probeDef
pprintValue (Vpassage pr)      = Uniformise.pprintPassageResult pr
pprintValue (Vresult r)        = pprintResult r
pprintValue (Pthunk ptree)     = Pretty.hcat [ Pretty.text "==>"
                                             , pprintPtree ptree
                                             ]

hprintValue :: Value -> String
hprintValue = Pretty.render . pprintValue


{-| Pretty print a result as a document
-}
pprintResult :: Result -> Doc
pprintResult (BoolResult True)          = Pretty.text "true"
pprintResult (BoolResult False)         = Pretty.text "false"
pprintResult (ThroughputResult d)       = Utils.pprintDouble d
pprintResult (AverageResult d)          = Utils.pprintDouble d
pprintResult (StatesSize s)             = Pretty.text s
pprintResult (SteadyResult sreport)     = 
  GenMatrix.pprintSteadyStateReport False sreport
pprintResult (ModelResult _file m)      =
  PepaPrint.pprintPepaModel m
pprintResult (PddlModelResult _file m)  = Pretty.text m
pprintResult (PassageResult pr)         = 
  Uniformise.pprintPassageResult pr
pprintResult (TransientResult r)        =
  Uniformise.pprintTransientResult r
pprintResult (ExperimentResult mapping) =
  Pretty.vcat $ map pprintValueResult mapping
  where
  pprintValueResult :: ((Variable, Value), Result) -> Doc
  pprintValueResult ((variable, value), result) =
     Pretty.hcat [ Pretty.text variable
                 , Pretty.text "="
                 , Pretty.vcat [ pprintValue value
                               , pprintResult result
                               ]
                 ]

pprintResult (FileResult _ contents)    = Pretty.text contents
pprintResult (MapResult results)        = 
  Pretty.vcat $ map pprintResult results
pprintResult (PassageEndResult per)     = 
  Uniformise.pprintPassageEndResult per
pprintResult (GenericStringResult s)    = Pretty.text s
pprintResult (ExternalResult ec)        = Pretty.text $ show ec
pprintResult (GraphResult _graph _)     =
  Pretty.text "Serious error: GraphResult shouldn't get this far"
pprintResult (GraphData _graph)         =
  Pretty.text "Serious error: GraphData shouldn't get this far"
-- pprintResult result                     = 
--  Pretty.text show result



hprintResult :: Result -> String
hprintResult = Pretty.render . pprintResult



{-| Prints a document prefixed with a separate string
    useful for ptrees since many are of the form:
    Model ModelTree
-}
pprintPrefix :: String -> Doc -> Doc
pprintPrefix name doc =
  Pretty.sep [ Pretty.text name, doc ]