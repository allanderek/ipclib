{-| 
    A parser for PEPA performance trees
-}
module Language.Ptrees.Parser
  ( parsePtreeFile
  , parsePtreeString
  , ptreeParser
  )
where

{- Standard Libraries modules imported -}
import Control.Monad
  ( liftM )
import qualified Data.Map as Map
import Data.Maybe
  ( fromMaybe )
import Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec
  ( Parser )
{- External Library modules imported -}
import Horddes.Solve
  ( TimeConfig       ( .. ) )
{- Local modules imported imported -}
import qualified Language.Pepa.QualifiedName as Qualified
import qualified Ipc.DrawGraph as DrawGraph
import qualified Language.Pepa.Transform.Rules.Parser as RulesParser
import qualified Language.Pepa.Probes.Parser as ProbeParser
import qualified Language.Pepa.FileParser as FileParser
import qualified Language.Pepa.Parser as PepaParser
import Language.Pepa.Parser
  ( comma
  , symbol
  , parens
  , squares
  )
import Language.Pepa.Syntax
  ( ActionIdentifier )
import Language.Ptrees.Syntax
  ( Ptree            ( .. )
  , CompileTarget    ( .. )
  , OdeConfig        ( .. )
  , TranslateTarget  ( .. )
  , Pfunction        ( .. )
  , PlotKind         ( .. )
  , PlotOptions      ( .. )
  , defaultPlotOptions
  , PassageQuery     ( .. )
  , PassageTimes     ( .. )
  , defaultPassageTimes
  , AveragePassageQuery ( .. )
  , PassageEndQuery  ( .. )
  , TransientQuery   ( .. )
  , SteadyQuery      ( .. )
  , ThroughPutQuery  ( .. )
  , EventTree        ( .. )
  , ModelTree        ( .. )
  , ModelFile        ( .. )
  , RateSpecTree
  , ActionTree       ( .. )
  , TransformTree
  , ProbeTree
  , VariableTree     ( .. )
  , Variable
  , Value            ( .. )
  , DataBaseTree     ( .. )
  , GraphInstruction ( .. )
  )

import qualified Language.BioPepa.Syntax as BioPepa
import Language.BioPepa.Syntax
  ( ModelOptions   ( .. ) )
import qualified Language.BioPepa.Parser as BioPepaParser
import Language.Pepa.MainControl
  ( IOMainControl
  , MainControl
  )
{- End of imports -}

{-| A function to parse a performance tree file -}
parsePtreeFile :: FilePath -> IOMainControl Ptree
parsePtreeFile = FileParser.parseFile ptreeParser

{-| Parse a string into a performance tree -}
parsePtreeString :: String -> MainControl Ptree
parsePtreeString = 
  FileParser.mainControlParse parser
  where
  parser = FileParser.wholeParser ptreeParser

{-| A parser for performance trees
-}
ptreeParser :: Parser Ptree
ptreeParser = 
  Parsec.choice [ liftM Pthroughput throughputParser
                , liftM Ppassage passageParser
                , liftM Ppassageend passageEndParser
                , liftM Ptransient transientParser
                , liftM PCdf     cdfPassageParser
                , liftM PPdf     pdfPassageParser
                , liftM Psteady steadyParser
                , averagePassageParser
                , odeResponseParser
                , liftM PStatesSize statesSizeParser
                , liftM Pmodel modelTreeParser
                , pddlTranslateParser
                , translateTreeParser
                , mapPerfTreeParser
                , plotTreeParser
                , plotsTreeParser
                , experimentParser
                , graphDataBaseParser
                ]


translateTreeParser :: Parser Ptree
translateTreeParser =
  do PepaParser.reserved "Translate"
     mTree           <- modelTreeParser
     translateTarget <- translateTargetParser
     return (Ptranslate mTree translateTarget)
  where
  translateTargetParser :: Parser TranslateTarget
  translateTargetParser = 
    (PepaParser.reserved "JavaSimulator") >> (return JavaSimulator)

pddlTranslateParser :: Parser Ptree
pddlTranslateParser =
  do PepaParser.reserved "Pddl"
     liftM PpddlModel modelTreeParser

mapPerfTreeParser :: Parser Ptree
mapPerfTreeParser =
  do PepaParser.reserved "Map"
     mappingFunction <- mappingFunctionParser
     ptree           <- ptreeParser
     return $ Pmap mappingFunction ptree
  where
  mappingFunctionParser :: Parser Pfunction
  mappingFunctionParser =
    (PepaParser.reserved "Cdf" >> return Pfcdf)
    <|>
    (PepaParser.reserved "Pdf" >> return Pfpdf)

plotTreeParser :: Parser Ptree
plotTreeParser =
  do PepaParser.reserved "Plot"
     plotKind    <- plotKindParser
     plotOptions <- plotOptionsParser
     ptree       <- ptreeParser
     return $ Pplot plotKind plotOptions ptree

plotsTreeParser :: Parser Ptree
plotsTreeParser =
  do PepaParser.reserved "Plots"
     plots <- PepaParser.squares $ Parsec.sepBy singlePlot PepaParser.comma
     return $ Pplots plots
  where
  singlePlot :: Parser (PlotKind, PlotOptions, Ptree)
  singlePlot =
    do plotKind    <- plotKindParser
       plotOptions <- plotOptionsParser
       ptree       <- ptreeParser
       return (plotKind, plotOptions, ptree)


plotKindParser :: Parser PlotKind
plotKindParser =
  (PepaParser.reserved "Lines" >> return Lines)
  <|>
  (PepaParser.reserved "Surface" >> return Surface)

plotOptionsParser :: Parser PlotOptions
plotOptionsParser =
  Parsec.option defaultPlotOptions $ PepaParser.squares plotOptions
  where
  -- As wwith most of the options, the whole thing is optional
  -- so that if we just don't have the square brackets at all
  -- or any of the individual fields are optional as well.
  plotOptions :: Parser PlotOptions
  plotOptions = 
    do names     <- optionMaybe lineNames 
       outfile   <- optionMaybe graphFile
       xlogscale <- parseXlogscale
       ylogscale <- parseYlogscale
       zlogscale <- parseZlogscale 
       outform   <- optionMaybe outputFormat
       view      <- optionMaybe parsePlotView
       return PlotOptions { plotOptsLineNames  = names
                          , plotOptsOutputFile = outfile
                          , plotOptsXLogScale  = xlogscale
                          , plotOptsYLogScale  = ylogscale
                          , plotOptsZLogScale  = zlogscale
                          , plotOptsOutFormat  = outform
                          , plotOptsView       = view
                          }
  lineNames :: Parser [ String ]
  lineNames = 
    do PepaParser.reserved "names"
       PepaParser.reserved "="
       qnames <- Parsec.sepBy PepaParser.pepaComponentId comma
       return $ map Qualified.textual qnames

  graphFile :: Parser FilePath
  graphFile =
    do PepaParser.reserved "output"
       PepaParser.reserved "="
       fileNameParser

  outputFormat :: Parser DrawGraph.GraphOutput
  outputFormat =
    do PepaParser.reserved "format"
       PepaParser.reserved "="
       format
  
  parsePlotView :: Parser (Int, Int, Int, Int)
  parsePlotView =
    do PepaParser.reserved "view"
       PepaParser.reserved "="
       a <- PepaParser.intParser
       symbol ","
       b <- PepaParser.intParser
       symbol ","
       c <- PepaParser.intParser
       symbol ","
       d <- PepaParser.intParser
       return (a,b,c,d)    

  
  parseXlogscale :: Parser Bool
  parseXlogscale =
    (do PepaParser.reserved "xlogscale"
        return True) <|> return False

  parseYlogscale :: Parser Bool
  parseYlogscale =
    (do PepaParser.reserved "ylogscale"
        return True) <|> return False

  parseZlogscale :: Parser Bool
  parseZlogscale =
    (do PepaParser.reserved "zlogscale"
        return True) <|> return False


  format :: Parser DrawGraph.GraphOutput
  format = Parsec.choice 
             [ PepaParser.reserved "pdf"    >> return DrawGraph.PDF
             , PepaParser.reserved "ps"     >> return DrawGraph.PS
             , PepaParser.reserved "eps"    >> return DrawGraph.PS
             , PepaParser.reserved "svg"    >> return DrawGraph.SVG
             , PepaParser.reserved "png"    >> return DrawGraph.PNG
             , PepaParser.reserved "csv"    >> return DrawGraph.CSV
             , PepaParser.reserved "window" >> return DrawGraph.Window
             ]

variableParser :: Parser Variable
variableParser = symbol "?" >> PepaParser.lowerId

throughputParser :: Parser ThroughPutQuery
throughputParser = 
  do PepaParser.reserved "Throughput"
     actions <- actionListParser
     model   <- modelTreeParser
     return ThroughPutQuery { throughModel   = model
                            , throughActions = Aactions actions
                            }

actionListParser :: Parser [ ActionIdentifier ]
actionListParser = 
  squares $ Parsec.sepBy PepaParser.pepaActionId comma

cdfPassageParser :: Parser (VariableTree PassageQuery)
cdfPassageParser =
  variableTreeParser $ do PepaParser.reserved "Cdf"
                          passageParser

pdfPassageParser :: Parser (VariableTree PassageQuery)
pdfPassageParser =
  variableTreeParser $ do PepaParser.reserved "Pdf"
                          passageParser

passageParser :: Parser PassageQuery
passageParser =
  do PepaParser.reserved "Passage"
     compile <- Parsec.option defaultCompile $ 
               compileTargetParser
     source  <- Parsec.option defaultSource $
                do PepaParser.reserved "Source"
                   eventTreeParser
     target  <- Parsec.option defaultTarget $
                do PepaParser.reserved "Target"
                   eventTreeParser
     times   <- parsePassageTimes
     model   <- modelTreeParser
     let pquery = PassageQuery { passageModel     = model
                               , passageSource    = source
                               , passageTarget    = target
                               , passageTimes     = times
                               , passageCompile   = compile
                               }
     return pquery
  where
  defaultCompile = CompileToCTMC
  defaultSource  = Eactions [ Qualified.unqualified "start" ]
  defaultTarget  = Eactions [ Qualified.unqualified "stop" ]

compileTargetParser :: Parser CompileTarget
compileTargetParser =
  Parsec.choice [ PepaParser.reserved "Odes" >> return CompileToOdes
                , PepaParser.reserved "Ctmc" >> return CompileToCTMC
                , PepaParser.reserved "Sim"  >> return CompileToSimulator
                ]

passageEndParser :: Parser PassageEndQuery
passageEndParser =
  do PepaParser.reserved "PassageEnd"
     source <- Parsec.option defaultSource $
               do PepaParser.reserved "Source"
                  eventTreeParser
     target <- Parsec.option defaultTarget $
               do PepaParser.reserved "Target"
                  eventTreeParser
     times  <- parsePassageTimes
     model  <- modelTreeParser
     return PassageEndQuery { passageEndModel     = model
                            , passageEndSource    = source
                            , passageEndTargets   = target
                            , passageEndTimes     = times
                            }
  where
  defaultSource = Eactions [ Qualified.unqualified "start" ]
  defaultTarget = Eactions [ Qualified.unqualified "stop" ]


-- Parses a set of passage times optional passage-time specification.
-- Note that the whole specification (enclosed in square brackets)
-- can be left out, or any of the individual time specifications can
-- be left out of an otherwise present passage-times. Even [] parses.
parsePassageTimes :: Parser PassageTimes
parsePassageTimes =
  Parsec.option defaultPassageTimes parseTimes
  where
  -- Each time specification is still optional
  parseTimes :: Parser PassageTimes
  parseTimes =
    -- Note that we are very liberal about where and when
    -- we place commas to separate out the timing specifications.
    -- Basically they are all optional, you could even give a
    -- timing specification like [,,] or [,] if you wanted to.
    -- I don't see any need to be restrictive about this.
    do symbol "["
       -- We need to use try here because the start of
       -- "start-time" looks like the start of "stop-time"
       -- we don't need to use it below because the start
       -- of "time-step" cannot be confused.
       startT <- Parsec.optionMaybe $ try parseStartTime
       Parsec.optional PepaParser.comma
       stopT  <- Parsec.optionMaybe parseStopTime
       Parsec.optional PepaParser.comma
       stepT  <- Parsec.optionMaybe parseTimeStep
       symbol "]"
       return PassageTimes { passageStartTime = startT
                           , passageStopTime  = stopT
                           , passageTimeStep  = stepT
                           }
  parseStartTime = do PepaParser.reserved "start-time"
                      symbol "="
                      PepaParser.forgivingFloat
  parseStopTime  = do PepaParser.reserved "stop-time"
                      symbol "="
                      PepaParser.forgivingFloat
  parseTimeStep  = do PepaParser.reserved "time-step"
                      symbol "="
                      PepaParser.forgivingFloat

transientParser :: Parser TransientQuery
transientParser =
  do PepaParser.reserved "Transient"
     times  <- parsePassageTimes
     model  <- modelTreeParser
     return TransientQuery { transientModel = model
                           , transientTimes = times
                           }

steadyParser :: Parser SteadyQuery
steadyParser = 
  do PepaParser.reserved "Steady"
     model   <- modelTreeParser
     return SteadyQuery { steadyModel   = model }

averagePassageParser :: Parser Ptree
averagePassageParser =
  do PepaParser.reserved "AveragePassage"
     source   <- Parsec.option defaultSource $
                 do PepaParser.reserved "Source"
                    eventTreeParser
     target   <- Parsec.option defaultTarget $
                 do PepaParser.reserved "Target"
                    eventTreeParser
     stopTime <- PepaParser.maybeOption $
                 do PepaParser.reserved "StopTime"
                    PepaParser.forgivingFloat
     model    <- modelTreeParser
     let avgPass = AveragePassageQuery { avgPassModel    = model
                                       , avgPassSource   = source
                                       , avgPassTarget   = target
                                       , avgPassStopTime = stopTime
                                       }
     return $ PaveragePassage avgPass
  where
  defaultSource = Eactions [ Qualified.unqualified "start" ]
  defaultTarget = Eactions [ Qualified.unqualified "stop" ]


odeResponseParser :: Parser Ptree
odeResponseParser =
  do PepaParser.reserved "OdeResponse"
     odeOpts   <- Parsec.option [] $ PepaParser.squares genericOptionsParser
     modelTree <- modelTreeParser
     let populations = filter isNotTimeOpt odeOpts
         odeConf = OdeConfig { odeConfTimes       = getTimes odeOpts
                             , odeConfPopulations = populations
                             }
     return $ Poderesponse odeConf modelTree
  where
  -- We could warn if there are any options which are not
  -- recognised. That could also go in to 'genericOptionsParser'
  -- if we just pass in a list of valid options.
  getTimes :: [ (String, Double) ] -> TimeConfig
  getTimes timesMap =
     TimeConfig { tcStartTime = startTime
                , tcStopTime  = stopTime
                , tcTimeStep  = timeStep
                , tcInterval  = tInterval
                }
     where
     startTime = fromMaybe 0.0 $ lookup "start-time" timesMap
     stopTime  = fromMaybe 100 $ lookup "stop-time" timesMap
     timeStep  = fromMaybe ((startTime - stopTime) / 100) 
                 $ lookup "time-step" timesMap
     tInterval = fromMaybe 0.1 $ lookup "interval" timesMap 

  isNotTimeOpt :: (String, Double) -> Bool
  isNotTimeOpt (name, _value) = not $ elem name [ "start-time"
                                                , "stop-time"
                                                , "time-step"
                                                , "interval"
                                                ]
    

{-
  Don't forget that in general you will have to wrap this up in
  PepaParser.squares.
-}
genericOptionsParser :: Parser [ (String, Double) ]
genericOptionsParser = 
  Parsec.sepBy assignParser PepaParser.comma
  where
  assignParser :: Parser (String, Double)
  assignParser =
    do ident <- optionIdentParser
       PepaParser.symbol "="
       value <- PepaParser.forgivingFloat
       return (ident, value)

  optionIdentParser :: Parser String
  optionIdentParser = 
    do s <- many1 (Parsec.alphaNum <|> Parsec.char '-')
       PepaParser.whiteSpace
       return s
      

statesSizeParser :: Parser ModelTree
statesSizeParser = 
  do PepaParser.reserved "StatesSize"
     modelTreeParser

eventTreeParser :: Parser EventTree
eventTreeParser =
  -- It may be that because of Initial and Conditions
  -- we have to prefix actions with a keyword.
  Parsec.choice [ liftM Evariable variableParser
                , liftM Eactions actionListParser
                , initialParser
                , conditionParser
                ]
  where
  initialParser = do PepaParser.reserved "Initial"
                     return Einitial
  conditionParser = do PepaParser.reserved "Condition"
                       expr <- PepaParser.parens PepaParser.exprParser
                       return $ Estates expr

modelTreeParser :: Parser ModelTree
modelTreeParser =
  Parsec.choice [ modelParser
                , modelFileParser
                , bioPepaFileParser
                , bioPepaModelParser
                , modelOdesParser
                , modelTranslateParser
                , liftM Mvariable variableParser
                , modelTransformParser
                , modelProbeParser
                , modelRateParser
                ]
  where
  modelFileParser :: Parser ModelTree
  modelFileParser =
    do PepaParser.reserved "ModelFile"
       liftM MmodelFile fileNameParser

  bioPepaFileParser :: Parser ModelTree
  bioPepaFileParser =
    do PepaParser.reserved "BioPepaFile"
       options  <- bioPepaOptionsParser
       fileName <- fileNameParser
       return $ MbiopepaFile options fileName

  bioPepaModelParser :: Parser ModelTree
  bioPepaModelParser =
    do PepaParser.reserved "BioPepa"
       options <- bioPepaOptionsParser
       model   <- PepaParser.parens BioPepaParser.biopepaModel
       return $ Mbiopepa options NoFile model

  modelOdesParser :: Parser ModelTree
  modelOdesParser =
    do PepaParser.reserved "Odes"
       liftM Modes modelTreeParser

  modelTranslateParser :: Parser ModelTree
  modelTranslateParser =
    do PepaParser.reserved "ModelTranslate"
       liftM Mtranslate modelTreeParser
  
  modelTransformParser :: Parser ModelTree
  modelTransformParser =
    do PepaParser.reserved "ModelTransform"
       transform <- transformTree
       modelTree <- modelTreeParser
       return $ Mtransform transform modelTree

  modelProbeParser :: Parser ModelTree
  modelProbeParser =
    do PepaParser.reserved "ModelProbe"
       probe     <- probeTree
       modelTree <- modelTreeParser
       return $ Mprobe probe modelTree

  modelRateParser :: Parser ModelTree
  modelRateParser =
    do PepaParser.reserved "ModelRate"
       rateOvers <- PepaParser.squares $ 
                    Parsec.sepBy rateOverrideParser PepaParser.comma
       modelTree <- modelTreeParser
       return $ Mrateover rateOvers modelTree

  rateOverrideParser :: Parser RateSpecTree
  rateOverrideParser =
    do rident   <- PepaParser.pepaRateId
       PepaParser.symbol "="
       vRateExp <- variableTreeParser PepaParser.pepaRateExp
       return (rident, vRateExp)


bioPepaOptionsParser :: Parser ModelOptions
bioPepaOptionsParser =
  Parsec.option BioPepa.defaultModelOptions $ 
  PepaParser.squares modelOptions
  where
  -- As with most of the options, the whole thing is optional
  -- so that if we just don't have the square brackets at all
  -- or any of the individual fields are optional as well.
  modelOptions :: Parser ModelOptions
  modelOptions = 
    do step       <- optionMaybe stepSize
       rateParams <- Parsec.option [] parseRateParams
       maxConcs   <- Parsec.option [] parseMaxConcs
       initConcs  <- Parsec.option [] parseInitConcs
       rateCalc   <- Parsec.option False parseRateCalc
       return ModelOptions { modelOptsStepSize   = step
                           , modelOptsRateParams = rateParams
                           , modelOptsMaxConcs   = maxConcs
                           , modelOptsInitConcs  = initConcs
                           , modelOptsRateCalc   = rateCalc
                           }
  stepSize :: Parser Int
  stepSize =
    do PepaParser.reserved "step-size"
       PepaParser.reserved "="
       PepaParser.intParser
  parseRateParams :: Parser BioPepa.RateParams
  parseRateParams =
    do PepaParser.reserved "rate-params"
       PepaParser.reserved "="
       PepaParser.squares $ Parsec.many parseRateParam
  parseRateParam :: Parser BioPepa.RateParameter
  parseRateParam =
    do name  <- BioPepaParser.biopepaRateName
       PepaParser.reserved "="
       value <- PepaParser.forgivingFloat
       return (name, value)
  parseMaxConcs :: Parser BioPepa.MaxConcentrations
  parseMaxConcs =
    do PepaParser.reserved "max-concentrations"
       PepaParser.reserved "="
       PepaParser.squares $ Parsec.many parseConcentration

  parseInitConcs :: Parser BioPepa.InitialConcentrations
  parseInitConcs =
    do PepaParser.reserved "initial-concentrations"
       PepaParser.reserved "="
       PepaParser.squares $ Parsec.many parseConcentration

  -- concentrations are the same for max concentration and
  -- for the initial concentrations
  parseConcentration :: Parser BioPepa.MaxConcentration
  parseConcentration = 
    do name <- BioPepaParser.biopepaComponentName
       PepaParser.reserved "="
       value <- PepaParser.intParser
       return (name, value)

  parseRateCalc :: Parser Bool
  parseRateCalc =
    do PepaParser.reserved "rate-calc"
       PepaParser.reserved "="
       rateFull <|> rateSimple
    where
    rateFull   = (PepaParser.reserved "full") >> return True
    rateSimple = (PepaParser.reserved "simple") >> return False

-- This is not *quite* correct but I think there should be a filename
-- parser available that someone has already written. If not perhaps
-- there is scope for a parsec-utils library on hackage.
fileNameParser :: Parser FilePath
fileNameParser = PepaParser.stringLiteral

transformTree :: Parser TransformTree
transformTree = variableTreeParser $ PepaParser.parens RulesParser.rulesParser

probeTree :: Parser ProbeTree
probeTree = variableTreeParser $ PepaParser.parens ProbeParser.probeDefParser

variableTreeParser :: Parser a -> Parser (VariableTree a)
variableTreeParser aParser =
  Parsec.choice [ liftM Vvariable variableParser
                , liftM Vconcrete aParser
                ]

modelParser :: Parser ModelTree
modelParser = 
  do PepaParser.reserved "Model"
     model <- parens PepaParser.pepaModel
     return $ Mmodel NoFile model


experimentParser :: Parser Ptree
experimentParser =
  do PepaParser.reserved "Experiment"
     variable <- variableParser
     values   <- squares $ Parsec.sepBy nameValuePairParser comma
     ptree    <- ptreeParser
     return $ Pexperiment variable values ptree

nameValuePairParser :: Parser (String, Value)
nameValuePairParser = 
  do name  <- nameParser
     symbol "="
     value <- valueParser
     return (name, value)
  where
  nameParser = PepaParser.upperOrLowerId <|> intString
  intString  = do i <- PepaParser.intParser
                  return $ show i
             
     
valueParser :: Parser Value
valueParser = Parsec.choice [ transformValue
                            , probeValue
                            , numberValue
                            , modelValue
                            ]

numberValue :: Parser Value
numberValue = liftM Vnumber PepaParser.forgivingFloat

modelValue :: Parser Value
modelValue = liftM Vmodel modelTreeParser

transformValue :: Parser Value
transformValue =
  do PepaParser.reserved "Transform"
     rules <- parens RulesParser.rulesParser
     return $ Vtransform rules

probeValue :: Parser Value
probeValue =
  do PepaParser.reserved "Probe"
     liftM Vprobe $ parens ProbeParser.probeDefParser

graphDataBaseParser :: Parser Ptree
graphDataBaseParser =
  do PepaParser.reserved "Data"
     database          <- databaseParser
     graphInstructions <- PepaParser.squares $ 
                          Parsec.many graphInstructionParser
     return $ PData  database graphInstructions 

graphInstructionParser :: Parser GraphInstruction
graphInstructionParser = 
  Parsec.choice 
    [ PepaParser.reserved "AllCandleStick" >> return AllCandleStick
    , PepaParser.reserved "AllTimeSeries"  >> return AllTimeSeries    
    , do PepaParser.reserved "Against"
         plotOpts  <- plotOptionsParser
         rateName  <- PepaParser.stringLiteral
         rateName2 <- optionMaybe PepaParser.stringLiteral
         return $ Against plotOpts rateName rateName2
    ]

databaseParser :: Parser (DataBaseTree)
databaseParser = 
  dataFile <|> dataFilter
  where
  dataFile =  do PepaParser.reserved "DataBaseFile"
                 fileName <- fileNameParser
                 return $ DataBaseFile fileName

  dataFilter = do PepaParser.reserved "DataFilter"
                  filters <- PepaParser.squares $ Parsec.many parseFilter
                  db      <- databaseParser
                  return $ DataFilter (Map.fromList filters) db

  parseFilter :: Parser (String, String)
  parseFilter = do l   <- PepaParser.stringLiteral
                   symbol "="
                   r   <- PepaParser.stringLiteral
                   return (l, r)