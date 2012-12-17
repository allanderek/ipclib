module Language.Pepa.Compile.Odes
  ( convertPepaToOdes
  , transientResultFromOdeResult

  -- * External solving of a pepa model
  , solvePepatoOdesModel
  , solvePepatoOdesFile
  , solvePepatoOdesAveragePassage
  )
where

{- Standard Library Modules Imported -}
import Control.Arrow
  ( second )
import Control.Monad.Trans
  ( liftIO )
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import Data.Map
  ( Map )
import qualified Data.Set as Set
-- import qualified Text.PrettyPrint.HughesPJ as Pretty
-- import Text.PrettyPrint.HughesPJ
--   ( Doc )
import System.Exit
  ( ExitCode       ( .. ) )
import System.FilePath
  ( addExtension
  , dropExtension
  )
import qualified System.Process as Process
{- External Library Modules Imported -}
import qualified Horddes.Solve
import qualified Horddes.Odes as Horddes
import Horddes.Odes
  ( ChangeFunction    ( .. ) )
{- Local Modules Imported -}
import qualified Language.Pepa.Utils as Utils
import qualified Language.Pepa.Print as PepaPrint
import qualified Language.Pepa.Rates as Rates
import Language.Pepa.Rates
  ( RateExpr   ( .. )
  , Rate       ( .. )
  )
import qualified Language.Pepa.QualifiedName as Qualified
import Language.Pepa.QualifiedName
  ( QualifiedName )
import qualified Language.Pepa.Syntax as Pepa
import Language.Pepa.Syntax
  ( ParsedModel )
import qualified Language.Pepa.PepaUtils as PepaUtils
import Language.Pepa.Compile.GenMatrix
  ( SteadyReport      ( .. ) )
import qualified Language.Pepa.Compile.Uniformise as Uniformise
import Language.Pepa.Compile.Uniformise 
  ( TransientResult   ( .. ) )
import qualified Language.Pepa.Pepato.OutputParser as PepatoParser
import Language.Pepa.MainControl
  ( IOMainControl
  , liftMC
  , MainControl
  )
import qualified Language.Pepa.Transform.Simplify as Simplify
import qualified Language.Pepa.Compile.Model as Model
import Language.Pepa.Compile.Model
  ( Parallel   ( .. ) )
{- End of Module Imports -}

type Time = Horddes.Solve.Time
type Name = Horddes.Name

transientResultFromOdeResult :: Horddes.Solve.TimeSeries 
                              -> Uniformise.TransientResult
transientResultFromOdeResult oderesult =
  TransientResult { transientDistributions = map makeDistribution oderesult }
  where
  makeDistribution :: (Time, Map Name Double) -> (Time, SteadyReport)
  makeDistribution (time, mapping) =
    (time, SteadyReport { srPopulations = newMapping
                        , srStates      = []
                        , srThroughput  = Map.empty
                        }
    )
    where
    newMapping = Map.mapKeys Qualified.unqualified mapping



-- We could have a single derivative for both source and target
-- and then for cooperating components we create two intermediate
-- actions for the cooperating actions.
data IntermediateAction =
     IntermediateAction { interAction :: Pepa.ParsedAction
                        , interRate   :: ChangeFunction
                        , interSource :: [ Model.Derivative ]
                        , interTarget :: [ Model.Derivative ]
                        }

-- interAction :: IntermediateAction -> ParsedAction
-- interAction = pepaTransAction . interTransition

convertPepaToOdes :: ParsedModel -> MainControl Horddes.Odes
convertPepaToOdes parsedModel =
  do model <- Model.pepaToModel parsedModel
     let inters      = getModelInters model
         derivatives = Model.derivativesOfModel model
         processOdes = map (createOde inters) (Set.toList derivatives)
         virtualOdes = map createVirtualOde $ Pepa.modelVirtualComps parsedModel
     return $ concat [ processOdes, virtualOdes ]
  where
  -- The virtual odes don't actually evolve with time but are based
  -- purely on the current populations of the other components.
  -- The are derived from the model's virtual components.
  createVirtualOde :: Pepa.VirtualSpec -> Horddes.Ode
  createVirtualOde (name, expr) = 
    (Qualified.textual name,  convertRateExpr expr)

  -- From the model we make a list of intermediate transitions.
  -- These record the source and target derivatives of each
  -- action possible. We must combine these at cooperation points
  -- to achieve the correct rates.
  getModelInters :: Model.Model -> [ IntermediateAction ]
  getModelInters (Cooperate left actions right) =
     nonCoop ++ cooperating
     where
     leftInters  = getModelInters left
     rightInters = getModelInters right
     nonCoop     = (filter nonCooperating leftInters) ++
                   (filter nonCooperating rightInters)
     nonCooperating = not . isCooperating
     isCooperating :: IntermediateAction -> Bool
     isCooperating inter = elem (Pepa.nameOfAction $ interAction inter) actions
     cooperating = concatMap coopsOfAction actions
     coopsOfAction :: Pepa.ActionIdentifier -> [ IntermediateAction ]
     coopsOfAction action =
       [ createCoop l r | l <- leftCoops, r <- rightCoops ]
       where
       createCoop :: IntermediateAction -> IntermediateAction 
                   -> IntermediateAction
       createCoop leftAction rightAction =
         IntermediateAction { interAction = interAction leftAction
                            , interRate   = rate
                            , interSource = sources
                            , interTarget = targets
                            }
         where
         rate    = proper {- choose proper -- approx -}
         -- approx  = MinFun rateLeft rateRight
         proper  = Mult aprateLeft $ Mult aprateRight $ 
                        MinFun leftSum rightSum
                   
         aprateLeft
           | (length leftCoops) == 1  = Number 1
           | otherwise               = Divide rateLeft leftSum
         aprateRight
           | (length rightCoops) == 1 = Number 1
           | otherwise               = Divide rateRight rightSum
         rateLeft   = interRate leftAction
         rateRight  = interRate rightAction
         sources    = (interSource leftAction) ++ (interSource rightAction)
         targets    = (interTarget leftAction) ++ (interTarget rightAction)

       leftSum    = foldr1 Plus $ map interRate leftCoops
       rightSum   = foldr1 Plus $ map interRate rightCoops

       leftCoops  = filter relevant leftInters
       rightCoops = filter relevant rightInters       

       relevant :: IntermediateAction -> Bool
       relevant = (== action) . Pepa.nameOfAction . interAction

  getModelInters (CompArray comp _ _ )          =
    getModelInters comp
  getModelInters (Sequential _initial derivMap) =
    concat exits
    where
    exits = map convertTransitions $ Map.toList derivMap
    convertTransitions :: (Model.Derivative, Model.Transitions)
                       ->  [ IntermediateAction ]
    convertTransitions (derivName, transitions) =
      map convertTrans transitions
      where
      convertTrans :: Model.Transition -> IntermediateAction
      convertTrans (pTrans, targetName) =
        IntermediateAction { interAction = Pepa.pepaTransAction pTrans
                           , interRate   = changeFunRate
                           , interSource = [ derivName ]
                           , interTarget = [ targetName ]
                           }
        where
        changeFunRate = 
          -- So this should be the collective minimum of all the cooperations
          -- we do over this activity.
          (Mult (convertRate $ Pepa.pepaTransRate pTrans) 
                (NameAtT $ Qualified.textual derivName))

  -- So from the list of actions we create with 'getModelInters'
  -- we must create a list of odes. This essentially involves,
  -- for each derivative we get the actions which are exits
  -- (ie. it is one of the sources) and negate that rate for the
  -- odes and for all those where it is an entry (ie. for those
  -- in which it is one of the targets) we add the rate to the ode.
  -- simple as.
  createOde :: [ IntermediateAction ] -> Model.Derivative -> Horddes.Ode
  createOde intermediates derivative =
    (Qualified.textual derivative, foldr1 Plus (enters ++ exits))
    where
    enters = map interRate $ filter isEntry intermediates
    exits  = map (negateFun . interRate) $ filter isExit intermediates

    isEntry = (elem derivative) . interTarget
    isExit  = (elem derivative) . interSource

    -- negate a change function, easy enough we just minus it from
    -- zero, we just first check that it isn't already a minus from
    -- zero in which case we just give the original expression back.
    negateFun :: ChangeFunction -> ChangeFunction
    negateFun (Minus (Number 0) x) = x
    negateFun x                    = (Minus (Number 0) x)

        

convertRate :: Pepa.ParsedRate -> ChangeFunction
convertRate (RateTop _)       = error "Do not know how to do passive rates"
convertRate (RateImmediate _) = error "Do not know how to do immediate rates"
convertRate (RateTimed expr)  = convertRateExpr expr

convertRateExpr :: RateExpr -> ChangeFunction
convertRateExpr (Cident n)     = NameAtT $ Qualified.textual n
convertRateExpr (Creal r)      = Number r
convertRateExpr (Cconstant c)  = Number $ fromIntegral c
convertRateExpr (Cmult l r)    = Mult (convertRateExpr l)
                                      (convertRateExpr r)
convertRateExpr (Cdiv l r)     = Divide (convertRateExpr l)
                                        (convertRateExpr r)
convertRateExpr (Csub l r)     = Minus (convertRateExpr l)
                                       (convertRateExpr r)
convertRateExpr (Cadd l r)     = Plus  (convertRateExpr l)
                                       (convertRateExpr r)
convertRateExpr (Cgt  l r)     = GrtThan  (convertRateExpr l)
                                          (convertRateExpr r)
convertRateExpr (Cge  l r)     = GrtEq (convertRateExpr l)
                                       (convertRateExpr r)
convertRateExpr (Clt  l r)     = LessThan  (convertRateExpr l)
                                           (convertRateExpr r)
convertRateExpr (Cle  l r)     = LessEq  (convertRateExpr l)
                                         (convertRateExpr r)
convertRateExpr (Ceq  l r)     = Equal  (convertRateExpr l)
                                        (convertRateExpr r)
convertRateExpr (Cand l r)     = AndAlso  (convertRateExpr l)
                                          (convertRateExpr r)
convertRateExpr (Cor  l r)     = OrAlso  (convertRateExpr l)
                                         (convertRateExpr r)
convertRateExpr (Cnot e)       = Not $ convertRateExpr e
convertRateExpr (Cifte c l r)  = Ifte (convertRateExpr c)
                                      (convertRateExpr l)
                                      (convertRateExpr r)
convertRateExpr (Cminimum l r) = MinFun (convertRateExpr l)
                                        (convertRateExpr r)
{-
convertRateExpr e              =
  error $ unlines [ "Sorry I do not know how to do complex rate expressions"
                  , show e
                  ]
-}




prepareTemporaryPepatoFile :: 
     FilePath -- ^ filepath from which to derive
              --   a reasonable file name to use
  -> ParsedModel 
  -> IOMainControl (FilePath, Simplify.NameRateMap)
prepareTemporaryPepatoFile filePath model =
     -- Note that currently we run the model simplifier
     -- over the model before printing it out, this is because
     -- pepato does not allow such things as constant declarations
     -- (eg: P = S ;) and variable arrays.
     -- However we hope to take these out otherwise we will have
     -- some problems in mapping names back etc.
  do simpResult      <- liftMC $ Simplify.simplify [] model
     let sModel      = Simplify.simplifiedModel           simpResult
         -- aliasMap    = Simplify.simplifiedAliasMap        simpResult
         -- rateMap     = Simplify.simplifiedRateMapping     simpResult
         sMapping    = Simplify.combineSimplifiedMappings simpResult
         modelString =  PepaPrint.hprintPepaModel sModel
     liftIO $ writeFile file modelString
     return (file, sMapping)
  where
  file        = addExtension (dropExtension filePath) ".tmp.pepa"

{-| A function to obtain from a modified model transient analysis by
    writing the model out to a file and then using pepato via odes
    to obtain a time series analysis
-}
solvePepatoOdesModel :: FilePath     -- ^ filepath from which to derive
                                     --   a reasonable file name to use
                     -> ParsedModel   -- ^ The model to analyse
                     -> Time          -- ^ The start time
                     -> Time          -- ^ The stop time
                     -> IOMainControl Uniformise.TransientResult 
solvePepatoOdesModel filePath model startTime stopTime =
     -- Note that currently we run the model simplifier
     -- over the model before printing it out, this is because
     -- pepato does not allow such things as constant declarations
     -- (eg: P = S ;) and variable arrays.
     -- However we hope to take these out otherwise we will have
     -- some problems in mapping names back etc.
  do (file,sMapping)    <- prepareTemporaryPepatoFile filePath model
     tResult            <- solvePepatoOdesFile file startTime stopTime
     return $ addQualified sMapping tResult
  where
  -- From the simplification we may have turned some processes into
  -- multiple processes. For example P || P, becomes P_1 || P_2
  -- we thus get a mapping saying how to make the concentration of
  -- the process 'P'. It is 'P_1 + P_2'. However in the transient
  -- analysis result we will not have a mapping for 'P', only for
  -- 'P_1' and 'P_2'. So we create one, for each of the mappings in
  -- the rate mapping returned by the simplifier we create a transient
  -- distribution by using the concentrations of the mapping.
  addQualified :: Map String Rates.RateExpr
               -> Uniformise.TransientResult
               -> Uniformise.TransientResult
  addQualified mapping tResult =
    TransientResult { transientDistributions = newDists }
    where
    newDists = map makeDist $ transientDistributions tResult
    makeDist :: (Time, SteadyReport) -> (Time, SteadyReport)
    makeDist = second modifySteady
    modifySteady :: SteadyReport -> SteadyReport
    modifySteady sReport =
      SteadyReport { srPopulations = modifyMapping $ srPopulations sReport
                   , srStates      = []
                   , srThroughput  = Map.empty
                   }
    modifyMapping :: Map QualifiedName Double -> Map QualifiedName Double
    modifyMapping current =
      Map.union current newElems
      where
      -- Note the 'reduceRateExpr' used here is defined below and is
      -- NOT the same as Rates.reduceRateExpr for the reasons noted.
      newElems   = Map.map reduceRateExpr newKeys
      -- Make the keys of the mapping qualified names, recall that the
      -- mapping is the one returned from the simplifier.
      newKeys    = Map.mapKeys Qualified.unqualified mapping

      -- We'd like to use 'Rates.reduceRateExpr' but we unfortunately cannot
      -- the reason is, the names in 'mapping' are obtained from the output
      -- of pepato, so although it will be a qualified name, it will be
      -- represented as an unqualified one. So whereas in the initial pepa
      -- file it was 'P' it was then qualified to 'P_1' as a qualified name.
      -- But in the pepato output parser we parsed it as the unqualified name
      -- "P_1". I guess the real fix would be to have a syntax for qualified
      -- names, something like: P--1.
      reduceRateExpr :: Rates.RateExpr -> Rates.RateNumber
      reduceRateExpr = 
        Rates.reduceRateExprWith getNameValue
        where
        getNameValue :: Rates.RateIdentifier -> Rates.RateNumber
        getNameValue rIdent = 
          -- Essentially then we look in the list of names which currently
          -- have a population size for one which has the same textual
          -- representation as the one we are looking up.
          maybe 0.0 snd $ 
          List.find ((Qualified.equalTextual rIdent) . fst) currentShown
      -- 'currentShown' then is essentially the same mapping from names
      -- to double values that the current time's concentration map gives
      -- except that we use the textual representation rather than the
      -- qualified representation. Bit of a kludge this.
      currentShown = Map.toList current
      

{-| A function to obtain from a pepa source file a transient analysis
    using pepato via odes
-}
solvePepatoOdesFile :: FilePath     -- ^ The file name containing the model
                    -> Time          -- ^ The start time
                    -> Time          -- ^ The stop time
                    -> IOMainControl Uniformise.TransientResult 
solvePepatoOdesFile file startTime stopTime = 
  do liftIO $ putStrLn "calling pepato now"
     liftIO $ putStrLn pepatoCommand
     (pepOutput, pepResult) <- liftIO $ Utils.getProcessOutput pepatoCommand
     liftIO $ putStrLn "okay finished that"
     analyseResult pepOutput pepResult
  where
  analyseResult :: String -> ExitCode 
                -> IOMainControl Uniformise.TransientResult
  analyseResult _output (ExitFailure _) = 
    fail $ "Pepato command failed" ++ "\n\t" ++ pepatoCommand
  analyseResult output  (ExitSuccess)   =
    return result
    where 
    result        = TransientResult { transientDistributions = distributions }
    mapping       = PepatoParser.parseOdeOutput output
    distributions = map makeDistribution mapping
  makeDistribution :: (Time, Map QualifiedName Double) -> (Time, SteadyReport)
  makeDistribution (time, mapping) =
    (time, SteadyReport { srPopulations = mapping
                        , srStates      = []
                        , srThroughput  = Map.empty
                        }
    )
  pepatoCommand = unwords [ "pepato"
                          , "-ode"
                          , "-start-time"
                          , showRoundedFloat startTime
                          , "-stop-time"
                          , showRoundedFloat stopTime
                          -- , "-c Dlg_AppProcDataWait,UserWaiting,UserIdle"  
                          , file
                          ]

  showRoundedFloat :: Double -> String
  showRoundedFloat d =
    show i where i :: Int
                 i = round d
 
solvePepatoOdesAveragePassage :: 
     FilePath     -- ^ The file name containing the model
  -> ParsedModel  -- ^ The Pepa model to analyse
  -> Double       -- ^ The stop-time, the longest time to solve to
  -> String       -- ^ The component name indicating within passage
  -> String       -- ^ The activity name of the start action.
  -> IOMainControl Double
solvePepatoOdesAveragePassage filePath model stopTime component rateName =
     -- Note that currently we run the model simplifier
     -- over the model before printing it out, this is because
     -- pepato does not allow such things as constant declarations
     -- (eg: P = S ;) and variable arrays.
     -- However we hope to take these out otherwise we will have
     -- some problems in mapping names back etc.
  do (file, _sMapping) <- prepareTemporaryPepatoFile filePath model
     solvePepatoOdesAveragePassageFile file stopTime component rateValue
  where
  rateValue :: Double
  rateValue = case PepaUtils.getRateSpec model rateName of
                Just (_, Cconstant i) -> fromIntegral i  
                Just (_, Creal r)     -> r
                Just _                -> error "Cannot be an abstract rate"
                Nothing               -> error "Rate name not found"


solvePepatoOdesAveragePassageFile :: 
     FilePath     -- ^ The file name containing the model
  -> Double       -- ^ The stop time, ie longest-time to solve odes
  -> String       -- ^ The component name indicating within passage
--  -> String       -- ^ The activity name of the start action.
  -> Double       -- ^ The rate of the throughput activity
  -> IOMainControl Double
solvePepatoOdesAveragePassageFile file stopTime component rateValue =
  do pepatoResult <- liftIO $ 
                     Process.readProcessWithExitCode pepato pepatoArgs ""
     analyseResult pepatoResult
  where
  analyseResult :: (ExitCode, String, String) -> IOMainControl Double
  analyseResult (ExitFailure _, _output, _stdErr) = 
    fail $ "Pepato command failed" ++ "\n\t" ++ pepatoCommand
  analyseResult (ExitSuccess, output, _stdErr)    =
    return $ userWaiting / (userIdle * rateValue)
    where
    userWaiting   = numbers !! 1
    userIdle      = numbers !! 2    

    numbers       = makeNumbers lastLine
    lastLine      = last $ lines output
    makeNumbers :: String -> [ Double ]
    makeNumbers = (map read) . commaSeparated

    commaSeparated :: String -> [ String ]
    commaSeparated ""           = []
    commaSeparated (',' : rest) = commaSeparated rest
    commaSeparated s            = name : (commaSeparated rest)
                                  where
                                  (name, rest) = break (== ',') s

  startTime     = max 0.0 (stopTime - 5.0)

  pepato        = "pepato"
  pepatoArgs    = [ "-ode"
                  , "-start-time"
                  , Utils.myShowFloat startTime
                  , "-stop-time"
                  , Utils.myShowFloat stopTime
                  , "-c"
                  , component ++ ",UserIdle"
                  , file
                  ]                  
  pepatoCommand  = unwords (pepato : pepatoArgs)

