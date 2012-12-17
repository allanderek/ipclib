module Language.Pepa.Compile.GenMatrix
  ( getGeneratorMatrix
  , GeneratorMatrix         ( .. )
    -- Definitely want to factor out the steady state stuff
  , solveForSteadyState
  , createSteadyState

    -- Probably we might want to move the steady
    -- state stuff out to a different module.
  , ProbabilityDistribution

  , SteadyState             ( .. )
  , SteadyReport            ( .. )
  , PopulationMap
  , getSteadyStateReport
  , getThroughPutOfActions
  , showSteadyResults
  , pprintSteadyStateReport
  , hprintSteadyStateReport
  , compareWithPepatoReport
  , calculateAverageReponseTime
  )
where

{- Standard Library Modules Imported -}
import Control.Monad
  ( foldM )
import qualified Data.Map as Map
import Data.Map
  ( Map )
import Data.Maybe
  ( mapMaybe )
import qualified Text.PrettyPrint.HughesPJ as Pretty
import Text.PrettyPrint.HughesPJ
  ( Doc )
{- External Library Modules Imported -}
{- Local Modules Imported -}
import Language.Pepa.QualifiedName
  ( ShowOrig          ( .. )
  , QualifiedName     ( .. )
  )
import Language.Pepa.Syntax
  ( ParsedComponentId
  , ParsedAction
  , ActionIdentifier
  , Transition        ( .. )
  , nameOfAction
  )
import Language.Pepa.Rates
  ( rateNumber )
import qualified Language.Pepa.Compile.States as States
import Language.Pepa.Compile.States
  ( StateSpace        ( .. )
  , State             ( .. )
  , StateId
  , StateMove
  , StateCondition

  , moveAction
  , moveRate
  , moveCoalsced
  , conditionSatisfied
  )
import Language.Pepa.MainControl
  ( MainControl
  , resultError
  , valueResult
  )
import qualified Data.Matrix as Matrix
import qualified Language.Pepa.Compile.MarkovChain as Markov
import Language.Pepa.Compile.MarkovChain
  ( MarkovChain
  , Probability
  , ProbVector
  )
import qualified  Language.Pepa.Utils as Utils
import Language.Pepa.Pepato.OutputParser
  ( PepatoOut    ( .. ) )
{- End of Module Imports -}


{- 
  Note that this should probably also have some form of measurement
  specification
-}
{-| The type of the generator matrix is just a matrix -}
data GeneratorMatrix =
  GenMatrix { gmStates     :: StateSpace
            , gmMatrix     :: Markov.MarkovChain
            , gmTransposed :: Bool
            }

{-|
  The type of the steady state. Note that it is not a general
  Matrix, but a row Matrix, or probability vector.
-}
data SteadyState =
  SteadyState { steadyProbabilities :: ProbabilityDistribution
              , steadyStateSpace    :: StateSpace

                -- This is really a legacy field because this was
                -- initially how the 'steadyProbabilities' were stored
                -- and this is quite an easy thing to print out.
              , steadyProbsList     :: ProbabilityStates
              }
{-| The type of a probability distribution mapping states to their
    probability values. Note that if there is zero probability of
    being in that state then it may have no mapping here.
-}
type ProbabilityDistribution = Map StateId Double

type ProbabilityStates = [ (Probability, State) ]

{-|
  Returns the generator matrix from the state space provided.
  Note: this does *not* produce the solution column.
  
  For a bit of speed up, it's fairly tempting to compute Q^{t}
  that is, the transpose of Q rather than Q, since that is what
  we will ultimately use in 'solveForSteadyState'.
  However we would need to take that into consideration when we
  do such operations as finding the embedded Markov chain.
  So perhaps the 'GeneratorMatrix' type should hold, via constructor
  name, whether it is the transposed or the non-transposed matrix.
-}
getGeneratorMatrix :: Bool         -- ^ Whether we should generate a transposed
                                   --   generator matrix or not.
                   -> StateSpace   -- ^ The state space to generate from.
                   -> MainControl GeneratorMatrix
getGeneratorMatrix transposed statespace
  | anyVanishing              =
    resultError "Generator Matrix: State space contains vanishing states"
  | otherwise                 = 
    do matrix <- foldM makeRow initial [ 1 .. matrixSize ]
       let logKey  = "generator-matrix"
           logInfo = show matrix
           result  = GenMatrix { gmStates     = statespace
                               , gmMatrix     = matrix
                               , gmTransposed = transposed
                               }
       valueResult result logKey logInfo

  where
  initial         = Matrix.emptyMatrix 0.0 (matrixSize, matrixSize)
  -- The matrix is square so it has as many rows as columns.
  -- The reason it is one less than the number of states is that the
  -- final row is ultimately replaced by the solution vector
  -- (when the matrix is transposed the final row becomes the final column)
  matrixSize      = noStates -- - 1

  noStates        = Map.size tangible

  -- The tangible states in the statespace
  tangible        = spaceTangible statespace
  -- The mapping from state numbers to state id
  tangIds         = spaceTangIds statespace
  -- The vanishing states in the state space
  vanishing       = spaceVanishing statespace
  -- Are there any vanishing states? we cannot do anything with them so we
  -- return an error in the case that there are any left.
  anyVanishing    = not $ Map.null vanishing

  -- For each state id we have to make up a row, however we can
  -- add the row to the Markov chain by reversing the indicies so
  -- that we actually add it as a column. This means that the resulting
  -- Markov chain will be pre-transposed thus saving an expensive operation.
  -- For transient analysis so we require the non-transposed matrix,
  -- it will be the embedded markov chain that is solved for steady state
  -- anyway. So the outer-level function has a 'transposed' argument
  -- which is true if we should generate a transposed steady-state
  -- and false otherwise.
  makeRow :: MarkovChain -> Int -> MainControl MarkovChain
  makeRow markov i =
    -- Note that we are no longer detecting deadlock here, we are
    -- instead doing this in: L.P.C.States.generateStateSpace
    -- it might be worth doing it here since then we do this for
    -- otherways of generating the state space. However we would
    -- require someway to know whether we wish to ignore deadlocks
    -- for example when doing transient analysis
    return $ foldl addEntry selfLoopMatrix moves
    where
    stateId        = States.indexStateId 1 i
    selfLoopMatrix = Matrix.matrixInsert i i selfLoopValue markov
    state          = case Map.lookup stateId tangIds of
                       Just v  -> v
                       Nothing -> error errMsg
    errMsg         = "Gen Matrix: serious error, unfound state: " ++ (show i)
    
    addEntry :: MarkovChain -> (Double, Int) -> MarkovChain
    addEntry m (rate, targetId)
      -- Here we are not detecting a self-loop as an error
      -- only that the value should not be added to the generator
      -- matrix because the self-loop value has already been added.
      -- We allow them because it can sometimes be vaguely useful
      -- to allow self-loops in order to determine correct throughput
      -- or other competitive rates.
      | i == j     = m
      -- Note that we insert the wrong way around so that we
      -- are inserting it as a transposed matrix.
      -- We must insert with an addition because there may be several
      -- moves from this state to state j.
      -- The alternative is to first group the moves and sum their rates
      | transposed = Matrix.matrixInsertWith (+) j i rate m
      -- If we are not to generate a pre-transposed matrix then
      -- we add them to the matrix the correct way.
      | otherwise  = Matrix.matrixInsertWith (+) i j rate m
      where
      -- the target id has already been converted from a state identifier
      -- into a matrix/vector index via 'States.stateIdIndex' in makeMove.
      j = targetId
    
    -- moves is basically the movements of the given state 'stateId' except
    -- that we have turned the target from a state representation into a
    -- state number.
    moves         = map makeMove $ stateMovements state
    -- Turns the move of the state which has a state representation as
    -- the target into a one with a state identifier (number) as the target
    makeMove :: StateMove -> (Double, Int)
    makeMove (transition, staterepr)
      | Just s <- Map.lookup staterepr tangible  = 
        ( rateNumber $ pepaTransRate transition
        , States.stateIdIndex 1 $ stateNumber s)
        -- Really surprised if this ever matches since we check that
        -- 'vanishing' is null.
      | Just _ <- Map.lookup staterepr vanishing =
        error $ "GenMatrix: serious error, target state of move found"
                ++ " to be a vanishing state"
      | otherwise                                =
        error "GenMatrix: serious error, target state of move not found"

    -- subtract from zero the sum of all the moves out of this state.
    -- do avoid re-doing some arithmetic. 
    selfLoopValue = foldl (-) 0.0 $ map getOutValue moves

    -- This is the rate at which the move transitions out of the current
    -- state. For most moves this is of course just the value of the rate
    -- however we now have an option to allow self-loops hence we must
    -- essentially disregard the value of any self-loop in the calculation
    -- so we return zero here in the case that the target is the current
    -- state.
    -- Remember the 'index' of the move has already been translated from
    -- a tangible id into a matrix index.
    getOutValue :: (Double, Int) -> Double
    getOutValue (d, index)
      | index == i = 0.0
      | otherwise  = d

createSteadyState :: StateSpace -> ProbVector -> SteadyState
createSteadyState states solution =
  SteadyState { steadyProbabilities = probabilities
              , steadyStateSpace    = states
              , steadyProbsList     = steadyList
              }
  where
  -- Well this is pretty questionable in fact I think simply wrong
  -- it must translate the indicies of the solution vector into
  -- state ids. So if we just say that this is from Ints which are
  -- yet to be converted to state ids then it is fine.
  probabilities = Map.mapKeys (States.indexStateId 1) $ 
                  Matrix.getMap solution

  steadyList    = map getState $ Map.elems tangIdMap
  tangIdMap     = spaceTangIds states

  getState :: State -> (Probability, State)
  getState state =
    ( Matrix.safeVectorIndex solution $ 
      States.stateIdIndex 1 $ States.stateNumber state
    , state
    )





{-
  Solve the generator matrix for the steady-state solution.
  Of course this really just calls out to the 'Markov' module
  to do the actual solving of the matrix we just 'see through'
  the 'GeneratorMatrix' data type and package up the 
  probability distribution vector into the 'SteadyState' datatype.
-}
solveForSteadyState :: GeneratorMatrix -> MainControl SteadyState
solveForSteadyState gMatrix =
  valueResult steady "steady-probabilities" logInfo
  where
  steady    = createSteadyState states solution

  -- The steady state solution is obtained via the Markov module
  -- we simply have to convert from and to lists.
  genMatrix     = gmMatrix     gMatrix
  states        = gmStates     gMatrix
  transposed    = gmTransposed gMatrix

  solution      = Markov.solveForSteadyState transposed genMatrix

  -- Calculate the log information this is just essentially printing
  -- out the the 'steadyProbsList'
  -- This could probably be done more efficiently just by printing out
  -- 'steadyProbabilities' (although the states would have to be looked up)
  logInfo       = unlines $ (show solution) :
                            (map showProbState $ steadyProbsList steady)
  showProbState :: (Double, State) -> String
  showProbState (p, s) =
    (show p) ++ "\n" ++ (Utils.indentLine $ States.displayState s) 

-- We could just show the results directly, we choose
-- to split it up into the generation of a report and
-- subsequent displaying of this. The reason for this is that
-- we hope to be able to automatically test against pepato with
-- this intermediate report.
showSteadyResults :: SteadyState -> String
showSteadyResults = (hprintSteadyStateReport True) . getSteadyStateReport

getSteadyStateReport :: SteadyState -> SteadyReport
getSteadyStateReport steadystate =
  SteadyReport { srPopulations  = populations
               , srStates       = probStates
               , srThroughput   = throughput
               }
  where
  probStates  = steadyProbsList steadystate
  populations = populationMapFromProbStates probStates

  throughput = foldl addStateThroughputs Map.empty probStates
  
  addStateThroughputs :: ThroughputMap -> (Double, State) -> ThroughputMap
  addStateThroughputs currentMap (prob, state) =
    foldl addStateThroughMove currentMap $ stateMovements state
    where
    addStateThroughMove :: ThroughputMap -> StateMove -> ThroughputMap
    addStateThroughMove cmap move =
      -- We now add not only the action name of the move
      -- but also the coalsced immediate actions. They of course
      -- have the same throughput (at this state).
      foldl insertAction cmap $ action : coalsced
      where

      insertAction :: ThroughputMap -> ParsedAction -> ThroughputMap
      insertAction tmap act =
        Map.insertWith (+) (nameOfAction act) thr tmap

      -- The throughput is the same for all the actions, that is
      -- the actual timed activity plus all the coalsced immediate actions
      thr      = prob * (rateNumber $ moveRate move)
      action   = moveAction move
      coalsced = moveCoalsced move

populationMapFromProbStates :: ProbabilityStates -> PopulationMap
populationMapFromProbStates probStates =
  foldl addStatePopulations Map.empty probStates
  where
  addStatePopulations :: PopulationMap -> (Double, State) -> PopulationMap
  addStatePopulations currentMap (prob, state) =
    Map.foldWithKey addStatePopu currentMap concMap
    where
    concMap = stateConcentrations state
    addStatePopu :: ParsedComponentId -> Int -> PopulationMap -> PopulationMap
    addStatePopu _     0 cmap = cmap
    addStatePopu ident x cmap = 
      Map.insertWith (+) ident popValue cmap 
      where
      -- The value to add then is the population size multipied by the
      -- probability of being in this state.
      popValue = (fromIntegral x) * prob

data SteadyReport =
  SteadyReport { srPopulations  :: PopulationMap
               , srStates       :: ProbabilityStates
               , srThroughput   :: ThroughputMap
               }
               deriving (Show, Read)

type PopulationMap = Map.Map ParsedComponentId Double
type ThroughputMap = Map.Map ActionIdentifier  Double

getThroughPutOfActions :: SteadyReport -> [ ActionIdentifier ] -> Double
getThroughPutOfActions report =
  -- Okay questionable, basically if a given action is not in the
  -- throughput map then we simply say that it has a throughput of zero.
  -- but instead we should probably report an error.
  sum . mapMaybe getThrouhput
  where
  throughReport = srThroughput report
  getThrouhput k = Map.lookup k throughReport


{- TODO: obviously this is the wrong way around we should pretty
   print the report and then use Pretty.render to hprint the report
-}
pprintSteadyStateReport :: Bool -> SteadyReport -> Doc
pprintSteadyStateReport b =
  Pretty.text . hprintSteadyStateReport b

{-|
  Pretty print a steady-state report. The first argument is a boolean
  which if set to 'True' turns on full printing of the entire state
  space and the associated probabilities. If set to false we omit this
  and print the information most likely to be useful.
-}
hprintSteadyStateReport :: Bool -> SteadyReport -> String
hprintSteadyStateReport fullReport steadyReport = 
  unlines $ map unlines reportLines
  where
  reportLines
    | fullReport = briefReport ++ [ stateLines ]
    | otherwise  = briefReport

  briefReport    =  [ populationLines
                    , throughputLines
                    ]
  -- The utilisations 
  populationLines = map showPoplLine $ Map.toList utils
  utils           = srPopulations steadyReport

  showPoplLine :: (ParsedComponentId, Double) -> String
  showPoplLine (ident, prob) =
    concat [ showOrig ident
           , ": "
           , show prob
           ]
  -- Note that here we do not enumerate out the list of states
  -- In general this would be too large a list to print out, we
  -- put it in the log.

  -- throughput
  throughputLines = map showThroughLine $ Map.toList throughputMap
  throughputMap   = srThroughput steadyReport

  showThroughLine :: (ActionIdentifier, Double) -> String
  showThroughLine (ident, rate) = 
    concat [ showOrig ident
           , ": "
           , show rate
           ]

  stateLines      = map showStateLine $ srStates steadyReport
  showStateLine ::(Probability, State) -> String
  showStateLine (probability, state) =
    unlines [ States.displayState state
            , Utils.indentLine $ "Probability: " ++ (show probability)
            ]


{-
  Given a steady-state analysis of a given pepa file and return
  whether or not the two analyses agree. Rather than returning 
  a boolean we return a list of the error reports, if this is
  the empty string then the two agree.
  This may be changed if we want to report things which are not
  necessarily a disagreement, for example pepato not reporting
  the throughput of immediate actions. This would probably mean
  returning two strings.
-}
compareWithPepatoReport :: SteadyState -> PepatoOut -> [ String ]
compareWithPepatoReport steadyState pepatoOut =
  concat [ statesSizeErrors
         , throughputErrors
         ]
  where
  -- The steady state report from ipc this will be used to compare
  -- against the output from pepato
  steadyReport       = getSteadyStateReport steadyState

  -- The states size error, first check that we have the same number
  -- of states.
  statesSizeErrors    
    | numberIpcStates == numberPepatoStates = []
    | otherwise                             =
      [ unlines [ "The number of states in ipc: " ++ show numberIpcStates
                , "The number of states in pepato: " ++ show numberPepatoStates
                ]
      ]
  numberIpcStates    = length . srStates $ steadyReport
  numberPepatoStates = pepatoStates pepatoOut

  -- Now we check the throughput, we just check that all the throughput actions
  -- from Pepato are mapped to the same thing in the ipc steady state report.
  -- Using mapMaybe if the throughput maps agree then we it will be the empty
  -- list. Note that we could also check that the size of the throughput
  -- maps are the same, since currently if ipc reports more actions than
  -- pepato we do not detect this, I'm leaving this for  now because pepato
  -- doesn't report the throughput of immediate actions (or even allow them).
  throughputErrors = mapMaybe throughputDisagree$ pepatoThroughPut pepatoOut
  throughPutMap    = srThroughput steadyReport

  -- We return 'Nothing' if the ipc throughput report agrees with the pepato
  -- one and 'Just errorMessage' if the action is not present in the ipc
  -- throughput report or the values are different.
  throughputDisagree :: (String, Double) -> Maybe String
  throughputDisagree (actionName, pepValue) =
    case Map.lookup (Unqualified actionName) throughPutMap of
      Nothing -> Just $ unwords [ actionName
                                , "present in the pepato output"
                                , "but not present in the ipc output"
                                ]
      Just d  -> if doublesAgree pepValue d
                    then Nothing 
                    else Just $ unlines [ "For the action: " ++ actionName
                                        , "Pepato reports: " ++ show pepValue
                                        , "Ipc reports:    " ++ show d
                                        ]

  -- This is essentially forgiving string comparison, it checks if two
  -- doubles are within a threshold of each other. 
  threshold  = 0.000001
  doublesAgree :: Double -> Double -> Bool
  doublesAgree d1 d2 = 
    (abs (d1 - d2)) < threshold
                            
    

{-|
  Calculates the average response-time of a system. The average response-time
  is given by a use of Little's law. We calculate the percentage of time spent
  in the given passage and divide that through by how often the passage occurs.
  This is found by the throughput of the start actions (the actions which start
  the passage). Note that in general the condition given in will be that a given
  probe component is in the running state and the start actions will be those
  will transition the probe into the running state. However we do not check this
  so you could use this function to calculate some random relationship which does
  not correspond to a response-time.
-}
calculateAverageReponseTime :: StateCondition       
                               -- ^ The probe running condition
                            -> [ ActionIdentifier ] 
                               -- ^ The start actions
                            -> SteadyState
                               -- ^ The steady state
                            -> Double
                               -- ^ The returned average response-time
calculateAverageReponseTime runningCond startActs steady =
    inCritical / through
    where
    -- The throughput of all the start actions.
    -- NOTE: there is a possible bug here if the start
    -- actions contain to immediate actions which are
    -- coalasced into the same timed action. Although
    -- this means that the immediate actions must be in
    -- sequence and hence you've probably got a silly
    -- measurement.
    -- QUESTIONABLE: use of 'mapMaybe' here, basically
    -- none of these should be 'Nothing'.
    through    = sum $ mapMaybe lookupAction startActs
    lookupAction :: ActionIdentifier -> Maybe Double
    lookupAction a = Map.lookup a throughMap
    
    -- The steady state report
    sreport    = getSteadyStateReport steady
    -- The map of throughput of actions.
    throughMap = srThroughput sreport

    -- inCritical is the probability that we are currently
    -- within the middle of the passage, we get this by
    -- summing the probability of being in a state in which
    -- the running condition is true.
    inCritical   = getProbabilityOfCondition runningCond sreport

{-|
  Returns the probability that some condition is true.
  So sums the probablities of all the states in which the condition
  is true.
-}
getProbabilityOfCondition :: StateCondition -> SteadyReport -> Double
getProbabilityOfCondition condition sreport =
  sum $ map getProb steadyStates
  where
  steadyStates = srStates sreport

  getProb :: (Double, State) -> Double
  getProb (d, state)
    | conditionSatisfied (stateConcentrations state) condition = d
    | otherwise                                                = 0.0
    