{-|
   A module for converting Pepa models to hydra models
-}

module Language.Pepa.Compile.Hydra
  ( hydraFlatModel
  , addHydraPassageSpecs
  , hydraHierModel
  , modelToHier
  )
where

{- Imported Standard Libraries -}
import Control.Monad
  ( liftM )
import qualified Data.Map as Map
import qualified Data.Set as Set
{- Imported Local Libraries -}
import Language.Pepa.QualifiedName
  ( unqualified )
import qualified Language.Pepa.Rates as Rates
import Language.Pepa.Rates
  ( Rate                   ( .. )
  , RateExpr               ( .. )
  )
import Language.Pepa.Syntax
  ( ParsedModel
  , ActionIdentifier
  , Transition             ( .. )
  , ParsedTrans
  , ParsedRate
  , nameOfAction
  )
import Language.Hydra.Syntax
  ( DNAMmodelFile         ( .. )
  , DNAMmodel             ( .. )
  , DNAMheader            ( .. )
  , DNAMstateVector
  , DNAMinitialVector  

  , DNAMstateDesc         ( .. )

  , DNAMtransition        ( .. )
  , DNAMcondition         ( .. )

  , DNAMspeed             ( .. )
  , DNAMaction
  , DNAMidentifier

  , DNAMsolutionControl
  , DNAMperformMeasure    ( .. )
  , DNAMpassageMeasure    ( .. )

  , DNAMcassignment       ( .. )
  , DNAMctype             ( .. )
  )
import qualified Language.Pepa.Compile.States as States
import Language.Pepa.Compile.States
  ( StateSpace             ( .. )
  , State                  ( .. )
  , StateMove
  , StateCondition
  , gatherStateConcentrations
  , getInitialConcentrations
  )
import qualified Language.Pepa.Compile.Model as Model
import Language.Pepa.Compile.Model
  ( Model
  , Parallel               ( .. )
  , Derivative
  )
import Language.Pepa.Utils 
  ( fst3
  , snd3
  , trd3
  )
import Language.Pepa.MainControl
  ( MainControl )
import qualified Language.Pepa.MainControl as MainControl
{- End of Imports -}


type HydraTrans     = DNAMtransition [ ActionIdentifier ] DNAMspeed
type HydraFlatModel = DNAMmodel [ ActionIdentifier ] DNAMspeed

{-|
   Given the entire state space of the model return a flatten
   hydra model for processing with Hydra
-}
hydraFlatModel :: StateSpace -> HydraFlatModel
hydraFlatModel statespace =
  DNAMmodel { modelHeaders     = [ header ]
            , modelStateVector = stateVector
            , modelConstants   = []
            , modelInitial     = initial
            , modelTransitions = transitions
            , modelInvariants  = []
            }
  where
  header               =  DNAMheader "/* it's good to be back */"
  -- The initial must set all possible derivatives to their initial values
  -- these are gleamed from the initial state where either they are
  -- present in a given concentration or absent in which case the
  -- concentration is zero.
  initial              = generateInitialVector allDerivatives 
                                               initialConcentations
  initialConcentations = gatherStateConcentrations $ spaceInitial statespace
  -- The state vector must declare all possible derivatives
  stateVector          = generateStateVector allDerivatives
  allDerivatives       = Set.toList $ spaceAllDerivs statespace

  transitions     = concatMap mkTransitions allStates
  allStates       = tangibleStates ++ vanishingStates
  tangibleStates  = Map.elems $ spaceTangible statespace
  vanishingStates = Map.elems $ spaceVanishing statespace

  mkTransitions :: State -> [ HydraTrans ]
  mkTransitions state = 
   map mkTransition $ zip [0..] (stateMovements state)
   where
   number          = show $ States.stateNumberInt state
   concentrations  = stateConcentrations state

   mkTransition :: (Int, StateMove) -> HydraTrans
   mkTransition (tNumber, (trans, targetRep)) =
     DNAMtrans { transName       = tName
               , transActionKind = map nameOfAction aKind
               , transConditions = conditions
               , transActions    = actions
               , transSpeed      = speed
               , transPriority   = pepaTransPriority trans
               }
     where
     speed     = case pepaTransRate trans of
                   RateImmediate d -> DNAMweight $ Creal d
                   RateTimed d     -> DNAMrate $ Creal d
                   RateTop _       -> 
                     error "Action performed passively without coop"


     -- The transition name is made from the number of the
     -- source state plus a subscript.
     tName = unqualified $ concat [ "trans_"
                                  , number
                                  , "_"
                                  , show tNumber
                                  ]
     -- The action kind is the action name plus all the coalasced
     -- immediate actions.
     aKind     = (pepaTransAction trans) : (pepaTransCoalsced trans)
     -- The conditions are the pepa transition conditions PLUS
     -- those which make up the current state, it might be that
     -- actually we could forget about the pepa conditions since
     -- the transition would not be included here if they were false
     -- in this state. But I leave them here for now since some of
     -- this code should obviously be re-usable for the hierarchical
     -- model translation.
     conditions      = pepaConditions ++ stateConditions
     pepaConditions  = map DNAMcond $ pepaTransConditions trans
     stateConditions = map mkDnamCondition $ Map.toList concentrations
     mkDnamCondition :: (Derivative, Int) -> DNAMcondition
     mkDnamCondition (name, i) =
       DNAMcond $ Ceq (Cident name) (Cconstant i)

     actions         = leaveActions ++ targetActions
     -- The leave actions are those derivatives which are present in
     -- in the source state but absent in the target state. For such
     -- derivatives we produce an action which simply sets it to zero.
     leaveActions    = map (makeZeroAction . fst) $ 
                       Map.toList (Map.difference concentrations targetConcs)
     -- The target actions are those which are present in the target state
     -- if so then we could detect that they are also present in the source
     -- state and hence emit no action for it, but easier just to do a
     -- straightforward assign
     targetActions   = map makeAssignAction $ Map.toList targetConcs
     targetConcs     = gatherStateConcentrations targetRep


     makeZeroAction :: Derivative -> DNAMaction
     makeZeroAction d = makeAssignAction (d, 0)
     makeAssignAction :: (Derivative, Int) -> DNAMaction
     makeAssignAction (d, i) = DNAMcassignment d (Cconstant i)



{-
  For both the flat and hierarchical hydra model outputs we must generate
  a state vector and an initial state vector. We provide two common functions
  for doing this. 
-}
generateInitialVector :: [ DNAMidentifier ] -> States.StateConcentrations 
                      -> DNAMinitialVector
generateInitialVector allDerivatives initialConcentations =
  map makeInitial allDerivatives
  where
  -- For all derivatives we must make an initial assignment.
  -- To do so we lookup the deriviative name in the initial concentrations
  -- either it is there and we simply return the assignment setting the
  -- derivative to its initial concentration or it is not present in
  -- the initial concentrations in which case it should be set to zero

  makeInitial :: Derivative -> DNAMcassignment
  makeInitial ident = 
    DNAMcassignment ident initialValue
    where
    initialValue =  maybe (Cconstant 0) Cconstant $ 
                    Map.lookup ident initialConcentations

generateStateVector :: [ DNAMidentifier ] -> DNAMstateVector
generateStateVector = map (DNAMstateDesc CShort)

type HydraHierModel    = DNAMmodel [ ActionIdentifier ] DNAMspeed
data IntermediateTrans =
  Trans { itransRate         :: ParsedRate
          -- The PEPA action name relating to the transition
        , itransKind         :: ActionIdentifier
          -- The derivatives cooperating over the action
          -- ie that must be present for the action to
          -- fire (and determine the rate).
        , itransDerivs       :: [ Derivative ]
        , itransStateUpdates :: [ DNAMaction ]
          -- The functional rate conditions
        , itransConds        :: [ RateExpr ]
        }


{-|
   Given the entire state space of the model return a flatten
   hydra model for processing with Hydra
-}
hydraHierModel :: ParsedModel -> MainControl HydraHierModel
hydraHierModel = liftM modelToHier . Model.pepaToModel

modelToHier :: Model -> HydraHierModel
modelToHier model = 
  DNAMmodel { modelHeaders     = [ header ]
            , modelStateVector = stateVector
            , modelConstants   = []
            , modelInitial     = initialVector
            , modelTransitions = transitions
            , modelInvariants  = []
            }
  where
  header               =  DNAMheader "/* it's good to be back */"

  -- The initial must set all possible derivatives to their initial values
  -- these are gleamed from the initial state where either they are
  -- present in a given concentration or absent in which case the
  -- concentration is zero.
  initialVector        = generateInitialVector allDerivatives 
                                               initialConcentations
  initialConcentations = getInitialConcentrations model
  -- The state vector must declare all possible derivatives
  stateVector          = generateStateVector allDerivatives
  allDerivatives       = Set.toList $ Model.derivativesOfModel model

  transitions          = map translateIntermediate $ 
                         zip [0..] (getParallelTrans model)
  translateIntermediate :: (Int, IntermediateTrans) -> HydraTrans
  translateIntermediate (i, itrans) =
    DNAMtrans { transName       = unqualified tName
              , transActionKind = [ itransKind itrans ]
              , transConditions = map DNAMcond $ itransConds   itrans
              , transActions    = itransStateUpdates itrans
              , transSpeed      = speed
              , transPriority   = 1
              }
    where
    tName     = "trans_" ++ (show i)
    speed     = case itransRate itrans of
                  RateImmediate e -> DNAMweight e
                  RateTimed e     -> DNAMrate e
                  RateTop _       ->
                    error "passive action at top level w/o cooperation"


  getParallelTrans :: Parallel -> [ IntermediateTrans ]
  getParallelTrans (Cooperate left actions right)     =
    concat [ leftNonCoop
           , rightNonCoop
           , cooperating
           ]
    where
    -- For each action in the cooperation set we map to
    -- a pair of lists. Each pair represents the list of
    -- transitions performed by the left side involving the
    -- given action and a list of transitions performed by the
    -- right side involving the given action.
    coopSets     = map getPerforming actions
    cooperating  = concatMap joinCoopSets coopSets
    
    joinCoopSets :: ([ IntermediateTrans ], [ IntermediateTrans ])
                  -> [ IntermediateTrans ]
    joinCoopSets (leftJoin, rightJoin) =
       [ cooperateTrans leftJoin rightJoin a b |
           a <- leftJoin
         , b <- rightJoin
       ]

    leftNonCoop  = filter (not . isInCooperationSet) leftTrans
    rightNonCoop = filter (not . isInCooperationSet) rightTrans
    leftTrans    = getParallelTrans left
    rightTrans   = getParallelTrans right

    isInCooperationSet :: IntermediateTrans -> Bool
    isInCooperationSet itrans =
      -- Is the intermediate transitions 'kind' in the cooperation set?
      elem (itransKind itrans) actions

    performsAction :: ActionIdentifier -> IntermediateTrans -> Bool
    performsAction action itrans = action == (itransKind itrans)

    getPerforming :: ActionIdentifier 
                 -> ([ IntermediateTrans ], [ IntermediateTrans ])
    getPerforming action =
       ( filter (performsAction action) leftTrans
       , filter (performsAction action) rightTrans )
  
    cooperateTrans :: [ IntermediateTrans ] -- All ways of left performing 'a'
                   -> [ IntermediateTrans ] -- All ways of right performing 'a'
                   -> IntermediateTrans     -- One particular from left
                   -> IntermediateTrans     -- One particular from right
                   -> IntermediateTrans
    cooperateTrans _leftAlts _rightAlts itrans1 itrans2 =
      itrans1 { itransRate  = combRate
              , itransConds = (itransConds itrans1) ++
                              (itransConds itrans2)
              }
      where
      -- Obviously this is complete and utter nonsense!
      combRate
        | Rates.isPassiveRate leftRate = rightRate
        | otherwise                    = leftRate
      leftRate  = itransRate itrans1
      rightRate = itransRate itrans2

      -- raLeft    = sumRates $ map itransRate leftAlts
      -- raRight   = sumRates $ map itransRate rightAlts
      -- sumRates  = foldr1 $ Rates.genRateBinop Cadd (+)

  getParallelTrans (CompArray _comp _size _actions) =
    error "Unimplemented component arrays"
  getParallelTrans (Sequential _ allTransMapping)  =
    concatMap translateTrans $ Map.toList allTransMapping

  translateTrans :: (Derivative, [ (ParsedTrans, Derivative) ])
                 -> [ IntermediateTrans ]
  translateTrans (source, localTrans) =
    map mkTransition localTrans
    where
    mkTransition :: (ParsedTrans, Derivative) -> IntermediateTrans
    mkTransition (trans, deriv) =
      Trans { itransRate         = -- Cmult (Cident source)
                                         (pepaTransRate trans)
            , itransKind         = nameOfAction $ pepaTransAction trans
            , itransDerivs       = [ source ]
            , itransStateUpdates = [ DNAMidentIncr deriv
                                   , DNAMidentDecr source
                                   ]
            , itransConds        = (Cgt (Cident source) (Cconstant 0))
                                   : (pepaTransConditions trans)
            }


{-|
   Given a Hydra model and a set of passage-time measurement specifications
   translate these into the language of hydra and add them to the hydra
   model to obtain a hydra model file.
-}
addHydraPassageSpecs :: DNAMmodel [ ActionIdentifier ] DNAMspeed
                        -- ^ The hydra model to process
                     -> DNAMsolutionControl
                        -- ^ The solution control to add
                     -> StateCondition
                        -- ^ The source condition
                     -> StateCondition
                        -- ^ The target condition
                     -> (Double, Double, Double)
                        -- The startTime, stopTime and timeStep respectively
                     -> DNAMmodelFile [ ActionIdentifier ] DNAMspeed
addHydraPassageSpecs hModel solControl sourceCond targetCond times =
  DNAMmodelFile hModel solControl [ passageMeasure ]
  where
  passageMeasure = DNAMpassage passage
  passage        = DNAMpassageMeasure 
                    { passageSourceCond = DNAMcond sourceCond
                    , passageTargetCond = DNAMcond targetCond
                    , passageStartTime  = fst3 times
                    , passageStopTime   = snd3 times
                    , passageStepTime   = trd3 times
                    }
  
