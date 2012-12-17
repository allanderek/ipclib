{-|
   The 'Model' datatype is a datatype which a parsed pepa model
   may be translated in to. It is more ammeniable to the kinds
   of compilation that we do. In addition we could potentially
   translate other kinds of models into this datatype.
-}
module Language.Pepa.Compile.Model
  ( Model
  , Parallel       ( .. )
  , Component
  , Transitions
  , Transition
  , Derivative

  , pepaToModel
  , derivativesOfModel
  , derivativesOfComponent
  , sequentialsOfModel

  , TransModel     ( .. )
  , Motion         ( .. )
  , modelToTransModel
  )
where

{- Standard Library Modules Imported -}
import Control.Arrow
  ( second )
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map
  ( Map )
import qualified Data.Set as Set
import Data.Set
  ( Set )
import qualified Text.PrettyPrint.HughesPJ as Pretty
import Text.PrettyPrint.HughesPJ
  ( Doc )
{- External Library Modules Imported -}
{- Local Modules Imported -}
import qualified Language.Pepa.QualifiedName as Qualified
import Language.Pepa.QualifiedName
  ( QualifiedName )
import qualified Language.Pepa.Rates as Rates
import Language.Pepa.Rates
  ( rateExpressionInt )
import qualified Language.Pepa.Syntax as Pepa
import Language.Pepa.Syntax
  ( ParsedModel       ( .. )
  , ActionIdentifier
  , ParsedComponent   ( .. )
  , CooperationSet    ( .. )
  , ParsedComponentId
  , ParsedTrans

  , nameOfAction
  )
import Language.Pepa.Print
  ( pprintActionList
  , pprintAngles
  , pprintCommaList
  )
import Language.Pepa.PepaUtils
  ( TransitionAnalysis      ( .. )
  , modelTransitionAnalysis
  , transitiveDerivatives -- May be able to write this better ourselves
                          -- given that we know the model is in normal
                          -- form we can use the named transitions?
                          -- Probably even better if we put that in
                          -- PepaUtils
  )
import qualified Language.Pepa.MainControl as MainControl
import Language.Pepa.MainControl
  ( MainControl )
{- End of Module Imports -}


type Model     = Parallel
data Parallel  = Cooperate Parallel [ ActionIdentifier ] Parallel
               | CompArray Parallel Int [ ActionIdentifier ]
               | Sequential (Derivative, Transitions) Component
                 deriving Eq
type Component   = Map Derivative Transitions
type Transitions = [ Transition ]
type Transition  = (ParsedTrans, Derivative)
type Derivative  = QualifiedName

sequentialsOfModel :: Model -> [ Component ]
sequentialsOfModel (Cooperate left _action right) =
  (sequentialsOfModel left) ++ (sequentialsOfModel right)
sequentialsOfModel (CompArray comp _ _)           =
  sequentialsOfModel comp
sequentialsOfModel (Sequential _ component)       =
  [ component ]

derivativesOfModel :: Model -> Set Derivative
derivativesOfModel (Cooperate left _action right) =
  Set.union (derivativesOfModel left) (derivativesOfModel right)
derivativesOfModel (CompArray comp _ _)           =
  derivativesOfModel comp
derivativesOfModel (Sequential _ mapping)         =
  Set.fromList $ derivativesOfComponent mapping

derivativesOfComponent :: Component -> [ Derivative ]
derivativesOfComponent = Map.keys 

actionNamesOfParallel :: Parallel -> Set ActionIdentifier
actionNamesOfParallel (Cooperate left _ right) =
  Set.union (actionNamesOfParallel left)
            (actionNamesOfParallel right)
actionNamesOfParallel (CompArray comp _ _)     =
  actionNamesOfParallel comp
actionNamesOfParallel (Sequential _ component) =
  Set.fromList $ map (Pepa.actionNameOfTrans . fst) allTrans
  where
  allTrans = concat $ Map.elems component


getInitialConcentrations :: Model -> [ (Derivative, Int) ]
getInitialConcentrations (Sequential (name, _) _)       = 
  -- We could add in zeros for the other derivatives
  [ (name, 1) ]
getInitialConcentrations (Cooperate left _actions right)     =
  (getInitialConcentrations left) ++ (getInitialConcentrations right)
getInitialConcentrations (CompArray comp size _actions) =
  map (second (* size)) $ getInitialConcentrations comp 

{-|
  Turn a PEPA model into a 'Model' description.
  This is a representation of the main system equation of the model
  which represents all of the derivatives at the (sequential) leaves
  of the system. In other words it's a main system with all of the model
  definitions represented internally.
-}
pepaToModel :: ParsedModel -> MainControl Model
pepaToModel model
  | badTrans > 0 = modelFormError "orphaned transitions"
  | otherwise    = 
    MainControl.addLogInformation "estimate-system" hprintModel system
  where
  system         = translateSystem $ modelSystemEqn model

  tAnalysis      = modelTransitionAnalysis model
  tNamed         = modelNamedTransitions tAnalysis

  -- If there are any 'badTrans' then this means that the model
  -- was not in normal form before we started and we should stop
  -- now and error.
  badTrans       = tNoSources + tNoTargets + tOrphans
  tNoSources     = length $ modelNoSourceTrans    tAnalysis
  tNoTargets     = length $ modelNoTargetTrans    tAnalysis
  tOrphans       = length $ modelOrphanTrans      tAnalysis

  -- The possible transitions from a given source name then,
  -- we basically just filter all of the possible transitions of
  -- the model on those with the source equal to the given derivative
  possibleTransitions :: Derivative -> Transitions
  possibleTransitions p = map snd $ filter ((== p) . fst) tNamed

  modelFormError :: String -> MainControl a
  modelFormError s = 
    MainControl.resultError $ 
      concat [ "Error: generating state space: model not in normal form"
             , "\n", s ]

  -- This should return a main control and then we needn't bother with
  -- the 'error' calls here, we can use 'MainControl.resultError' instead.
  translateSystem :: ParsedComponent -> MainControl Model
  translateSystem (IdProcess ident)                   = 
    return $ translateIdent ident
  translateSystem (ProcessArray comp size mActions) =
    do newComp <- translateSystem comp
       return $ CompArray newComp (rateExpressionInt size) actions
    where
    actions = maybe [] (map nameOfAction) mActions
  translateSystem (Cooperation left coopSet right)    =
    do tLeft  <- translateSystem left
       tRight <- translateSystem right
       let actions  = case coopSet of
                        ActionSet a -> map nameOfAction a
                        WildCard    -> Set.toList $ Set.intersection lActions rActions
           lActions = actionNamesOfParallel tLeft
           rActions = actionNamesOfParallel tRight
       return $ Cooperate tLeft actions tRight
  translateSystem (Hiding _p _actions)                =
    modelFormError "Hiding should have been removed from the model before now"
  translateSystem (PrefixComponent _ _)               =
    modelFormError $ "pepaToTimedSystemEqn: prefix components " ++ badEqnError
  translateSystem (ComponentSum _ _)                  =
    modelFormError $ "pepaToTimedSystemEqn: component sums " ++ badEqnError
  translateSystem (CondBehaviour _ _)                 =
    modelFormError $ "pepaToTimedSystemEqn: conditional behaviour " 
                     ++ badEqnError
  translateSystem (StopProcess)                       =
    modelFormError $ "pepaToTimedSystemEqn: stop processes " ++ badEqnError

  {-
    Todo: I definitely think this could be done much better. I think the
    way forward is for PepaUtils to have a function to return from a model
    a structure containing all the transitions. In four forms;
     1. Those transitions with a name source and target
     2. Those with only a named source
     3. Those with only a named target
     4. Those with neither.
   Then the first thing we do is check that 2,3 and 4 are all null, otherwise
   the model is not in normal form. Having done this we group the transitions
   by source and we simply have to look them up here.
  -}
  translateIdent :: ParsedComponentId -> Model
  translateIdent ident =
    Sequential (initial, initialTrans) mapping
    where
    -- The initial state is the 'ident' given in the parsed system equation
    initial       = ident
    -- The initial derivatives are of course those possible from the
    -- initial derivative.
    initialTrans  = possibleTransitions initial
    -- Our mapping then contains at least a mapping from the initial
    -- derivative to the initial transitions.
    singleMap     = Map.singleton initial initialTrans
    -- We gather up the names of the derivatives of this component.
    derives       = List.nub $ transitiveDerivatives model ident
    -- The rest of the mapping is made up by mapping obtaining the
    -- list of transitions possible from each of the derivatives and
    -- inserting those into the mapping starting with our obvious singleton
    -- mapping.
    mapping       = foldl insertDeriv singleMap derives
    insertDeriv :: Component -> Derivative -> Component
    insertDeriv cmap d = Map.insert d (possibleTransitions d) cmap
  badEqnError = "should not be in the main system equation"

{-| Pretty print the model and render it as a string -}
hprintModel :: Model -> String
hprintModel = Pretty.render . pprintModel

{-| Pretty print the model, note though that we do not display
    the transitions between the local derivatives (or any other transitions)
-}
pprintModel :: Model -> Doc
pprintModel = pprintComponent

pprintComponent :: Model -> Doc
pprintComponent (Cooperate left actions right)    =
  Pretty.sep [ pprintComponent left
             , pprintAngles $ pprintActionList actions
             , pprintComponent right
             ]
pprintComponent (CompArray comp size actions)     =
  Pretty.hsep [ pprintComponent comp
              , Pretty.brackets $ Pretty.int size
              , Pretty.brackets $ pprintActionList actions
              ]
pprintComponent (Sequential _initial derivatives) =
  Pretty.braces $ pprintCommaList derivativeDocs
  where
  derivativeDocs = map Qualified.pprintQualifiedName $ Map.keys derivatives


{-| A transition model or 'TransModel' is one in which we give the
    initial state plus a list of transitions which all have conditions
    upon when they fire. This is essentially like a hydra model but
    could be output for different formats.
-}
data TransModel = TransModel { tmodelTransitions :: [ Motion ]
                             , tmodelDerivatives :: [ [ Derivative ] ]
                             , tmodelInitial     :: [ (Derivative, Int) ]
                             }
data Motion     = Motion { motionDecrements :: [ Derivative ]
                         , motionIncrements :: [ Derivative ] 
                         , motionSpeed      :: Speed
                         -- a field for motion rate definitions.
                         , motionKind       :: ActionIdentifier 
                         }
-- type MotionKind = String
type Speed      = Rates.ModelRate
-- type SpeedDefs  = (QualifiedName, Rates.RateExpr)


modelToTransModel :: Model -> TransModel
modelToTransModel model =
  TransModel { tmodelTransitions = modelToMotions model
             , tmodelDerivatives = derivatives
             , tmodelInitial     = initial
             }
  where
  derivatives = map derivativesOfComponent modelComps
  modelComps  = sequentialsOfModel model
  initial     = getInitialConcentrations model

modelToMotions :: Model -> [ Motion ] 
modelToMotions (Sequential _ mapping)           =
  concatMap translateTrans (Map.toList mapping)
  where
  translateTrans :: (Derivative, [ (ParsedTrans, Derivative) ]) -> [ Motion ]
  translateTrans (source, localTrans) =
    map mkMotion localTrans
    where
    mkMotion :: (ParsedTrans, Derivative) -> Motion
    mkMotion (trans, target) =
      -- Watch out, what if source == target, we should probably make
      -- those the empty set in that case. Except that we also use the
      -- sources for the conditions, so we would need a separate conditions.
      Motion { motionDecrements = [ source ]
             , motionIncrements = [ target ]
             , motionSpeed      = speed
             , motionKind       = name
             }
      where
      -- The idea is that this solves two problems, the
      -- compilation of component arrays and the fact that the rate
      -- should be zero when there are no components in that state.
      speed = Rates.modifyRate (Rates.Cmult (Rates.Cident source)) $
              Pepa.pepaTransRate trans
      name  = Pepa.nameOfAction $ Pepa.pepaTransAction trans
modelToMotions (CompArray comp _size [])        =
  -- This does indeed look wrong, however see above that we multiple
  -- the rate by the number of component in that state.
  modelToMotions comp
modelToMotions (Cooperate left actions right)   =
  concat [ leftNonCoop, rightNonCoop, cooperations ]
  where
  cooperations = concatMap makeCoop actions
  rightNonCoop = filter isIndependent rightMotions
  leftNonCoop  = filter isIndependent leftMotions
  leftMotions  = modelToMotions left
  rightMotions = modelToMotions right

  isIndependent :: Motion -> Bool
  isIndependent m = not $ elem (motionKind m) actions  
 
  performsAction :: ActionIdentifier -> Motion -> Bool
  performsAction a m = a == (motionKind m)
 
  makeCoop :: ActionIdentifier -> [ Motion ]
  makeCoop action =
    [ combine l r | l <- leftRel, r <- rightRel ]
    where
    -- The Rel means relevant, ie performs the given action
    leftRel  = filter (performsAction action) leftMotions
    rightRel = filter (performsAction action) rightMotions

    -- These will fail if either are null but in that case it
    -- shouldn't be called as the list comprehension will be zero.
    overallLeftRate  = Rates.sumModelRates $ map motionSpeed leftRel
    overallRightRate = Rates.sumModelRates $ map motionSpeed rightRel 
 
    combine :: Motion -> Motion -> Motion
    combine l r = 
      Motion { motionDecrements = (motionDecrements l) ++
                                  (motionDecrements r)
             , motionIncrements = (motionIncrements l) ++
                                  (motionIncrements r)
             , motionSpeed      = speed
             , motionKind       = action
             }
      where
      speed = Rates.apparentRateExpr (motionSpeed l) 
                                     (motionSpeed r)
                                     overallLeftRate 
                                     overallRightRate
      
   

