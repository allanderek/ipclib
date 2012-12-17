{-| 
   This module implements the adding of probe components into the
   model. This also calls the translation routine found in
   'Language.Pepa.Probes.Translate'

   Actually you could say this is slightly bad design perhaps this should
   only do the actual addition, that is it should take in a pepa component
   to add to the model. This would of course be more general.
   If you are reading this and want this, give me a shout.

   SMALL note, scattering of @nub actions@ when adding a cooperation, these
   are probably not all necessary we could probably get away with doing it once.
   This is to avoid such things as @Probe <a,a> P@ but the action sets will normally
   be small so always 'nubbing' is safe and not too costly.
-}
module Language.Pepa.Probes.AddProbes
  ( -- * Adding the translated probe to the model
    addMasterProbe
  , addPassageProbe
  , addProbeDefs
  , addGlobalProbe
  , addLocalProbe
    
    -- * Re-exported from 'Language.Pepa.Probes.Translate'
  , ProbeTranslateFlags     ( .. )
  )
where

{- Imported Standard Libraries -}
import Data.List
  ( find
  , nub
  )
import qualified Data.Maybe as Maybe
import Data.Set
  ( toList )
{- Imported External Libraries -}
{- Imported Local Libraries -}
import Language.Pepa.QualifiedName
  ( QualifiedName     ( .. )
  , ShowOrig          ( .. )
  )
import Language.Pepa.Rates
  ( Rate              ( .. )
  , RateExpr          ( .. )
  , isImmediateRate
  , rateExprDecr
  , defaultWeight
  )
import Language.Pepa.Syntax
  ( ActionIdentifier
  , ParsedComponentId
  , ProcessDef
  , ParsedModel       ( .. )
  , ParsedComponent   ( .. )
  , CooperationSet    ( .. )
  , ParsedAction      ( .. )
  , Transition        ( .. )
  , ParsedTrans
  , defaultPepaPriority
  , actionNameOfTrans
  )
import Language.Pepa.PepaUtils
  ( addProcessDefinitions
  , allTransitionsOfModel
  )
import Language.Pepa.Transform.NameSupply
  ( NameSupply
  , getName
  , initialNameSupply
  )

-- Importing the 'ProbeR' data type from here is temporary
-- until I figure out the nice module structure for probes.
import Language.Pepa.Probes.Syntax
  ( ProbeDef
  , probeAlphabet
  )
import Language.Pepa.Probes.Translate
  ( translateProbeR
  , ProbeTranslateFlags     ( .. )
  --   , comActionPriority
  )
import Language.Pepa.MainControl
  ( MainControl )
{- End of Imports -}

{-| 
   Add the master probe, this is always the same probe.
   It has just two states @stopped@ and @running@.
-}
addMasterProbe :: String        -- ^ The name of the stopped probe component
               -> String        -- ^ The name of the running probe component
               -> [ String ]    -- ^ The names of the start actions
               -> [ String ]    -- ^ The names of the stop actions
               -> ParsedModel   -- ^ The input model to which to add the probe
               -> ParsedModel   -- ^ The returned probed model.
addMasterProbe stoppedName runningName startNames stopNames pmod =
   addProcessDefinitions masterDefs $ 
      addGlobalProbe masterActions masterComp pmod
   where
   masterComp      = IdProcess stoppedId
   (masterDefs,
    masterActions) = masterProbeDefinitions pmod startIds stopIds 
                                                 stoppedId runningId
   stoppedId       = Unqualified stoppedName
   runningId       = Unqualified runningName

   startIds        = map Unqualified startNames
   stopIds         = map Unqualified stopNames

   
{-| The master probe definitions are very simple.
    There are two states, running and stopped, and we
    synchronise only over the special communication actions
    /start/ and /stop/.
    This function takes in the stopped and running state
    names.
-}
masterProbeDefinitions :: ParsedModel -- ^ The model used to determine
                                      -- whether the model performs the action
                                      -- timed or immediately
                       -> [ ActionIdentifier ] -- ^ The start activities
                       -> [ ActionIdentifier ] -- ^ The stop activities
                       -> ParsedComponentId -- ^ The stopped process name
                       -> ParsedComponentId -- ^ The running process name
                       -> ([ ProcessDef ], [ ParsedAction ] )
masterProbeDefinitions _ [] _ _ _                                     =
   error "You must provide some start actions"
masterProbeDefinitions _ _ [] _ _                                     =
   error "You must provide some stop actions"
masterProbeDefinitions pmod startNames stopNames stoppedName runningName =
   ([ stoppedDef, runningDef ], allActions)
   where
   stoppedDef  = (stoppedName, stoppedProc)
   runningDef  = (runningName, runningProc)
   -- Astute readers will notice that they are both the same
   -- process IFF the sets of activities do not overlap.
   stoppedProc = foldr1 ComponentSum stoppedPrefixes
   runningProc = foldr1 ComponentSum runningPrefixes

   -- ie the prefixes which will make up the Stopped Process
   -- this consists of all the start actions which move to
   -- the running state plus self loops for all the stop actions
   -- which are not also start actions
   stoppedPrefixes = (map mkStartPrefix startNames) ++
                     (map mkStopPrefix onlyStopNames)

   -- ie all the prefixes which will make up the Running Process
   -- this consists of all the stop activities transitioning to the
   -- stop state plus all those activities which are ONLY start actions
   -- performing a self-loop
   runningPrefixes = (map mkStopPrefix stopNames) ++
                     (map mkStartPrefix onlyStartNames)

   -- The names which are only start actions
   onlyStartNames  = nub $ filter (not . (flip elem stopNames)) startNames
   -- The names which are only start actions
   onlyStopNames   = nub $ filter (not . (flip elem startNames)) stopNames


   -- okay not the most efficient way to get all the actions
   allActions      = map (pepaTransAction . makeProbeTransition modTrans)
                         allNames
   -- Note that this is all names but with each only occurring once.
   allNames        = onlyStartNames ++ stopNames
                                  

   mkStartPrefix :: ActionIdentifier -> ParsedComponent
   mkStartPrefix a = 
     PrefixComponent (makeProbeTransition modTrans a) runningIdP

   mkStopPrefix :: ActionIdentifier -> ParsedComponent
   mkStopPrefix a = 
     PrefixComponent (makeProbeTransition modTrans a) stoppedIdP
  
   stoppedIdP  = IdProcess stoppedName
   runningIdP  = IdProcess runningName


   -- All the transitions in the model, used to make a transition
   -- for a given action name
   modTrans      = allTransitionsOfModel pmod


{-
  The passage-probe is similar to the master probe.
  However we wish the probe to be running only in the state directly
  after the master probe has started to run. This is because of the
  way that hydra computes the passage-time quantiles.

  Note that we only require the activities which start the probe.
  All the activities which the model performs stop the probe.
-}
addPassageProbe :: String        -- ^ The name of the stopped probe component
                -> String        -- ^ The name of the running probe component
                -> [ String ]    -- ^ Names of activities which start the probe
                -> ParsedModel   -- ^ The model to which to attach the probe
                -> ParsedModel
addPassageProbe _ _ [] _            =
  error "you must provide some start names for a passage probe"
addPassageProbe stoppedName runningName startNames pmod =
  addProcessDefinitions passageDefs $
    addGlobalProbe passageActions passageComp pmod
  where
  -- The passage actions are all of the actions performed by
  -- the probe which are all of the actions performed by the
  -- model.
  passageActions = map pepaTransAction allProbeTrans

  -- The component to be added to the model, this should be the
  -- passage probe in the stopped state.
  passageComp     = IdProcess stoppedId

  -- The names of the two seqential components representing
  -- the two states of the passage probe.
  stoppedId       = Unqualified stoppedName
  runningId       = Unqualified runningName
   
  -- The passage definitions then are simple, we just define the
  -- stopped process to be the choice between all the stoppedPrefixes
  -- and similarly the running process to be the choice between
  -- all the running prefixes.
  passageDefs     = [ stoppedDef, runningDef ]
  stoppedDef      = (stoppedId, stoppedProcess)
  stoppedProcess  = foldr1 ComponentSum stoppedPrefixes
  runningDef      = (runningId, runningProcess)
  runningProcess  = foldr1 ComponentSum runningPrefixes

  -- All of the activities done by the model.
  modelActIds     = nub $ map actionNameOfTrans modelTrans
  -- All the transitions of the model, note that this should probably
  -- be all the transitions which are not hidden at the top level.
  -- Since it is possible that the user wishes to model from all the
  -- non-hidden occurrences of a particular activity.
  modelTrans      = allTransitionsOfModel pmod

  -- All transitions which will be used in the probe, this
  -- is all the actions of the model turned into transitions
  -- by noting whether or not they are immediate or timed
  -- activities, such that the probe performs these either
  -- immediate or for the latter passively.
  allProbeTrans   = map (makeProbeTransition modelTrans) modelActIds

  -- The start prefixes then we make a prefix for the start
  -- names by checking if their action name belongs to that
  -- of the list of start names. If so then it is a prefix to
  -- the running state else it is a self loop to the stopped state.
  stoppedPrefixes = map mkStopped allProbeTrans
  mkStopped :: ParsedTrans -> ParsedComponent
  mkStopped t
    | elem (showOrig $ actionNameOfTrans t) startNames =
      mkToRunningPrefix t
    | otherwise                                        =
      mkToStoppedPrefix t


  -- The running prefixes should just move all activity names
  -- back to the stopped process. However if the activity is
  -- an immediate action then we must self-loop. Why?
  -- Because otherwise it would be possible for the probe to be
  -- in the running state for an zero amount of time, this would
  -- be a diminishing state which would be hard for 
  -- So for the running prefixes then we just check if it is
  -- an immediate action performed by the transition.
  runningPrefixes = map mkRunning allProbeTrans
  mkRunning :: ParsedTrans -> ParsedComponent
  mkRunning t
    | isImmediateRate $ pepaTransRate t =
      mkToRunningPrefix t
    | otherwise                         =
      mkToStoppedPrefix t

  mkToRunningPrefix :: ParsedTrans -> ParsedComponent
  mkToRunningPrefix t = PrefixComponent t runningIdProcess

  mkToStoppedPrefix :: ParsedTrans -> ParsedComponent
  mkToStoppedPrefix t = PrefixComponent t stoppedIdProcess

  -- The stopped and running process are just the identifier
  -- components of their respective names
  stoppedIdProcess = IdProcess stoppedId
  runningIdProcess = IdProcess runningId


{------------------------------------------------------------------------
  -- The start actions then are provided by the caller
  startIds        = map Unqualified startNames
  -- For the start transitions we just map all the start names
  -- to transitions.
  startTrans      = map (makeProbeTransition modTrans) startIds  
    
  -- The stop transitions are all the actions performed by the model
  -- which are *not* a start activity. This is
  stopIds         = modelActIds \\ startIds
  -- And so for the stop trans we just map all those activities
  -- to transitions
  stopTrans       = map (makeProbeTransition modTrans) stopIds

  -- To make the stopped defintitions we want to move to the running
  -- state on all the start activities and self-loop to the stopped
  -- state on all the stopped activities.
  stoppedPrefixes = (map mkToRunningPrefix startTrans) ++
                    (map mkToStoppedPrefix stopWithoutStartTrans)

  -- The running processes should just move all activity names
  -- back to the stopped process. However if the activity is
  -- an immediate action then we must self-loop. Why?
  -- Because otherwise it would be possible for the probe to be
  -- in the running state for an zero amount of time, this would
  -- be a diminishing state which would be hard for 
  runningPrefixes  = runningSelfLoops ++ runningStops
  runningSelfLoops = map mkToRunningPrefix $ filter isImmediate stopTrans
  runningStops     = map mkToStoppedPrefix $ 
                         filter (not . isImmediate) stopTrans

  isImmediate :: ParsedTrans -> Bool
  isImmediate = isImmediateRate . pepaTransRate

---------------------------------------------------------------------------------
  -- All of the transitions in the model used by 'makeProbeTransition'
  -- to determine whether an action is performed immediately or as a timed
  -- activity by the model (and should thusly be performed by the probe
  -- at rate immediate or passive respectively).
  modTrans       = allTransitionsOfModel pmod


   -- The stopActs is everything so we do not need to add the startActs
   -- to them to get the passageActions.
   passageActions = stopActs


   startIds       = 

   -- The stopStart actions are those that are in the stop ids but *not*
   -- in the start ids.
   stopStartIds   = stopIds \\ startIds
   stopStartTrans = map (makeProbeTransition modTrans) stopStartIds
   stopActs       = map pepaTransAction stopTrans   


   stopTrans      = map (makeProbeTransition modTrans) stopIds
   
   

   stopIds        = 


  {-
    if the start and stop trans sets overlap???
    MUST BE CALLED WITH NON-OVERLAPPING SETS OF TRANSITIONS
  -}
passageProbeDefinitions :: [ ParsedTrans ] -> [ ParsedTrans ]
                       -> ParsedComponentId -> ParsedComponentId
                       -> [ ProcessDef ]
passageProbeDefinitions [] _ _ _                                     =
   error "You must provide some start actions"
passageProbeDefinitions _ [] _ _                                     =
   error "You must provide some stop actions"
passageProbeDefinitions startTrans stopTrans stoppedName runningName =
   [ stoppedDef, runningDef ]
   where
   stoppedDef  = (stoppedName, stoppedProc)
   runningDef  = (runningName, runningProc)
   -- Astute readers will notice that they are both the same
   -- process.
   stoppedProc = foldr1 ComponentSum $ toRunning ++ loopStopped
   runningProc = foldr1 ComponentSum toStopped

   -- actions in the stopped state, change to running or loop stopped
   toRunning   = map mkStartPrefix startTrans
   loopStopped =  map mkStopPrefix stopTrans
                  -- Note that we assume there is no overlap between
                  -- the two sets of start and stop trans.
                  -- (stopTrans \\ startTrans)

   -- actions in the running state, all toStopped
   toStopped   = map mkStopPrefix allTrans


   -- Again we make the assumption that the sets are non-overlapping
   allTrans    = {- nub $-} startTrans ++ stopTrans
   -- allPrefixes = (map mkStartPrefix startTrans) ++
      --            (map mkStopPrefix stopTrans)

   mkStartPrefix :: ParsedTrans -> ParsedComponent
   mkStartPrefix t = PrefixComponent t runningIdP

   mkStopPrefix :: ParsedTrans -> ParsedComponent
   mkStopPrefix t = PrefixComponent t stoppedIdP

   stoppedIdP  = IdProcess stoppedName
   runningIdP  = IdProcess runningName
-}


{-| Translate and add a series of probe definitions to a model 
    TODO: this should return a MainControl ParsedModel
-}
addProbeDefs :: [ ProbeTranslateFlags ] -> [ ProbeDef ]
             -> ParsedModel -> MainControl ParsedModel
addProbeDefs trFlags = 
   translateAndAdd initialNameSupply
   where
   translateAndAdd :: NameSupply -> [ ProbeDef ] -> ParsedModel
                   -> MainControl ParsedModel
   translateAndAdd _ns [] model                        =
      return model
   translateAndAdd ns ((mPattach, probe) : rest) model = 
      case mPattach of
         Nothing        ->
            translateAndAdd newNs rest probeModel
            where
            probeModel  = addProcessDefinitions pDefs probeModel1
            probeModel1 = addGlobalProbe actions probeComp model        
         Just attachId  ->
            case addLocalProbe  actions probeComp attachId model of
               Nothing   -> fail "Local probe could not be attached"
               Just pMod -> translateAndAdd newNs rest $
                              addProcessDefinitions pDefs pMod
      where
      pDefs       = translateProbeR trFlags startName transits probe
      -- translateProbe probe newNs startId stopId rPairs transits []
      
      probeComp   = IdProcess startId
      startId     = Unqualified startName
      newNs       = fst startNSRes
      startNSRes  = getName ns "Probe_" 
      startName   = (snd startNSRes) ++ "_start_state_yy"
      actionIds   = toList $ probeAlphabet probe
      transits    = map (makeProbeTransition modTrans) actionIds
      modTrans    = allTransitionsOfModel model
      actions     = map pepaTransAction transits




-- When creating the probe tracker it must know whether to add the
-- passive actions at rate 'infty' or at rate 'immediate' so this
-- function takes in an action and returns a parsed trans that can
-- be used in the probe tracker
makeProbeTransition :: [ ParsedTrans ] -> ActionIdentifier -> ParsedTrans
makeProbeTransition transitions a =
  maybe defaultTrans topifyRate $ find isRightAction transitions 
  where
  defaultTrans = Transition { pepaTransAction     = Action a
                            , pepaTransCoalsced   = []
                            , pepaTransRate       = topRate 
                            , pepaTransPriority   = defaultPepaPriority
                            , pepaTransConditions = []
                            }
  topRate       = RateTop defaultWeight
  isRightAction :: ParsedTrans -> Bool
  isRightAction (Transition { pepaTransAction = Action a2 })    = a == a2
  isRightAction (Transition { pepaTransAction = ComAction a2 }) = a == a2
  isRightAction _                                               = False

  -- If the rate is an immediate one then leave it as is, if however it
  -- is a timed or passive rate then turn it into a default passive rate
  topifyRate :: ParsedTrans -> ParsedTrans
  topifyRate trans
    | isImmediateRate $ pepaTransRate trans = trans
    | otherwise                             = trans { pepaTransRate = topRate }
 
{-| Adding a global probe is just the addition of the cooperation
    with the main system equation.
-}
addGlobalProbe :: [ ParsedAction ] -> ParsedComponent 
              -> ParsedModel -> ParsedModel
addGlobalProbe actions probeComp model =
   model { modelSystemEqn = cooperation }
   where
   cooperation = Cooperation mainCoop coopSet probeComp
   mainCoop    = modelSystemEqn model
   coopSet     = ActionSet $ nub actions
   {- actions     = map Action actionIds -}

{-| The addition of a local probe to a model.
    Here the probe is attached in cooperation with the given component
    name.
    This must essentially do breadth first search on the model
    (it doesn't necessarily have to be breadth first but if it is done
    this way then the modeller see his/her probe attached at the
    highest possible level.) Note that it is possible that it makes
    a difference, but then the person has not defined their model/probe
    very well.
    [@todo@] We do not actually do breadth first search, we just attempt
    to add it to the system equation and if that's not possible then we
    just attempt to add it to one of the process definitions, making
    sure that it is a parallel definition. This can actually result in
    an error. If we add it to a process definition which is used in a
    sequential position. Consider
    @
    P = (a, r).Q ;
    Q = (b, r).S ;
    S = Q
    S <a> S
    @
    If we attempt to add the probe to @Q@ we will get the model
    @
    P = (a, r).S ;
    Q = (b, r).P ;
    S = Q <a> Probe
    S <a> S
    @
    but @S@ is used in the sequential position in the definition
    of @P@. What would be nice is if the typing phase allowed the
    splitting of the definitions into parallel and sequential definitions.
    For now, provided that typing is done after the probe is added this
    will suffice.
-}
addLocalProbe :: [ ParsedAction ]    -- ^ The actions the probe observes
              -> ParsedComponent     -- ^ The probe component
              -> ParsedComponentId   -- ^ The component to attach to
              -> ParsedModel         -- ^ The model
              -> Maybe ParsedModel
addLocalProbe actions probeComp attachId model =
   case addToCoop $ modelSystemEqn model of
      Just newCoop -> return $ model { modelSystemEqn = newCoop }
      -- Notice we reverse the defs and then re-reverse them, this
      -- just means it is more likely that a definition near the
      -- system equation will be used to add the probe, lessening
      -- the chance that the above bug will occur.
      Nothing      -> do newDefs <- addToProcessDefs $ reverse pDefs
                         return $ model { modelProcessDefs = reverse newDefs }
   where
   pDefs = modelProcessDefs model
   -- We return the new component if it was possible to attach the
   -- the probe.
   addToCoop :: ParsedComponent -> Maybe ParsedComponent
   addToCoop (IdProcess ident)
      | attachId == ident = Just cooperation
      | otherwise         = Nothing
   addToCoop (Cooperation left acts right)  =
      case addToCoop left of
        Nothing      -> do newRight <- addToCoop right
                           return $ Cooperation left acts newRight
        Just newLeft -> return $ Cooperation newLeft acts right
   addToCoop (ProcessArray comp size mActs) =
     do probedElement <- addToCoop comp
        return $ makeCooperation probedElement
     where
     -- This is only called if we managed to attach the probe
     -- some where within the arrayed component. The component
     -- passed in here is the probed element of the array.
     -- Hence we wish to put that in cooperation with the original
     -- array except that the original array should have one less
     -- in it's initial concentration.
     makeCooperation :: ParsedComponent -> ParsedComponent
     makeCooperation probedElement
        -- If the size was less than one, then we've made an error.
        | (Cconstant x) <- size
          , x < 0               = 
           error "Process array with less than one components"
        | (Creal x)     <- size
          , x < 0.0             = 
           error "Process array with less than one components"
        -- If the array only had one element, then we just return
        -- the probed element.
        | (Cconstant 1) <- size = probedElement
        | (Creal 1.0)   <- size = probedElement
        -- If the array had 2 elements then we do not need an array
        -- for the remaining (unprobed) element
        | (Cconstant 2) <- size  = Cooperation probedElement arrayActs comp
        | (Creal     2) <- size  = Cooperation probedElement arrayActs comp
        | otherwise = Cooperation probedElement arrayActs $
                      ProcessArray comp (rateExprDecr size) mActs
     arrayActs = ActionSet $ Maybe.fromMaybe [] mActs
   addToCoop (Hiding comp hiddenActs)      =
      do newComp <- addToCoop comp
         return $ Hiding newComp hiddenActs
   -- If it's a sequential component then we cannot add it.
   addToCoop _                             =
      Nothing

   -- This is not exactly what we want as we would rather it add
   -- to something that was specifically in the main coop.
   addToProcessDefs :: [ ProcessDef ] -> Maybe [ ProcessDef ]
   addToProcessDefs []         = Nothing
   addToProcessDefs ((name, comp) : rest) =
      case addToCoop comp of
         Nothing -> do newRest <- addToProcessDefs rest
                       return $ (name, comp) : newRest
         Just c  -> Just $ (name, c) : rest

   -- The cooperation will always be the same
   cooperation = Cooperation (IdProcess attachId) coopSet probeComp
   coopSet     = ActionSet $ nub actions
   {- actions     = map Action actionIds -}
