{-|
  This module represents the implementation of analysis over pepa
  models. These should be static analyses and hence should operate over
  the full parsed syntax, although it may be convenient to do some desugaring
  prior to the actual analysis.
-}
module Language.Pepa.Analysis.Analysis 
   ( AnalysisReport      ( .. )
   , hprintAnReport
   , hprintAnMessage
   , hprintAnResult
   , messagesOfReport
   , splitMessages
   , analyseModel
   
   -- * Results of individual analyses
   , AnalysisMessage     ( .. )
   , AnalysisResult      ( .. )
   , AliasMap
   
   -- * Individual analysis functions
   , possibleSuccessors
   , possibleActionsNonAliased
   , componentPerformsActions
   
   , ConcentrationMap
   , getInitialConcentrations

   , ratesUsedInProcessDefs
   , ratesUsedInComponent
   )
where

{- Standard Modules Imported -}
import Control.Arrow
  ( first
  , second
  )
import Prelude hiding 
 ( lookup )
import Data.List
  ( nub
  , (\\)
  , intersect
  , union
  -- , partition
  , find
  )
import qualified Data.Map as Map
import Data.Map
  ( Map )
import Data.Maybe 
  ( fromMaybe 
  , maybeToList
  , isNothing
  )
import qualified Data.Set as Set
import Data.Set
  ( Set )
{- Local Modules Imported -}
import qualified Language.Pepa.Analysis.FixMap as FixMap
import Language.Pepa.Analysis.FixMap
  ( FixMap
  , empty
  --   , insertWith
  , insertList
  , lookup
  , toMap
  , fixPoint
  , toList
  )
import qualified Language.Pepa.Rates as Rates
import Language.Pepa.Rates
  ( Rate            ( .. )
  , namesOfRateExpr
  )
import qualified Language.Pepa.Syntax as Pepa
import Language.Pepa.Syntax
  ( ParsedModel          ( .. )
  , ProcessDef
  , ParsedComponent      ( .. )
  , CooperationSet       ( .. )
  , ParsedComponentId
  , RateSpec
  , RateIdentifier
  , ActionIdentifier
  , Transition           ( .. )
  , ParsedAction         ( .. )
  , nameOfAction
  , isTauAction
  , ParsedRate
  )
import Language.Pepa.PepaUtils
  ( findProcessDefinition )
import Language.Pepa.Analysis.Typing
  ( typeModel )
import qualified Language.Pepa.Print as Print
import qualified Language.Pepa.Utils as Utils
{- End of Imports -}

{-
  List of analyses I should add:
  One that checks if an action escapes from the main system equation
  with rate Top. This should be quite simple just recurse through the
  system and check if a component does an action at rate Top.
  If it is a cooperation then lose those actions which cooperate and which
  one side of the component does not do at rate Top.

  One that checks for zero rates, I actually think it should incorporate
  the command line rate options for this, in that it should take in a list
  of overrides possible and check if the rates are *ever* zero,
  this would be very good for condor related goodness.
-}


{-| The type of a static analysis warning or error, a list of such
    analysis messages is the result of many of the static analyses, where
    by an empty list returned indicates that the model passed the analysis.
    The idea is that all of the messages are of a single type, but they may
    be wrapped in 'AnalysisError' or 'AnalysisWarning' as per the default.
    Then any caller is free to write a transformation function which, 
    for example, turns all warnings into errors, or only a subset of
    the warnings into errors.
-}
data AnalysisMessage = AnalysisError   AnalysisResult
                     | AnalysisWarning AnalysisResult

{-| The type of information returned from an analysis, 
    which should be packaged up
    in the 'AnalysisMessage' type to indicate whether, by default, 
    it is an error or a warning.
-}
data AnalysisResult = 
    -- | The process cooperates over the first set of actions
    -- but neither side of the cooperation actually performs
    -- the second set of actions (perhaps because they are the same
    -- for example in a proces array)
    ExcessActions ParsedComponent [ ParsedAction ] [ ParsedAction ]
    -- | The process is a parallel cooperation which cooperates 
    -- over the first set of actions but the left hand side never
    -- actually performs the second set
    | ExcessActionsLeft ParsedComponent [ ParsedAction ] [ ParsedAction ]
    -- | The process is a parallel cooperates which cooperates
    -- over the first set of actions but the right hand side never
    -- actually performs the second set of actions.
    | ExcessActionsRight ParsedComponent [ ParsedAction ] [ ParsedAction ]
    -- | The process hides the first set of actions but does
    -- does not actually perform the second set
    | NeedlessActionsHide ParsedComponent [ ParsedAction ] [ ParsedAction ]
    -- | The process identifiers which are used but are not defined
    | UsedButUndefinedProcesses  [ ParsedComponentId ]
    -- | The process identifiers which are defined but not used
    | DefinedButNotUsedProcesses [ ParsedComponentId ]
    -- | The process identifiers which are defined more than once
    | DefinedMultiplyProcesses   [ ParsedComponentId ]
    -- | An alias loop such as @ P = P @
    | AliasLoop [ ParsedComponentId ]
    -- | The main system composition contains a self-loop over the given actions
    | SystemSelfLoop ParsedComponent [ ParsedAction ]
    -- | Rates are defined but not used
    | DefinedButNotUsedRates  [ RateIdentifier ]
    -- | Rates which are used but not defined
    | UsedButUndefinedRates   [ RateIdentifier ]
    -- | Rates which are defined more than once
    | DefinedMultiplyRates    [ RateIdentifier ]
    -- | A generic typing error, should probably change this.
    | TypingError             String

{-| The type of the result of analysing an entire module.
    This is essentially a large record type.
    The individual analyses should not be required to be exported since
    a programmer can just analyse the whole module but then only examine 
    the result of the analyses they are interested in, due to lazy evaluation
    the ignored analyses will not actually be performed.
-}
data AnalysisReport = 
    AnalysisReport
        { -- | Does the parsed model contain process arrays?
          processArraysPresent :: Bool 
          -- | A mapping from process identifiers to the actions they perform.
          --   Not just those that can be performed immediate by the given
          --   process but by those which some derivative may perform.
        , processActionsMap    :: ProcessActionMap
          -- | A list of actions performed (and not hidden) by
          -- the main composition.
        , compositionActions   :: [ ParsedAction ]
          -- | A list of reports on excess actions in cooperations 
          -- involved in the given pepa model
        , excessActionsReport  :: [ AnalysisMessage ]
          -- | A list of the process names defined within the model
          -- Note we may wish for a set here, but we need to be careful
          -- because we analyse for multiply defined names, and we won't
          -- be able to do that with simply a set, I guess we need a
          -- MultiSet.
        , definedProcessNames  :: [ ParsedComponentId ]
          -- | A list of process names used within the model
        , usedProcessNames     :: [ ParsedComponentId ]
          -- | Results of analysing the defined and used process names
        , definedNamesReport   :: [ AnalysisMessage ]
          -- | An alias database mapping process names to those names
          -- they can become without performing any actions.
        , aliasDataBase        :: AliasMap
          -- | Results of checking for alias loops
        , aliasLoops           :: [ AnalysisMessage ]
          -- | Results of analysing for self-loops
        , selfLoopsReport      :: [ AnalysisMessage ]
          -- | The used action names
        , usedActionNames      :: [ ActionIdentifier ]
          -- | Rate names defined
        , definedRateNames     :: [ RateIdentifier ]
          -- | Used rate names
        , usedRateNames        :: [ RateIdentifier ]
          -- | Results of analysing the rate names defined\/used.
        , rateNamesReport      :: [ AnalysisMessage ]
          -- | Results of the typing phase
        , typingErrors         :: [ AnalysisMessage ]
        }

{-------------------------------------------------------
 The Analysis Print functions
--------------------------------------------------------}
{-|
    Print an entire analysis report.
    [@todo@] consider moving this into the 'Language.Pepa.Analysis.Analysis' 
    module since I think there it would be easier to maintain.
    To do this though I think
    we would need to restructure things a little, in particular the
    /Analysis/ module would depend on this module rather than vice versa
    and so the 'hprintMessages' function below would need to be moved
    there as well.
-}
hprintAnReport :: AnalysisReport -> String
hprintAnReport report = 
    unlines [ if processArraysPresent report
              then "The model contains process arrays"
              else "The model does not contain any process arrays"
            , pActionsMap
            , "The following actions are performed (and not hidden)"
              ++ " by the main composition " ++
              (Print.hprintCooperationSet $ ActionSet $ 
                 compositionActions report)
            , hprintMessages $ messagesOfReport report
            ]
    where
    pActionsMap = unlines $ map hprintPidActPair 
                    (Map.toList $ processActionsMap report)

    hprintPidActPair :: (ParsedComponentId, [ ParsedAction ] ) -> String
    hprintPidActPair (pid, actions) =
        unwords [ Print.hprintComponentName pid
                , ":"
                , Print.hprintCooperationSet $ ActionSet actions
                ]


{-|
    Human print just the messages, that is the warnings and errors
    of a full analysis report.
-}
hprintMessages :: [ AnalysisMessage ] -> String
hprintMessages messages =
    unlines [ "There were " ++ show noErrors ++ " errors."
            , "There were " ++ show noWarnings  ++ " warnings."
            , "The warnings were:"
            , warningLines
            , "The errors were :"
            , errorLines
            ]
    where
    errorLines   = unlines $ map hprintAnResult errors
    errors       = fst warnsErrs
    noErrors     = length errors
    warnings     = snd warnsErrs
    noWarnings   = length warnings
    warningLines = unlines $ map hprintAnResult warnings
    warnsErrs    = splitMessages messages 


hprintAnMessage :: AnalysisMessage -> String
hprintAnMessage (AnalysisError message)   = 
  "Error: " ++ hprintAnResult message
hprintAnMessage (AnalysisWarning message) = 
  "Warning: " ++ hprintAnResult message

hprintAnResult :: AnalysisResult -> String
hprintAnResult ( ExcessActions par acts1 acts2 )       =
    unwords [ "The process:"
            , Print.hprintComponent par
            , "cooperates over the actions"
            , Print.hprintCooperationSet $ ActionSet acts1
            , "but neither side performs the actions:"
            , Print.hprintCooperationSet $ ActionSet acts2
            ]    
hprintAnResult ( ExcessActionsLeft par acts1 acts2 )   =
    unwords [ "The process:"
            , Print.hprintComponent par
            , "cooperates over the actions"
            , Print.hprintCooperationSet $ ActionSet acts1
            , "but the left hand side does not perform the actions:"
            , Print.hprintCooperationSet $ ActionSet acts2
            ]
hprintAnResult ( ExcessActionsRight par acts1 acts2 )  =
    unwords [ "The process:"
            , Print.hprintComponent par
            , "cooperates over the actions"
            , Print.hprintCooperationSet $ ActionSet acts1
            , "but the right hand side does not perform the actions:"
            , Print.hprintCooperationSet $ ActionSet acts2
            ]
hprintAnResult ( NeedlessActionsHide par acts1 acts2 ) =
    unwords [ "The process:"
            , Print.hprintComponent par
            , "hides the actions"
            , Print.hprintCooperationSet $ ActionSet acts1
            , "but does not ever perform the actions:"
            , Print.hprintCooperationSet $ ActionSet acts2
            ]
hprintAnResult ( UsedButUndefinedProcesses pids )      =
    unwords [ "The following process identifers are used but not defined"
            , hprintProcessIdList pids
            ]
hprintAnResult ( DefinedButNotUsedProcesses pids )     =
    unwords [ "The following process identifers are defined but not used"
            , hprintProcessIdList pids
            ]
hprintAnResult ( DefinedMultiplyProcesses pids )       =
    unwords [ "The following process identifers are defined more than once"
            , hprintProcessIdList pids
            ]
hprintAnResult ( UsedButUndefinedRates rids )      =
    unwords [ "The following rate identifers are used but not defined"
            , hprintProcessIdList rids
            ]
hprintAnResult ( DefinedButNotUsedRates rids )     =
    unwords [ "The following rate identifers are defined but not used"
            , hprintProcessIdList rids
            ]
hprintAnResult ( DefinedMultiplyRates rids )       =
    unwords [ "The following rate identifers are defined more than once"
            , hprintProcessIdList rids
            ]

hprintAnResult ( AliasLoop pids )                      =
    unwords [ "The definitions of the following process identifiers"
            , "form an alias loop"
            , hprintProcessIdList pids
            ]
hprintAnResult ( SystemSelfLoop par acts )             =
    unwords [ "The main system equation :"
            , Print.hprintComponent par
            , "may perform a self loop over the actions"
            , Print.hprintCooperationSet $ ActionSet acts
            ]
hprintAnResult (TypingError s)                         =
    s



hprintProcessIdList :: [ ParsedComponentId ] -> String
hprintProcessIdList = Utils.mkCSlist . (map Print.hprintComponentName)


{-| Returns all the warnings and errors, that is all the
    'AnalysisMessage's from an 'AnalysisReport'.
    This is useful for applications which simply wish to ask
    the question "Does the model pass static analysis?"
    If the list returned from this function is @[]@
-}
messagesOfReport :: AnalysisReport -> [ AnalysisMessage ]
messagesOfReport aReport =
    concat [ excessActionsReport aReport
           , definedNamesReport  aReport
           , aliasLoops          aReport
           , selfLoopsReport     aReport
           , rateNamesReport     aReport
           , typingErrors        aReport
           ]

{-| Splits a list of messages into two lists of 'AnalysisResult's,
    the first of a list of errors the second a list of warnings.
-}
splitMessages :: [ AnalysisMessage ] -> ( [ AnalysisResult ]
                                        , [ AnalysisResult ] )
splitMessages []                         = ( [], [] )
splitMessages (AnalysisError m : rest)   =
    first (m : ) $ splitMessages rest
splitMessages (AnalysisWarning m : rest) =
    second (m : ) $ splitMessages rest


data AnalysisFlag = UnusedAliasesAllowed
                    deriving Eq

{-| Performs all of the analyses on the model and returns an
    'AnalysisReport'
    This *should* take in the analysisFlags so that we can set them
    according to our wishes.    
-}
analyseModel :: ParsedModel -> AnalysisReport
analyseModel model =
    AnalysisReport 
     { processArraysPresent = containsProcessArrays model 
     , processActionsMap    = processActionsMapping
     , compositionActions   = compActions
     , excessActionsReport  = excessActReports
     , definedProcessNames  = processIdentsDefined
     , usedProcessNames     = processNamesUsed
     , definedNamesReport   = analyseDefinedNames analysisFlags
                                                  processIdentsDefined 
                                                  processNamesUsed
                                                  aliasMap
     , aliasDataBase        = aliasMap
     , aliasLoops           = maybeToList $ checkForAliasLoops aliasMap
     , selfLoopsReport      = maybeToList $ selfLoopsModel aliasMap model
     , usedActionNames      = nub $ map nameOfAction usedActions
     , definedRateNames     = rateNamesDefined
     , usedRateNames        = rateNamesUsed
     , rateNamesReport      = analyseDefinedRates rateNamesDefined 
                                  rateNamesUsed
     , typingErrors         = typingResults
     }
    where
    usedActions           = concat $ Map.elems processActionsMapping
    processActionsMapping = toMap processActionDb
    processActionDb       = processActions model
    compActions           = fst excessActResults
    excessActReports      = snd excessActResults
    excessActResults      = excessActions model processActionDb
    processIdentsDefined  = definedProcesses model
    processNamesUsed      = Set.toList $ usedProcesses model
    rateSpecifications    = ratesDefinedModel model
    aliasMap              = buildAliasDb model
    rateNamesUsed         = ratesUsedModel model
    rateNamesDefined      = map fst rateSpecifications
    typingResults         = map (AnalysisError . TypingError) $ typeModel model
    analysisFlags         = [ UnusedAliasesAllowed ]

{-|
  A very simple function to determine whether or not a 'ParsedModel'
  contains an array of processes construct. This is mostly useful for
  the @ipci@ program to decide whether or not to ask the user if
  we should aggregate or not, if there are no array of processes then
  it doesn't make any difference and we therefore do not need to ask.
  Also it could be used by both @ipc@ and @ipci@ to give a warning
  if @--aggregate@ (or indeed the @--noaggregate @)
  flag is given on the command line but no array of processes are used.
-}
containsProcessArrays :: ParsedModel -> Bool
containsProcessArrays model =
    containsComp pComp || any containsDef pDefs
    where
    pComp  = modelSystemEqn model
    pDefs  = modelProcessDefs model
    containsComp :: ParsedComponent -> Bool
    containsComp (ProcessArray _ _ _)       = True
    containsComp (StopProcess)              = False
    containsComp (IdProcess _)              = False
    containsComp (Hiding par _)             = containsComp par
    containsComp (Cooperation left _ right) = containsComp left ||
                                              containsComp right
    containsComp (ComponentSum left right)  = containsComp left ||
                                              containsComp right
    containsComp (PrefixComponent _ right)  = containsComp right
    containsComp (CondBehaviour _ right)    = containsComp right
    
    containsDef :: ProcessDef -> Bool
    containsDef = containsComp . snd

{-
  This analysis determines which actions each of the processes within
  a model may perform. Using the results of such an analysis we can,
  for example, determine whether a cooperation synchronises on a spurious
  action, for example
  @
  P = (a, 1.0).P
  Q = (a, 1.0).Q
  ( P <b> Q )
  @
  since neither process can perform the action @b@ the user should be
  warned about this.
-}
type ProcessActionDb = FixMap ParsedComponentId [ ParsedAction ]

{-
  To avoid testing for @Nothing@ we assume that if we do not
  find the component in the database it is merely because we have
  not yet added and we will do so in a future iteration.
  Besides which this analysis does not deal with undefined process names
  that is for another analysis to worry about.
-}
lookupCptActions :: ParsedComponentId -> ProcessActionDb -> [ ParsedAction ]
lookupCptActions name = (fromMaybe []) . (lookup name)


{-
  The strategy is to begin with an identity action database such
  that each process is mapped to the empty set of actions.
  Then we walk through the model adding to the data base entry
  each action that it may do, plus all the actions that may be done
  by any of the processes that the current one may become.
  This gives us a new process action database, we keep doing this
  until the process action database does not change.
-}
processActions :: ParsedModel -> ProcessActionDb
processActions model =
    fixPoint processAllDefs empty
    where
    pDefs = modelProcessDefs model
    -- Notice here we always apply it to the same def list
    -- the model does not change, only the data base.
    processAllDefs :: ProcessActionDb -> ProcessActionDb
    processAllDefs db = foldl processDef db pDefs

    processDef :: ProcessActionDb -> ProcessDef -> ProcessActionDb
    processDef db (ident, proc) =
        insertList ident actions db
        where actions = processProc db proc

    processProc :: ProcessActionDb -> ParsedComponent -> [ ParsedAction ]
    processProc _db (StopProcess)                        =
       []
    processProc db (IdProcess name)                      = 
        lookupCptActions name db
    processProc db (PrefixComponent trans nextCpt)
      -- If it is a tau action then we do not include
      -- it. Note this is kind of important for the fixmap
      -- since `act /= `act, and so the fix map will have a
      -- tau action as an element but we will just continually
      -- add that to the fixmap because we can never equal it.
      | isTauAction action1      = nextActions
      | elem action1 nextActions = nextActions
      | otherwise                = action1 : nextActions
        where 
        nextActions = processProc db nextCpt
        action1     = pepaTransAction trans
    processProc db (CondBehaviour _ nextCpt) =
        processProc db nextCpt
    processProc db (ComponentSum left right)             =
        nub $ leftActions ++ rightActions
        where leftActions  = processProc db left
              rightActions = processProc db right
    processProc db (Cooperation left _ right)            =
        nub $ leftActions ++ rightActions
        where leftActions  = processProc db left
              rightActions = processProc db right
    processProc db (ProcessArray comp _ _)               =
        processProc db comp
    processProc db (Hiding p _acts)                      =
        processProc db p


{-
  The analysis done by 'processActions' allowed us to determine for each
  process the set of actions that it may eventually perform.
  This now tests any cooperation for a superflous action within
  the cooperation.

  Since here we are computing the actions done by the main cooperation
  we also return these, since this can be used to determine for example
  if the probe makes sense.
-}
excessActions :: ParsedModel -> ProcessActionDb -> ( [ ParsedAction ]
                                                   , [ AnalysisMessage ] )
excessActions model database =
    checkComp $ modelSystemEqn model
    where
    excessError :: ParsedComponent -> [ ParsedAction ] 
                -> [ ParsedAction ] -> AnalysisMessage
    excessError p acts1 = 
        AnalysisError . ExcessActions p acts1

    needlessHideError :: ParsedComponent -> [ ParsedAction ]
                      -> [ ParsedAction ] -> AnalysisMessage
    needlessHideError p acts1= 
        AnalysisError . NeedlessActionsHide p acts1

    excessErrorLeft :: ParsedComponent -> [ ParsedAction ] 
                   -> [ ParsedAction ] -> AnalysisMessage
    excessErrorLeft p acts1 =
        AnalysisError . ExcessActionsLeft p acts1

    excessErrorRight :: ParsedComponent -> [ ParsedAction ]
                     -> [ ParsedAction ] -> AnalysisMessage
    excessErrorRight p acts1 = 
        AnalysisError . ExcessActionsRight p acts1
    {-
      'checkComp' must do two things, it must return errors for
      any cooperation within it which has excess actions (which
      are either cooperated over or needlessly hidden). It must
      also return the actions which the input component can do
      such that the checking of enclosing pars can be done.
    -}
    checkComp :: ParsedComponent -> ( [ ParsedAction ],  [ AnalysisMessage ] )
    checkComp (StopProcess)                        =
      ([], [])
    checkComp (IdProcess ident)                    = 
        ( lookupCptActions ident database, [] )
    checkComp (PrefixComponent trans p)
        | elem act actions = (actions, messages)
        | otherwise        = (act : actions, messages)
        where 
        (actions, messages) = checkComp p
        act                 = pepaTransAction trans
    checkComp (CondBehaviour _ cpt)                =
      checkComp cpt
    checkComp (ComponentSum left right)            = 
        (actions, messages)
        where
        messages = messagesLeft ++ messagesRight
        actions  = nub $ actionsLeft ++ actionsRight
        (actionsLeft, messagesLeft)   = checkComp left
        (actionsRight, messagesRight) = checkComp right
    checkComp (ProcessArray comp _ Nothing)        =
        checkComp comp
    checkComp par@(ProcessArray comp _ (Just pActions))
        | notDoneBy == [] = compResults
        | otherwise       = ( compActions, errorRpt : compMessages)
        where
        compResults  = checkComp comp
        compActions  = fst compResults
        compMessages = snd compResults
        notDoneBy    = pActions \\ compActions
        errorRpt     = excessError par pActions notDoneBy
    checkComp par@(Hiding left actions)
        | notDoneBy == [] = (leftActions, leftErrors)
        | otherwise       = (leftActions, errorRpt : leftErrors)
        where
        ( leftActions,
          leftErrors ) = checkComp left
        notDoneBy      = actions \\ leftActions
        errorRpt       = needlessHideError par actions notDoneBy
    checkComp (Cooperation left WildCard right)               =
        -- We cannot have any spurious actions in the wildcard
        -- set however, we *should* check that the two do at least
        -- contain some activities in common
        ( leftActions ++ rightActions
        , concat [ leftErrors
                 , rightErrors
                 ]
        )
        where
        ( leftActions,
          leftErrors )  = checkComp left
        ( rightActions,
          rightErrors ) = checkComp right 
    checkComp proc@(Cooperation left (ActionSet actions) right) = 
        ( leftActions ++ rightActions
        , concat [ leftError
                 , rightError
                 , spuriousError
                 , leftErrors
                 , rightErrors
                 ]
        )
        where
        ( leftActions,
          leftErrors )  = checkComp left
        ( rightActions,
          rightErrors ) = checkComp right

        notDoneByLeft   = actions \\ leftActions
        notDoneByRight  = actions \\ rightActions
        notDoneByEither = intersect notDoneByLeft notDoneByRight
        leftButNotRight = notDoneByRight \\ notDoneByLeft
        rightButNotLeft = notDoneByLeft \\ notDoneByRight
    
        leftError 
            | leftButNotRight == [] = []
            | otherwise             = 
                [ excessErrorRight proc actions leftButNotRight ]
        rightError 
            | rightButNotLeft == [] = []
            | otherwise             = 
                [ excessErrorLeft proc actions rightButNotLeft ]
        spuriousError
            | notDoneByEither == [] = []
            | otherwise             =
                [ excessError proc actions notDoneByEither ]


{-
  Note that this deliberately returns more than one instance of a name
  if it is defined more than once.
-}
definedProcesses :: ParsedModel -> [ ParsedComponentId ]
definedProcesses = (map fst) . Pepa.processDefsOfModel

{-
  This function returns the list of processes that can be reached from
  the main composition, whether or not those processes are defined.
  From this information we can easily define
  'unusedProcesses' which returns any processes that are defined but not
  used, and 'undefinedProcesses' which are used but not defined.
-}
type Seen = Set ParsedComponentId
usedProcesses :: ParsedModel -> Set ParsedComponentId
usedProcesses model =
    searchComp Set.empty pComp
    where
    pDefs = modelProcessDefs model
    pComp = modelSystemEqn   model
    {-
      There are two main auxiliary functions for this
      'searchComp' for components and
      'searchId' for looking up an identifier.
      Both of these must take in and maintain a list of "seen" identifiers
      such that we don't end up in an infinite loop.
    -}
    searchComp :: Seen -> ParsedComponent -> Set ParsedComponentId
    searchComp seen (IdProcess ident) 
        | Set.member ident seen                 = seen
        | otherwise                             = 
          searchId (Set.insert ident seen) ident pDefs
    searchComp seen (StopProcess)               = seen
    searchComp seen (PrefixComponent _ nextCpt) =
        searchComp seen nextCpt
    searchComp seen (ComponentSum left right)   =
        searchComp seenLeft right
        where seenLeft = searchComp seen left
    searchComp seen (CondBehaviour _ next)      =
        searchComp seen next
    searchComp seen (ProcessArray comp _ _)     =
        searchComp seen comp
    searchComp seen (Hiding left _)            =
        searchComp seen left
    searchComp seen (Cooperation left _ right) =
        searchComp seenLeft right
        where seenLeft = searchComp seen left
              

    {-
      Note here that if we do not find the given identifier we do not
      raise an error, this is for another kind of analysis which can use
      the results of this to find processes that are used but not defined.
    -}
    searchId :: Seen -> ParsedComponentId -> [ ProcessDef ]
             -> Set ParsedComponentId 
    searchId seen _      []                     = seen
    searchId seen ident ((ident2, comp) : rest)
        | ident == ident2 = searchComp seen comp
        | otherwise       = searchId seen ident rest


{-
  This analyses the 
  The next section checks for an unusued or undefined process definition.
  To do this we write two auxiliary functions, one returns the list
  of process names which are defined and the other the list of process
  definitions which are used. The first is very simple but the second must
  do a deep search, that is it is not enough to see which names are
  mentioned in the main composition but follow the definition of those
  names. These two 
  As a further analysis we check for process names that are defined
  more than once.
-}

analyseDefinedNames :: [ AnalysisFlag ]
                    -> [ ParsedComponentId ] 
                    -> [ ParsedComponentId ]
                    -> AliasMap
                    -> [ AnalysisMessage ]
analyseDefinedNames analysisFlags defNames usedNames aliasMap =
    concat [ unusedProcesses
           , undefinedProcesses
           , multipleDefinedProcesses
           ]
    where
    unusedProcesses
        | definedButNotUsed == [] = []
        | otherwise               = 
          [ AnalysisWarning $ DefinedButNotUsedProcesses definedButNotUsed ]

    undefinedProcesses 
        | usedButNotDefined == [] = []
        | otherwise               = 
            [ AnalysisError $ UsedButUndefinedProcesses usedButNotDefined ]

    multipleDefinedProcesses
        | definedMultiply == [] = []
        | otherwise             =
            [ AnalysisError $ DefinedMultiplyProcesses definedMultiply ]

    nDefNames         = nub defNames
    usedButNotDefined = usedNames \\ nDefNames 
    definedButNotUsed
      | elem UnusedAliasesAllowed analysisFlags =
        filter isNotAlias (nDefNames \\ usedNames)
      | otherwise                               = 
        (nDefNames \\ usedNames)
    definedMultiply   = defNames  \\ nDefNames

    isNotAlias p = isNothing $ FixMap.lookup p aliasMap

{-
  Very similar to 'analyseDefinedNames' but for rate names defined.
-}
analyseDefinedRates :: [ RateIdentifier ] -> [ RateIdentifier ]
                    -> [ AnalysisMessage ]
analyseDefinedRates defNames usedNames =
    concat [ unusedRates
           , undefinedRates
           , multipleDefinedRates
           ]
    where
    unusedRates
        | definedButNotUsed == [] = []
        | otherwise               = 
          [ AnalysisWarning $ DefinedButNotUsedRates definedButNotUsed ]

    undefinedRates
        | usedButNotDefined == [] = []
        | otherwise               = 
            [ AnalysisError $ UsedButUndefinedRates usedButNotDefined ]

    multipleDefinedRates
        | definedMultiply == [] = []
        | otherwise             =
            [ AnalysisError $ DefinedMultiplyRates definedMultiply ]

    nDefNames         = nub defNames
    usedButNotDefined = usedNames \\ nDefNames 
    definedButNotUsed = nDefNames \\ usedNames
    definedMultiply   = defNames  \\ nDefNames


{-|
  The alias database is a bit of a misnomer actually.
  It does not build up aliases that only consist of ones such as
  @ P = S @
  but also ones that consist of
  @
    P = (a, r).S ;
    S = P + ... ;
  @
  Because for the purposes of this analysis it is indeed what
  we want. Basically we want a data base mapping processes to
  those that they can become without doing any actions.
  So in the above, @S can become a P@ without doing any
  actions.
-}
type AliasMap = FixMap ParsedComponentId [ ParsedComponentId ]

{- If the process is not in the alias data base then we assume that it has
   no aliases (at least not yet, we may be currently
   building up the alias database)
-}
findProcessAlias :: AliasMap -> ParsedComponentId -> [ ParsedComponentId ]
findProcessAlias aliasDb name = fromMaybe [] $ lookup name aliasDb


buildAliasDb :: ParsedModel -> AliasMap
buildAliasDb model =
    fixPoint processAllDefs empty
    where
    defs = modelProcessDefs model
    processAllDefs :: AliasMap -> AliasMap
    processAllDefs am = foldl processDef am defs

    addAlias :: ParsedComponentId -> ParsedComponentId -> AliasMap -> AliasMap
    addAlias name1 name2 am = 
        insertList name1 newAliases am
        where
        name2Aliases = findProcessAlias am name2
        newAliases = if elem name2 name2Aliases
                     then name2Aliases
                     else name2 : name2Aliases

    processDef :: AliasMap -> ProcessSpec -> AliasMap
    processDef am (_    , StopProcess)            = am
    processDef am (_    , PrefixComponent _ _)    = am
    processDef am (name1, IdProcess name2)        =
        addAlias name1 name2 am
    processDef am (name1, ComponentSum cpt1 cpt2) =
        processDef am1 (name1, cpt2)
        where am1 = processDef am (name1, cpt1)
    processDef am (name1, CondBehaviour _ cpt1)   =
       processDef am (name1, cpt1)
    processDef am (_name1, Cooperation _ _ _)     = am
    processDef am (_name1, ProcessArray _ _ _)    = am
    processDef am (name1, Hiding p _)             =
        processDef am (name1, p)


{-
  An alias data base can be used to check for alias loops.
  This means where we have a set of processes that form a cycle
  and hence are undefined. The simplest example of this is.
  @ P = P @
  But we could also have
  @ P = S
    S = R
    R = P
  @
  Note that not all loops detected in this way will necessarily 
  be impossible to compile for example:
  @ P = S + R
    R = (a, r).R1
    S = P
  @
  however such a definition is effectively the same as simply:
  @ P = (a, r).R1
    S = P
    R = P
  @
  so we can reject it anyway since the model is defined in a strange
  way, and is quite possibly and error.
  
  todo: this only finds the first one and should in fact find all of them.
  The tricky part is that suppose we have
  @ P = S
    S = P
  @
  then the naive solution would report this loop twice, since it will be
  in the alias map twice, once under @P@ and once under @S@.
-}
checkForAliasLoops :: AliasMap -> Maybe AnalysisMessage
checkForAliasLoops aliasDb =
    do (ident, aliases) <- find isAliasLoop $ toList aliasDb
       return $ AnalysisError $ AliasLoop (ident : aliases)
    where
    isAliasLoop :: ( ParsedComponentId, [ ParsedComponentId ] ) -> Bool
    isAliasLoop (ident, aliases) = elem ident aliases



type ProcessActionMap = Map ParsedComponentId [ ParsedAction ]
{-| Given a process action map, which maps process names to actions
    which the given process or a deriviative of the given process
    may perform, and given a component we return the actions which
    the component may perform.
    This should only be called on parallel components.
-}
componentPerformsActions :: ProcessActionMap 
                         -> ParsedComponent
                         -> Set ParsedAction
componentPerformsActions mapping component =
  case component of
    (StopProcess)                      -> Set.empty
    (IdProcess ident)                  -> lookupActions ident
    (Cooperation left _coopSet right)  ->
      Set.union (componentPerformsActions mapping left)
                (componentPerformsActions mapping right)
    (ProcessArray comp _size _coopSet) ->
      componentPerformsActions mapping comp
    (Hiding comp hideSet)              ->
      Set.difference (componentPerformsActions mapping comp)
                     (Set.fromList hideSet)
    (PrefixComponent _trans _comp) ->
      error sequentialError
    (ComponentSum _left _right)    ->
      error sequentialError
    (CondBehaviour _cond _comp)    -> 
      error sequentialError
  where
  lookupActions :: ParsedComponentId -> Set ParsedAction
  lookupActions ident = maybe Set.empty Set.fromList $ Map.lookup ident mapping
  sequentialError = "componentPerformsAction: called on a sequential component"

{-
  This section defines the functions required for detecting self-loops
  within the model.
  [@todo@] Write a bit more documentation, I've a couple of pages of
           notes in my notebook
  We maintain a dictionary mapping process identifiers to the actions
  on which they can do a self loop.

  Now we would like to detect a self loop by looking at a
  process definition and seeing:
  @
  P = (a, r) . P ;
  @
  Here we can see that the result of the prefix is the same as
  the name used in the definition.
  But this doesn't work because of aliases, for example:
  @
  P = (a, r) . S ;
  S = P ;
  @
  We would not detect that this is a simple loop because in
  the prefix the result is @S@ and not @P@. Even without
  a simple alias we can get the following:
  @
  P = (a, r) . S ;
  S = P + (a, r) . S1 ;
  @
  Here, the fact that @P@ is even a part of the definition of
  @S@ means that we end up with a self-loop. All of this
  goes to show that we must first build up a list of processes that
  each process may become without doing *any* action. So in the example
  above, @S@ can become @P@ without doing any actions.

  We have to store two kinds of actions, those on which the process
  may perform a self-loop, and those on which the process may perform
  a self-loop and which are hidden. The ones which are hidden cannot
  be cooperated over and hence will always be a self-loop.
-}
type ProcessSpec   = ( ParsedComponentId, ParsedComponent )
-- type LoopActions   = ( [ ParsedAction ], [ ParsedAction ] )
-- type ProcessLoopDb = FixMap ParsedComponentId LoopActions


{-
  To avoid testing for 'Nothing' we assume that if we do not
  find the component in the database it is merely because we have
  not yet added and we will do so in a future iteration.
-}
-- findProcessLoopActs :: ParsedComponentId -> ProcessLoopDb -> LoopActions
-- findProcessLoopActs name = (fromMaybe ([], [])) . (lookup name)


{-
    For now self-loops are not analysed. The change to allow parallel component
    definitions has invalidated the checking a little bit, however it should be
    fixable, it's just not the most important thing to do right now.

    Basically the problem boils down to:
    @
    P = (a, r).P; // self-loop
    Q = P \ { a } ; // still a self-loop
    R = < some process which does not self loop on 'a' >
    Q < a > R
    @
    I would say that this still does a self-loop but it will not be detected.


    Perhaps worse we seem to be detecting self-loops where there are none.
-}    
selfLoopsModel :: AliasMap -- -> ProcessActionDb 
               -> ParsedModel -> Maybe AnalysisMessage
selfLoopsModel _aliasDb _model = Nothing
   -- TODO: this has currently been disabled because it is
   -- a little inconvenient to do this when the wildcard is
   -- enabled. In addition I am thinking of revamping the
   -- analysis because I think there are several things that
   -- could be done a little better and perhaps we need to
   -- merge some code with PepaUtils.
   -- In the mean-time at least self-loops are detected
   -- dynamically by the state space generator.




{-
  Finding all the rate names defined within a model is very simple we simply
  walk through the definitions in the 'ParsedModel'.
  This returns the full spec, the names of those defined within the model
  can be gotten from simply mapping 'fst' over the list of rate specifications
  returned.
  Okay now that we have updated the 'ParsedModel' data type this function is
  even simpler, yay.
-}
ratesDefinedModel :: ParsedModel -> [ RateSpec ]
ratesDefinedModel = Pepa.rateDefsOfModel

{-
    Finds all the rates used in a model. This is a little simplistic
    since it considers a use in a rate definition to still be a use.
    It shouldn't really.
    Consider
    @ r = 1.0 ;
      s = r + 1.0 ;
      < model that doesn't use s or r >
    @
    In this case @r@ will not be reported as not being used. But it
    doesn't actually get used in the model since it is only used
    in the definition of @s@ which itself isn't used. It would be
    simple to augment this. For now it isn't a big deal since the user
    will at least get one warning since @s@ isn't used.

    In fact it is worse than that, we should only actually consider
    the process definitions that are actually used.
  
    We also don't consider the main system, since we shouldn't really
    allow prefixes in that.
-}
ratesUsedModel :: ParsedModel -> [ RateIdentifier ]
ratesUsedModel model =
   -- When asking the rates used of a rate expression
   -- (or an expression used in a process concentration or
   -- a conditional behaviour) we return all the names used
   -- this will include process names etc so we must subtract
   -- from that set any process names which are defined.
   Set.toList $ Set.difference allUsed processNames   
   where
   rDefs          = modelRateSpecs   model
   pDefs          = modelProcessDefs model
   mainComp       = modelSystemEqn   model
   allUsed        = Set.unions [ ratesRates, ratesProcesses, ratesMainComp ]
   ratesRates     = Set.fromList $ concatMap ratesOfRateSpec rDefs
   ratesProcesses = ratesUsedInProcessDefs pDefs
   ratesMainComp  = ratesUsedInComponent mainComp
   

   ratesOfRateSpec :: RateSpec -> [ RateIdentifier ]
   ratesOfRateSpec = namesOfRateExpr . snd



   processNames = Set.fromList $ map fst pDefs


{- Care should be taken here, we return all names used within rate expressions
   and conditions, hence the set returned here may contain process names
   as well as rate names.
-}
ratesUsedInProcessDefs :: [ ProcessDef ] -> Set RateIdentifier
ratesUsedInProcessDefs = Set.unions . (map (ratesUsedInComponent . snd))

{- Care should be taken here, we return all names used within rate expressions
   and conditions, hence the set returned here may contain process names
   as well as rate names.
-}
ratesUsedInComponent :: ParsedComponent -> Set RateIdentifier
ratesUsedInComponent (IdProcess _)                  = Set.empty
ratesUsedInComponent (StopProcess)                  = Set.empty
ratesUsedInComponent (PrefixComponent trans nextP)  =
  Set.unions [ Set.fromList $ ratesOfRate $ pepaTransRate trans
             , Set.fromList $ concatMap namesOfRateExpr $ 
                              pepaTransConditions trans
             , ratesUsedInComponent nextP
             ]
  where
  ratesOfRate :: ParsedRate -> [ RateIdentifier ]
  ratesOfRate (RateTop _)        = []
  ratesOfRate (RateImmediate _ ) = []
  ratesOfRate (RateTimed e)      = namesOfRateExpr e  
ratesUsedInComponent (ComponentSum left right)      =
  Set.union (ratesUsedInComponent left)
            (ratesUsedInComponent right)
ratesUsedInComponent (CondBehaviour cond next)      =
  Set.union (Set.fromList $ namesOfRateExpr cond)
            (ratesUsedInComponent next)
ratesUsedInComponent (Cooperation left _ right)     =
  Set.union (ratesUsedInComponent left)
            (ratesUsedInComponent right)
ratesUsedInComponent (ProcessArray comp size _)     = 
  Set.union (ratesUsedInComponent comp)
            (Set.fromList $ namesOfRateExpr size)
ratesUsedInComponent (Hiding comp _actions)         =
  ratesUsedInComponent comp

-------------------------------------------------------------------------
-- Individual analysis functions
-------------------------------------------------------------------------

-- The type of a transition from one state to another.
-- type Transition = (ParsedComponentId, ParsedComponentId)

{-|
   Returns the possible transitions from a given starting component in
   the given model. The model must have been reduced to the state where
   all prefix components have, as their right hand side a component
   identifier, and not an arbitrary form of sequential component.
-}
possibleSuccessors :: ParsedComponentId -> ParsedModel -> [ ParsedComponentId ]
possibleSuccessors startIdent model =
   possibleSuccessorsInModel [] startIdent
   where
   -- The first argument is the 'seen' list, ie the list of process identifiers
   -- which we have already processed and hence do not need to look up
   -- again. Note that this could quite easily be made tail recursive if
   -- we doubled up the successors with the tail-recursive-argument.
   possibleSuccessorsInModel :: [ ParsedComponentId ] -> ParsedComponentId
                             -> [ ParsedComponentId ]
   possibleSuccessorsInModel seen ident
      | elem ident seen = []
      | otherwise       =
         case findProcessDefinition model ident of
            Nothing   -> error notFound
            Just comp -> possibleComp comp
      where
      newSeen = ident : seen
      possibleComp :: ParsedComponent -> [ ParsedComponentId ]
      possibleComp (StopProcess)                         =
         []
      possibleComp (IdProcess ident2)                    =
         possibleSuccessorsInModel newSeen ident2
      possibleComp (ComponentSum left right)             =
         union (possibleComp left) (possibleComp right)
      possibleComp (PrefixComponent _ (IdProcess right)) =
         union [ right ] $ possibleSuccessorsInModel newSeen right
      possibleComp (CondBehaviour _ p)                   =
         possibleComp p
      
      {- The errors cases -}
      possibleComp (PrefixComponent _ _)                 =
         error notSimplified
      possibleComp (Cooperation _ _ _ )                  =
         error errorMessage
      possibleComp (ProcessArray _ _ _ )                 =
         error errorMessage
      possibleComp (Hiding _ _ )                         =
         error errorMessage
   
   errorMessage  = "Attempt to obtain the sequentional transitions " ++
                   "of a parallel component"
   notFound      = "component not found when attempting to gather next " ++
                   "transitions"
   notSimplified = "The model has not first been simplfied before calling " ++
                   "possibleTransitions"


{-|
   Returns the list of actions which it is possible for the given
   component to perform. This does NOT include those done by alias
   components. So for example if the component given is
   @ (a, \infty).P + R @
   the returned list is simply the singleton list containing @a@,
   regardless of what @R@ may be able to do. This is apparent
   from the type, since we do not take in a model or list of definitions
   there is no way for this function to know what @R@ may perform.

   Parallel components are ignored completely.
-}
possibleActionsNonAliased :: ParsedComponent -> [ ParsedAction ]
possibleActionsNonAliased (IdProcess _)              = []
possibleActionsNonAliased (StopProcess)              = []
possibleActionsNonAliased (PrefixComponent trans _)  = 
   [ pepaTransAction trans ]
possibleActionsNonAliased (ComponentSum left right)  =
   union (possibleActionsNonAliased left  )
         (possibleActionsNonAliased right )
possibleActionsNonAliased (CondBehaviour _ p)        =
   possibleActionsNonAliased p
possibleActionsNonAliased (Cooperation _ _ _ )       =
   error "possibleActionsNonAliased: parallel component found"
possibleActionsNonAliased (ProcessArray _ _ _ )      =
   error "possibleActionsNonAliased: parallel component found"
possibleActionsNonAliased (Hiding _ _ )              =
   error "possibleActionsNonAliased: parallel component found"
   

{-| The result of a call to 'getInitialConcentrations' -}
type ConcentrationMap = Map ParsedComponentId Int

{-|
   'getInitialConcentrations' maps a pepa model to an initial concentration
   list of states (here the states are not full states but just the states
   of individual components.

   The model must be in Normal form, that is NO parallel definitions,
   and prefix components must only have an 'IdProcess' as their next
   component. All components are used exactly once.
-}
getInitialConcentrations :: Rates.RateNumberClass a => ParsedModel -> Map ParsedComponentId a
getInitialConcentrations pModel =
   foldl makeVirtual concrete $ Pepa.modelVirtualComps pModel
   where
   concrete = getParallel $ modelSystemEqn pModel
   -- makeVirtual :: ConcentrationMap -> Pepa.VirtualSpec -> ConcentrationMap
   makeVirtual concMap (name, expr) = 
     Map.insert name (Rates.reduceRateExpr concMap expr) concMap
   
   -- getParallel :: ParsedComponent -> ConcentrationMap
   getParallel (IdProcess ident)                  = 
      Map.insert ident 1 $ 
      getSequentialId (Set.singleton ident) ident
   getParallel (Cooperation left _ right)         =
      Map.union (getParallel left) (getParallel right)
   getParallel (Hiding comp _)                    =
      getParallel comp
   getParallel (ProcessArray comp size _actions)  =
      Map.map updateConc $ getParallel comp
      where
      -- updateConc :: Int -> Int
      updateConc = (newSize *)
      newSize    = Rates.reduceRateExpr Map.empty size
      {-
        case size of
          Cconstant x -> x
          Creal x     -> round x
          _           -> error "Non-trivial process array concentration" -}

   {- Sequential components in the main system equation-}
   getParallel (StopProcess)                      =
      error "Sequential component in the main system equation"
   getParallel (CondBehaviour _ _)                =
      error "Sequential component in the main system equation"
   getParallel (PrefixComponent _ _)              =
      error "Sequential component in the main system equation"
   getParallel (ComponentSum _ _)                 =
      error "Sequential component in the main system equation"

   -- getSequentialId :: Seen -> ParsedComponentId -> ConcentrationMap
   getSequentialId seen ident =
     case findProcessDefinition pModel ident of
       Nothing  -> error "Undefined process in getInitialConcentrations"
       Just p   -> getSequential seen p

   -- getSequential :: Seen -> ParsedComponent -> ConcentrationMap
   getSequential _seen (StopProcess)            = Map.empty
   getSequential seen (IdProcess ident)
      | Set.member ident seen                   = Map.empty
      | otherwise                               = 
        Map.insert ident 0 $ getSequentialId (Set.insert ident seen) ident
   getSequential seen (PrefixComponent _ next)  =
      getSequential seen next
   getSequential seen (CondBehaviour _ next)    =
      getSequential seen next
   {-
      component sums are a little different; Suppose we have
      P = Q + R
      Q = (a, r) . S
      R = (a, r) . S
      Then we do not wish to add Q and R since a process will
      never have this as true (the process will either be in
      state P or S, but cannot be in state Q).
      Note however that we do not add Q and R to the 'seen' list
      because it is possible that for exmaple we have
      P = Q + R
      Q = (a, r) . Q
      in which case a process MAY be in state Q.
   -}
   getSequential seen (ComponentSum left right) =
      Map.union leftConc rightConc
      where
      leftConc  = case left of
                    IdProcess l -> getSequentialId seen l
                    _           -> getSequential seen left
      rightConc = case right of
                    IdProcess r -> getSequentialId seen r
                    _           -> getSequential seen right
   getSequential _seen (Cooperation _ _ _)      =
      error $ "Parallel component in sequential definition "
              ++ "detected in getInitialConcentrations"
   getSequential _seen (ProcessArray _ _ _)     =
      error $ "Parallel component in sequential definition "
              ++ "detected in getInitialConcentrations"
   getSequential _seen (Hiding _ _)             =
      error $ "Parallel component in sequential definition "
              ++ "detected in getInitialConcentrations"

   
-- Utility to find the process definition which defines the given
-- identifier.
-- findProcessDef :: [ ProcessDef ] -> ParsedComponentId -> ProcessDef
-- findProcessDef pDefs ident =
--  find ((ident ==) . fst) pDefs

{-



{-
  \subsection{Rate Expression Reduction}
  This section involves the reduction of rate expressions.

  The reduction of a parsed rate expression given a dictionary mapping
  rate identifiers to values. 

  @todo{We do not check for recursive definitions, obviously we should}
-}
type RateMapping = FixMap RateIdentifier ParsedRateExp

reduceRateSpecs :: [ RateSpec ] -> [ RateSpec ]
reduceRateSpecs specs = 
    toList $ fixPoint reduceAllSpecs empty
    where
    reduceAllSpecs :: RateMapping -> RateMapping
    reduceAllSpecs rm = foldl reduceRateSpec rm specs

    reduceRateSpec :: RateMapping -> RateSpec -> RateMapping
    reduceRateSpec mapping (rateName, rateExp) =
	insert rateName reducedExp mapping
	where reducedExp = reduceRateExpr mapping rateExp

    reduceRateExpr :: RateMapping -> ParsedRateExp -> ParsedRateExp
    reduceRateExpr _ RateTop                             = RateTop
    reduceRateExpr _ rate@(RateNum _)                    = rate
    reduceRateExpr _ rate@(RateProc _)                   = rate
    -- Note here that we do not reduce the original expression
    -- because of course the whole point of that is that it contains
    -- the original expression in order to show the user.
    reduceRateExpr mapping rate@(RateId ident _)         =
	case lookup ident mapping of
	Nothing   -> rate
	Just rExp -> reduceRateExpr mapping rExp
    reduceRateExpr mapping (RateBexp oper lexp rexp)     =
	case (rLexp, rRexp) of
	(RateNum v1, RateNum v2) -> applyOper v1 v2
	_                        -> RateBexp oper rLexp rRexp
	where
	rLexp = reduceRateExpr mapping lexp
	rRexp = reduceRateExpr mapping rexp

        applyOper :: ParsedValue -> ParsedValue -> ParsedRateExp
	applyOper leftValue rightValue =
	    case oper of
	    "+" -> RateNum $ leftValue + rightValue
	    "-" -> RateNum $ leftValue - rightValue
	    "*" -> RateNum $ leftValue * rightValue
	    "/" -> RateNum $ leftValue / rightValue
	    _   -> error $ "Undefined rate expression operator: " ++ oper
    -- For now we just assume that the condition can never be reduce
    reduceRateExpr mapping (RateCond cond lexp rexp)     =
	RateCond cond rLexp rRexp
	where
	rLexp = reduceRateExpr mapping lexp
	rRexp = reduceRateExpr mapping rexp
-}
