{-|
    This module is a collection of utility functions performed over the
    syntax of Pepa models.
-}
module Language.Pepa.PepaUtils
   ( TransitionAnalysis      ( .. )
   , modelTransitionAnalysis
   , isPepaActionEnabled
   , TransSuccessor
   , possibleTransSuccessors
   , possibleTransitions
   , allTransitionsOfModel
   
   , SuccessorTrans
   , SuccessorTransMap
   , allSuccessors
   , CanonicalSuccessor
   , CanonicalSuccessors
   , allSuccessorsCanonical

   , derivativesOfComponent
   , reachableProcessDefs
   , transitiveDerivatives
   
   , findProcessDefinition
   
   , makePepaChoice
   , addProcessDefinitions
   
   , ParOrSeq                ( .. )
   , isParOrSeqComponent
   , getRateSpec   
   )
where

{- External Library Modules Imported -}
{- Standard Modules Imported -}
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Maybe
  ( mapMaybe )
{- Local Modules Imported -}
import qualified Language.Pepa.QualifiedName as Qualified
import qualified Language.Pepa.Syntax as Pepa
import Language.Pepa.Syntax
  ( ParsedModel          ( .. )
  , ProcessDef
  , ParsedComponent      ( .. )
  , ParsedComponentId
  , Transition           ( .. )
  , ParsedTrans
  , ParsedAction         ( .. )
  )
{- End of Imports -}

-- | A type containing all the transitions in a given model.
data TransitionAnalysis =
  TransitionAnalysis { modelNamedTransitions :: CanonicalSuccessors
                     , modelNoSourceTrans    :: [ ( ParsedTrans
                                                  , ParsedComponentId
                                                  )
                                                ]
                     , modelNoTargetTrans    :: [ ( ParsedComponentId
                                                  , ParsedTrans
                                                  )
                                                ] 
                     , modelOrphanTrans      :: [ ParsedTrans ]
                     }

modelTransitionAnalysis :: ParsedModel -> TransitionAnalysis
modelTransitionAnalysis model =
  TransitionAnalysis { modelNamedTransitions = namedTrans
                     , modelNoSourceTrans    = noSourceTrans
                     , modelNoTargetTrans    = noTargetTrans
                     , modelOrphanTrans      = orphanTrans
                     }
  where
  allTransitions = allSuccessors model
  namedTrans     = [ (source, (trans, target)) |
                     (Just source, (trans, Just target)) <- allTransitions
                   ]
  noSourceTrans  = [ (trans, target) |
                     (Nothing, (trans, Just target)) <- allTransitions
                   ]
  noTargetTrans  = [ (source, trans) |
                     (Just source, (trans, Nothing)) <- allTransitions
                   ]
  orphanTrans    = [ trans |
                     (Nothing, (trans, Nothing)) <- allTransitions
                   ]


{-|
    Returns true if the pepa component given may perform the given action.
-}
isPepaActionEnabled :: ParsedAction -> ParsedComponentId -> ParsedModel -> Bool
isPepaActionEnabled  act ident model = 
    any ((act ==) . pepaTransAction) $ possibleTransitions model ident

-- | The type of transitions from components to components
type TransSuccessor = (ParsedTrans, Maybe ParsedComponentId)

-- | The type of /seen/ process identifiers
type Seen = [ ParsedComponentId ]

{-|
    Returns the list of possible transitions from a given starting point
    in a sequential component. This is not transitive, it only returns
    the transitions which can be made now.
-}
possibleTransSuccessors :: ParsedModel -> ParsedComponentId -> [ TransSuccessor ]
possibleTransSuccessors model ident =
    snd $ possibleTrans [] ident
    where   
    -- The first argument is a list of seen processes this stops us
    -- going into the inevitable infinite loop.
    possibleTrans :: Seen -> ParsedComponentId -> ( Seen , [ TransSuccessor ] )
    possibleTrans seen ident2
        | elem ident2 seen = (seen, [])
        | otherwise       = 
            case findProcessDefinition model ident2 of
               Nothing   -> error $ notFound ident2
               Just comp -> possibleTransComp (ident2 : seen) comp

    -- The same thing here, the first argument is the list of seen
    -- processes.
    possibleTransComp :: Seen -> ParsedComponent -> (Seen, [ TransSuccessor ])
    possibleTransComp seen (StopProcess)              = (seen, [])
    possibleTransComp seen (IdProcess ident2)         =
        possibleTrans seen ident2
    possibleTransComp seen (CondBehaviour _ right)    =
        possibleTransComp seen right
    possibleTransComp seen (ComponentSum left right)  =
        -- Note, that I think this actually messes up on: @P = Q + Q@
        -- that is it will produce less transitions that it should.
        (rightSeen, leftTrans ++ rightTrans)        
        where
        (leftSeen, leftTrans)   = possibleTransComp seen left
        (rightSeen, rightTrans) = possibleTransComp leftSeen right
    possibleTransComp seen (PrefixComponent trans s)  =
        -- Here we do not call 'ident2' recursively hence we are
        -- /not/ finding all the reachable transitions only the ones
        -- that are possible NOW.
        case s of
            IdProcess ident2 -> (seen, [ (trans, Just ident2) ])
            _                -> (seen, [ (trans, Nothing ) ])
    possibleTransComp _seen (Cooperation _ _ _ )      =
        error errorMessage
    possibleTransComp _seen (ProcessArray _ _ _ )     =
        error errorMessage
    possibleTransComp _seen (Hiding _ _ )             =
        error errorMessage

    errorMessage  = "Attempt to obtain the sequetional transitions " ++
                    "of a parallel component"
    notFound name = unwords [ "component:"
                            , Qualified.textual name
                            , "not found when attempting to gather next"
                            , "transitions"
                            ]

{-|
   The same as 'possibleTransSuccessors' except we remove the successors. 
-}
possibleTransitions :: ParsedModel -> ParsedComponentId -> [ ParsedTrans ]
possibleTransitions model = (map fst) . (possibleTransSuccessors model)



{-|
  Finds all the derivatives of a given sequential component.
  This works whether the model is in normal form or not, but it
  will only return named derivatives.
  The behaviour is supposed to be somewhat undefined on ill-typed
  models, for example on in which a sequential component has a parallel
  or undefined derivative.
-}
derivativesOfComponent :: ParsedModel -> ParsedComponentId 
                       -> [ ParsedComponentId ]
derivativesOfComponent model startId =
  mapMaybe snd tSuccs
  where
  tSuccs = possibleTransSuccessors model startId
  

{-|
  Finds all the process definitions reachable (or related) to a given
  process name. So this in a sense finds the sub-model related to a given
  process name. This includes the definition for the given ParsedComponentId
  since this is reachable by virtue of doing nothing. Normally this doesn't
  make much difference since it is reachable since the process is cyclic.
-}
reachableProcessDefs :: ParsedModel -> ParsedComponentId -> [ ProcessDef ]
reachableProcessDefs model startId =
  getDerivativesId [] startId
  where
  modelDefs = modelProcessDefs model
  getDef :: ParsedComponentId -> Maybe ParsedComponent
  getDef ident = lookup ident modelDefs

  getDerivativesId :: [ ProcessDef ] -> ParsedComponentId -> [ ProcessDef ]
  getDerivativesId seen currentId
    | any ((== currentId) . fst) seen    = seen
    | Just comp <- getDef currentId      =
      getDerivatives ((currentId, comp) : seen) comp
    | otherwise                               =
      error $ "Undefined component in reachable: "  ++ 
              (Qualified.textual currentId)

  getDerivatives :: [ ProcessDef ] -> ParsedComponent ->[ ProcessDef ]
  getDerivatives seen (IdProcess currentId)              =
    getDerivativesId seen currentId
  getDerivatives seen (PrefixComponent _trans next)      =
    getDerivatives seen next
  getDerivatives seen (ComponentSum left right)          = 
    getDerivatives (getDerivatives seen left) right
  getDerivatives seen (CondBehaviour _rate comp)         =
    getDerivatives seen comp
  getDerivatives seen (StopProcess)                      = seen
  getDerivatives seen (Cooperation left _actions right)  =
    getDerivatives (getDerivatives seen left) right
  getDerivatives seen (ProcessArray comp _size _coop)    =
    getDerivatives seen comp
  getDerivatives seen (Hiding comp _coop)                =
    getDerivatives seen comp


{-|
  'derivativesOfComponent' finds those derivatives which can be reached
  via a single transition. 'transitiveDerivatives' finds all reachable
  derivatives from the given starting point.
  Note that the given 'startId' is only returned if it is reachable
  from itself.
-}
transitiveDerivatives :: ParsedModel            -- ^ The input model
                      -> ParsedComponentId      -- ^ The named starting point
                      -> [ ParsedComponentId ]  -- ^ Reachable derivatives
transitiveDerivatives model startId =
  getDerives startId []
  where
  getDerives :: ParsedComponentId -> [ ParsedComponentId ]
             -> [ ParsedComponentId ]
  getDerives ident seen 
    | elem ident seen = seen
    | otherwise       = 
      foldr getDerives (ident : seen) $ derivativesOfComponent model ident
     



{-|
  Finds all the transitions of a model, note that it is possible that some
  of these can never be performed, for example if they are defined within
  an unused process.
-}
allTransitionsOfModel :: ParsedModel -> [ ParsedTrans ]
allTransitionsOfModel model =
  concatMap (getTransitions . snd) pDefs
  where
  pDefs = modelProcessDefs model
  getTransitions :: ParsedComponent -> [ ParsedTrans ]
  getTransitions (IdProcess _)                = []
  getTransitions (StopProcess)                = []
  getTransitions (PrefixComponent trans next) = trans : (getTransitions next)
  getTransitions (ComponentSum left right)    = (getTransitions left) ++
                                                (getTransitions right)
  getTransitions (CondBehaviour _cond comp)   = getTransitions comp
  -- Shouldn't really get a transition in a parallel component but this won't hurt.
  getTransitions (Cooperation left _ right)   = (getTransitions left) ++
                                                (getTransitions right)
  getTransitions (ProcessArray _ _ _)         = []
  getTransitions (Hiding comp _)              = getTransitions comp

    

{-
   (map takeTrans) . allSuccessors 
   where
   takeTrans :: SuccessorTrans -> ParsedTrans
   takeTrans = fst . snd
-}

{-|
   Yet another type of successor transition.
   This one includes the possibility of having the identifier
   of the component from which we are transisting.
   This is a good analysis to do when the model is in normal
   form such that every prefix component is a single prefix
   component within a process definition such as:
   @P = (a, r).Q@
   That is, there is no chaining of prefix components and they are not
   a part of other components such as prefixes.

   [@todo@] all the other ones should just call this and then
   map the result.

   I still think this is not quite right, if we have
   @P = S@
   then we should have all the successors of @S@ as being possible
   from @P@.

   So now this uses 'possibleTransSuccessors'

   This should be re-written to descend from the main composition.
   My worry here is that we might duplicate the transitions in the whole
   model due to aliases (of course we shouldn't call this if the model
   is not in normal form, but still). 
-}
type SuccessorTrans = ( Maybe ParsedComponentId
                      , (ParsedTrans, Maybe ParsedComponentId)
                      )
type SuccessorTransMap = [ SuccessorTrans ]

allSuccessors :: ParsedModel -> SuccessorTransMap
allSuccessors model =
   concatMap succsOfDef pDefs
   where
   pDefs      = modelProcessDefs model
   succsOfDef :: ProcessDef -> SuccessorTransMap
   succsOfDef (ident, _comp) =
      map addIdent transSuccs
      where
      transSuccs = possibleTransSuccessors model ident
      addIdent :: TransSuccessor -> SuccessorTrans
      addIdent t = (Just ident, t)


{- The type of the less forgiving 'SuccessorTransMap' which is the result
  of 'allSuccessorsCanonical' which is only to be called on those models
  in canonical form.
-}
type CanonicalSuccessor = ( ParsedComponentId
                          , (ParsedTrans, ParsedComponentId) )
type CanonicalSuccessors = [ CanonicalSuccessor ]

{-
  'allSucessorsCanonical' is just as the above 'allSuccessors' however
  it will return 'Nothing' in the case that any of the successors has
  no name for the source component or no name for the target component.
  This means it should only be called with a model in canonical form.
-}
allSuccessorsCanonical :: ParsedModel -> Maybe CanonicalSuccessors
allSuccessorsCanonical model =
  if any (not . hasBothNames) successors
  then Nothing
  else Just okay
  where
  successors      = allSuccessors model
  okay            = [ (sou, (t, tar) ) | (Just sou, (t, Just tar)) <- successors ]
  hasBothNames :: SuccessorTrans -> Bool
  hasBothNames (Just _, (_, Just _)) = True
  hasBothNames _                     = False
      
{-
   succsOfDef :: ProcessDef -> SuccessorTransMap
   succsOfDef (ident, PrefixComponent t (IdProcess ident2)) =
      [ (Just ident, (t, Just ident2)) ]
   succsOfDef (ident, PrefixComponent t next)               =
      ( Just ident, (t, Nothing) ) : (succsOfProcess next)
   succsOfDef (ident, ComponentSum left right)              =
      (succsOfDef (ident, left)) ++
      (succsOfDef (ident, right))
   succsOfDef (_, component)                                =
      succsOfProcess component
   
   succsOfProcess :: ParsedComponent -> SuccessorTransMap
   succsOfProcess (IdProcess _)                         =
      []
   -- So here the second maybe is a 'Just' because there is
   -- an ident, we assume that the definition is not directly
   -- above here since in that case it would have been caught
   -- by 'succsOfDef'
   succsOfProcess (PrefixComponent t (IdProcess ident)) =
      [ (Nothing, (t, Just ident)) ]
   -- Right, now we have no ident as the source or the target
   succsOfProcess (PrefixComponent t next)              =
      (Nothing, (t, Nothing)) : (succsOfProcess next)
   succsOfProcess (ComponentSum left right)             =
      (succsOfProcess left) ++ (succsOfProcess right)
   
   -- There shouldn't be any in the parallel ones, but oh well
   succsOfProcess (Cooperation left _ right)            =
      (succsOfProcess left) ++ (succsOfProcess right)
   succsOfProcess (ProcessArray _ _ _)                  =
      []
   succsOfProcess (Hiding left _)                       =
      succsOfProcess left
-}

-- | Finds a process definition in a pepa model
findProcessDefinition :: ParsedModel -> ParsedComponentId
                      -> Maybe ParsedComponent
findProcessDefinition pModel ident  =
    lookup ident $ modelProcessDefs pModel


{-| makes a choice between a given number of components.
    Note that this currently just raises an error if passed the empty list,
    an alternative would be to go to the special @Stop@ process.
-}
makePepaChoice :: [ ParsedComponent ] -> ParsedComponent
makePepaChoice [] = error "makePepaChoice called with an empty list"
makePepaChoice ls =
    foldl1 ComponentSum ls

addProcessDefinitions :: [ ProcessDef ] -> ParsedModel -> ParsedModel
addProcessDefinitions defs model =
  model { modelProcessDefs = defs ++ (modelProcessDefs model) }

{-| The type returned by 'isParOrSeqComponent' -}
data ParOrSeq = IsPar | IsSeq | ParOrSeq
                deriving Eq

{-|
   Returns whether or not a component is a parallel component.
   It relies on a function to tell whether a component identifier
   is parallel or sequential. This will depend on the state of the
   model at the time. If for example we have applied a tranformation
   to remove all parallel defs then all component identifiers can
   be assumed to be sequential.
-}
isParOrSeqComponent :: ParOrSeq --  What to return for an identifier 
                    -> ParsedComponent -> ParOrSeq
isParOrSeqComponent idRes (IdProcess _)           = idRes
isParOrSeqComponent _     (StopProcess)           = IsSeq
isParOrSeqComponent _     (PrefixComponent _ _)   = IsSeq
isParOrSeqComponent _     (ComponentSum _ _)      = IsSeq
isParOrSeqComponent _     (CondBehaviour _ _)     = IsSeq
isParOrSeqComponent _     (Cooperation _ _ _)     = IsPar
isParOrSeqComponent _     (ProcessArray _ _ _)    = IsPar
isParOrSeqComponent _     (Hiding _ _)            = IsPar


{-|
   Get the rate specification of the given rate name if it
   occurs in the model. We only do a textual search for the
   name.   
-} 
getRateSpec :: ParsedModel -> String -> Maybe Pepa.RateSpec
getRateSpec model name =
  List.find isRightRate $ Pepa.rateDefsOfModel model
  where
  isRightRate :: Pepa.RateSpec -> Bool
  isRightRate (q, _) = name == (Qualified.textual q)