{-| This module implements simplifying transformations over
    parsed pepa models
-}
module Language.Pepa.Transform.Simplify
  ( SimplifyResult      ( .. )
  , NameRateMap
  , combineSimplifiedMappings
  , simplify
  , liftComplex
  , liftRateExprs
  , reduceModelRateExprs
  , removeAliases
  , expandProcessArrays
  , removeParallelDefs
  , removeConditionalBehaviour
  , removeUnusedDefinitions
  , relegateConditionalBehaviour
  , removeHidingUnqualifiedModel
  , modifyModelTransitions
  -- , reduceConstantMappings
  )
where

{- Standard Libraries modules imported -}
import Control.Arrow
  ( first
  , second
  )
import Control.Monad 
  ( liftM )
import qualified Data.Function as Function
import Data.List
  ( mapAccumL
  , unionBy
  , partition
  )
import qualified Data.Map as Map
import Data.Map
  ( Map )
import qualified Data.Maybe as Maybe
import Data.Maybe
  ( mapMaybe
  , fromMaybe
  )
{- External Library modules imported -}
{- Local modules imported imported -}
import Ipc.Cli
  ( CliOpt    ( .. )
  , containsHideNonCoop
  )
import qualified Language.Pepa.QualifiedName as Qualified
import Language.Pepa.QualifiedName
  ( QualifiedName     ( .. )
  , qualifyQName
  , ShowOrig          ( .. )
  )
import qualified Language.Pepa.Rates as Rates
import Language.Pepa.Rates
  ( Rate              ( .. )
  , RateExpr          ( .. )

  , modifyRate
  , normaliseRateExpr
  , remapRateExpr
  , namedRateExp
  , rateExpressionInt
  )
import Language.Pepa.Syntax
  ( ParsedModel       ( .. )
  , ProcessDef
  , RateSpec
  , ParsedComponentId 
  , ParsedComponent   ( .. )
  , CooperationSet    ( .. )
  , Transition        ( .. )
  , ParsedTrans
  , ParsedAction      ( .. )
  , ActionIdentifier
  )
import Language.Pepa.Transform.NameSupply
  ( NameSupply
  , initialNameSupply
  , getName
  )
import qualified Language.Pepa.PepaUtils as PepaUtils
import Language.Pepa.Analysis.Analysis 
  ( AnalysisReport      ( .. )
  , analyseModel
  )
import Language.Pepa.Transform.HideAll
  ( hideAllActions )

import Language.Pepa.Print
  ( hprintPepaModel )
import Language.Pepa.MainControl
  ( MainControl
  , valueResult
  )
{- End of imports -}


{-| A name rate-expression mapping turns a process name into a rate-expression
    which will evaluate into the concentration of the name component.
    Of course the rate expression returned will be a functional rate involving
    the names of processes that the process in which you are interested in
    has been simplified to.
-}
type NameRateMap = Map String RateExpr

{-| The result of simplifying a model -}
data SimplifyResult =
  -- Note that I think the second field here should take into
  -- account aliasing and it's probably not too hard to.
  -- Because we already have the alias map.
  Simplify { simplifiedModel       :: ParsedModel
           , simplifiedRateMapping :: NameRateMap
             -- ^ Maps a string representing a component name
             --   into an expression which will evaluate to the
             --   concentration of the component name in the 
             --   simplified model. This takes into account
             --   the qualification of the model. But as yet
             --   not aliasing.
           , simplifiedAliasMap    :: AliasMap
           , simplifiedVirtualMap  :: NameRateMap
           }

{-| Often from the mapping produced from the simplification procedure we
    we would a single mapping which tells us how to compute the concentration
    of a named component in the new simplified model which doesn't contain
    the process name we are interested in.
    This function combines the various mappings within a simplification result
    into a single mapping from name rate-expression mapping.
-}
combineSimplifiedMappings :: SimplifyResult -> NameRateMap
combineSimplifiedMappings simpResult =
  Map.union rateMapping $ Map.union virtualMap aliasRateMap
  where
  rateMapping   = simplifiedRateMapping simpResult
  -- The alias map with textual keys and rate expressions instead of
  -- component names as the values. Here is the important bit, the alias
  -- map now, in a way, invalid, since removing the aliases was done before
  -- qualifying the names so we have to look up the name in the rateMapping.
  aliasRateMap  = Map.mapMaybe getRateExpr aliasTextKeys
  getRateExpr :: ParsedComponentId -> Maybe RateExpr
  getRateExpr ident = Map.lookup (Qualified.textual ident) rateMapping
  -- The alias map but with textual keys rather than qualified names
  aliasTextKeys = Map.mapKeys Qualified.textual aliasMap
  aliasMap      = simplifiedAliasMap simpResult
  -- The virtual map holds the rate expressions corresponding to the
  -- the virtual components
  virtualMap    = simplifiedVirtualMap simpResult

{-|
   Applies all the simplifications to a model.
-}
simplify :: [ CliOpt a ] 
            -- ^ The command-line options used for example
            --   to tell if we should aggregate arrays
         -> ParsedModel 
            -- ^ The parsed model
         -> MainControl SimplifyResult
            -- ^ The returned model plus the qualified mapping
simplify options parsedModel = 
  valueResult returnValue logKey logInfo
  where
  returnValue       =  Simplify { simplifiedModel       = simpModel
                                , simplifiedRateMapping = rateExprMapping
                                , simplifiedAliasMap    = aliasMapping
                                , simplifiedVirtualMap  = virtualMap
                                }
  simpModel         = liftComplex
                    . removeHiding
                    . hideNonCooperating 
                    $ noVirtualModel

  -- We wish to remove virtual process definitions as well since they
  -- contribute nothing to the actual compilation of the model and
  -- are (at the time of writing) specific to ipc so other forms such
  -- as pepato cannot handle them.
  (noVirtualModel,
   virtualMap)      = removeVirtualComponents qualifiedModel

  -- When we qualify the model we also obtain a mapping from
  -- strings to rate expressions. This allows us to convert a rate
  -- expression containing references to process names.
  -- For example: if P then 2.0 else 3.0
  -- can be turned into 
  -- if P_1 + P_2 + P_3 then 2.0 else 3.0
  -- if P_1, P_2 and P_3 represent the qualified occurences of P
  (qualifiedModel,
   rateExprMapping) = qualifyModel
                    . removeParallelDefs
                    $ noAliasesModel

  -- we remove aliases before removing parallel defs 
  -- that way the remove parallel definitions function 
  -- can get rid of all non-sequential definitions.
  (noAliasesModel,
   aliasMapping)    = removeAliases
                    -- Removal of process arrays must be done before
                    -- 'qualifyModel' because it will produce several
                    -- references to the same process.
                    . removeProcessArraysOrNot 
                    -- Note that this *should* be done before we can remove
                    -- the process arrays, because the size of an array may
                    -- be given by a rate expression but when removing the
                    -- arrays we have to know the actual number of elements
                    -- in order to expand it.
                    . reduceModelRateExprs
                    $ parsedModel

  logKey = "simplified-model"
  logInfo = hprintPepaModel simpModel

  -- If the @--hide-non-coop@ option is set then we perform
  -- the transformation however if it is not set then it is
  -- simply the identity transformation.
  hideNonCooperating :: ParsedModel -> ParsedModel
  hideNonCooperating model
    | containsHideNonCoop options = 
      hideAllActions (analyseModel model) model
    | otherwise                   = model

  -- Here if the @aggregate@ option is set then we do not expand
  -- the process arrays. If the @--no-aggregate@ option is set
  -- then we do expand the process arrays. We now do nothing if
  -- neither is set, this means that aggregation is on by default.
  removeProcessArraysOrNot :: ParsedModel -> ParsedModel
  removeProcessArraysOrNot
    | any isAggregateOpt options   = id
    | any isNoAggregateOpt options = expandProcessArrays
    | otherwise                    = id
    
  isAggregateOpt :: CliOpt a -> Bool
  isAggregateOpt (CliAggregate) = True
  isAggregateOpt _              = False

  isNoAggregateOpt :: CliOpt a -> Bool
  isNoAggregateOpt (CliNoAggregate) = True
  isNoAggregateOpt _                = False


{-|
    lift complex -- This tranformation will lift complex sub-process forms
    to become definitions of their own.
    For example:
    @ P = (a, r).(b, r).Q ; @
    becomes
    @ P  = (a, r).P1 ;
      P1 = (b, r).Q  ;
    @

    For now I've taken out the component sum part of this. Previously
    if we had;
    @ P = (a, r).P1 + (b, r).P2 @
    this was transformed into:
    @ P = Left + Right ;
      Left = (a, r).P1 ;
      Right = (b,r).P2 ;
    @
    But I've taken this behaviour out as I think it is needless.
-}
liftComplex :: ParsedModel -> ParsedModel
liftComplex model =
    model { modelProcessDefs = newDefinitions }
    where
    newDefinitions = aliasDefList initialNameSupply $ modelProcessDefs model

    -- Possibly a little tricky to understand but it's pretty simple really
    -- 'mapAccumL' applies 'aliasDef' to each of process definitions, with
    -- the name supply as the accumulator. So basically each process definition
    -- is processed with the name supply accumulated so far. The processing
    -- of each definition will, along with the accumulated name supply return
    -- a list of new definitions, so we end up with a list of list of process
    -- definitions, so we concatenate them to give us our final list of
    -- process definitions.
    aliasDefList :: NameSupply -> [ ProcessDef ] -> [ ProcessDef ]
    aliasDefList ns = concat . snd . (mapAccumL aliasDef ns)

    aliasDef :: NameSupply -> ProcessDef -> ( NameSupply, [ ProcessDef ] )
    aliasDef renamer (ident, process) =
        (newRenamer, (ident, newProcess) : newDefs)
        where
        (newRenamer, newProcess, newDefs) = aliasProcess renamer process

    aliasProcess :: NameSupply -> ParsedComponent
                -> ( NameSupply, ParsedComponent, [ ProcessDef ] )
    aliasProcess renamer proc@(StopProcess)                         =
        (renamer, proc, [])
    aliasProcess renamer proc@(IdProcess _)                         =
        (renamer, proc, [])
    aliasProcess renamer proc@(PrefixComponent _ (IdProcess _))     =
        (renamer, proc, [])
    aliasProcess renamer (PrefixComponent trans nextProcess)        =
        (newNs, newProcess, newDef : processDefs)
        where
        newDef        = (newIdent, newNext)
        newIdent      = Unqualified $ snd newName
        newProcess    = PrefixComponent trans $ IdProcess newIdent
        (newNs, 
         newNext,
         processDefs) = aliasProcess (fst newName) nextProcess
        newName       = getName renamer "Prefix_"
    aliasProcess renamer proc@(CondBehaviour _ (IdProcess _))       =
        (renamer, proc, [])
    aliasProcess renamer (CondBehaviour cond nextProcess)           =
        (newNs, newProcess, newDef : processDefs)
        where
        newDef        = (newIdent, newNext)
        newIdent      = Unqualified $ snd newName
        newProcess    = CondBehaviour cond $ IdProcess newIdent
        (newNs, 
         newNext,
         processDefs) = aliasProcess (fst newName) nextProcess
        newName       = getName renamer "CondB_"
    {- As per the comment at the top we are no longer lifting branches
       of a component sum. This case is the all encomposing case of
       which will not simplify component sums, but will of course do
       the other cases of simplification such that:
       @ P = (a, r) . (b, r) . P1 + (c, r) . (d, r) . P2 ; @
       becomes
       @ P = (a, r) . P1_0 + (c, r) . P2_0 ;
         P1_0 = (b, r) . P1 ;
         P2_0 = (d, r) . P2 ;
       @
       Notice how the component sum is not simplified but the prefixes
       contained therein are.
    -}
    aliasProcess renamer (ComponentSum left right)                  =
      (rightNs, newSum, leftDefs ++ rightDefs)
      where
      newSum                         = ComponentSum newLeft newRight
      (leftNs,  newLeft,  leftDefs)  = aliasProcess renamer left
      (rightNs, newRight, rightDefs) = aliasProcess leftNs  right
    {-
      Component sums are a bit awkward since we do not wish to introduce
      needless aliases such @P = S@, therefore we have to check if either
      of the branches are already an identifier process. There are therefore
      four cases, both are identifiers, the left is, the right is and neither
      are identifiers.

      HOWEVER, these cases are currently disabled -- see comment at top.
    aliasProcess renamer proc@(ComponentSum (IdProcess _)  
                                            (IdProcess _))          =
       ( renamer, proc, [] )
    aliasProcess renamer (ComponentSum left right@(IdProcess _))    =
       ( leftNs
       , ComponentSum (IdProcess newIdent) right
       , newDef : leftDefs
       )
       where
       newDef      = (newIdent, leftProc)
       newIdent    = Unqualified $ snd newName
       newName     = getName renamer "LeftCompSum_"
       (leftNs,
        leftProc,
        leftDefs)  = aliasProcess (fst newName) left
    aliasProcess renamer (ComponentSum left@(IdProcess _) right)    =
       ( rightNs
       , ComponentSum left $ IdProcess newIdent
       , newDef : rightDefs
       )
       where
       newDef      = (newIdent, rightProc)
       newIdent    = Unqualified $ snd newName
       newName     = getName renamer "RightCompSum_"
       (rightNs,
        rightProc,
        rightDefs) = aliasProcess (fst newName) right
    aliasProcess renamer (ComponentSum left right)                  =
        ( rightNs
        , ComponentSum (IdProcess leftIdent) (IdProcess rightIdent)
        , leftDef : rightDef : (leftDefs ++ rightDefs)
        )
        where
        leftDef     = (leftIdent, leftProc)
        rightDef    = (rightIdent, rightProc)
        
        leftIdent   = Unqualified $ snd leftName
        rightIdent  = Unqualified $ snd rightName

        leftName    = getName renamer "LeftCompSum_"
        rightName   = getName (fst leftName) "RightCompSum"

        (leftNs,
         leftProc,
         leftDefs)  = aliasProcess (fst rightName) left
        (rightNs,
         rightProc,
         rightDefs) = aliasProcess leftNs right
    -}
    -- Now all of the parallel operators should not contain any prefix operator.
    -- so they can be returned as is. We may revise this if we decide that we
    -- want to allow @(a,r).P <> (b,r).P@ which in some sense is not ludicrous.
    aliasProcess renamer proc@(Cooperation _ _ _)                   =
        (renamer, proc, [])
    aliasProcess renamer proc@(ProcessArray _ _ _)                  =
        (renamer, proc, [])
    aliasProcess renamer proc@(Hiding _ _)                          =
        (renamer, proc, [])

{- lift rate expressions -}
liftRateExprs :: ParsedModel -> ParsedModel
liftRateExprs model = 
   model { modelRateSpecs   = (modelRateSpecs model) ++ newRateDefs
         , modelProcessDefs = newProcessDefs
         }
   where
   newRateDefs    = concat $ fst newDefinitions
   newProcessDefs = snd newDefinitions
   newDefinitions = unzip $ liftDefList initialNameSupply pDefs
   pDefs          = modelProcessDefs model

   -- Possibly a little tricky to understand but it's pretty simple really
   -- 'mapAccumL' applies 'liftDef' to each of process definitions, with
   -- the name supply as the accumulator. So basically each process definition
   -- is processed with the name supply accumulated so far. The processing
   -- of each definition will, along with the accumulated name supply return
   -- a list of new definitions, so we end up with a list of list of process
   -- definitions, so we concatenate them to give us our final list of
   -- process definitions.
   liftDefList :: NameSupply -> [ ProcessDef ]
               -> [ ( [ RateSpec ], ProcessDef ) ]
   liftDefList ns = snd . (mapAccumL liftDef ns)

   liftDef :: NameSupply -> ProcessDef 
           -> ( NameSupply, ( [ RateSpec ], ProcessDef ) )
   liftDef renamer (ident, process) =
       (newRenamer, ( newRDefs, (ident, newProcess)) )
       where
       (newRenamer, 
        newProcess, 
        newRDefs)  = liftProcess renamer process
   
   liftProcess :: NameSupply -> ParsedComponent
               -> (NameSupply, ParsedComponent, [ RateSpec ])
   liftProcess renamer proc@(StopProcess)                                    =
       (renamer, proc, [])
   liftProcess renamer proc@(IdProcess _)                                    =
       (renamer, proc, [])
   liftProcess renamer (PrefixComponent trans nextP)                         =
      case pepaTransRate trans of
         RateTop _             -> nonLifted
         RateImmediate _       -> nonLifted
         RateTimed (Cident _ ) -> nonLifted
         RateTimed (Creal  _ ) -> nonLifted
         RateTimed rExp        -> 
            (newLiftedR, liftedP, rDef : newLiftedRdefs)            
            where
            liftedP      = PrefixComponent newTrans newLiftedNext
            (newLiftedR,
             newLiftedNext,
             newLiftedRdefs) = liftProcess (fst newName) nextP
            
            newIdent     = Unqualified $ snd newName
            newName      = getName renamer "liftedRate_"
            newTrans     = trans { pepaTransRate = newRate }
            newRate      = RateTimed $ namedRateExp newIdent -- (Just rExp)
            rDef            = (newIdent, rExp)
      where
      nonLifted    = (newRenamer, nonLiftedP, newRDefs)
      nonLiftedP   = PrefixComponent trans newNextP
      (newRenamer,
       newNextP,
       newRDefs)   = liftProcess renamer nextP

{-
   liftProcess renamer (PrefixComponent (t, RateTop) nextP)                  =
       (newRenamer, newProc, newRDefs)
       where
       newProc      = PrefixComponent (t, RateTop) newNextP
       (newRenamer,
        newNextP,
        newRDefs)   = liftProcess renamer nextP
   liftProcess renamer (PrefixComponent (t, RateImmediate weight) nextP)     =
       (newRenamer, newProc, newRDefs)
       where
       newProc      = PrefixComponent (t, RateImmediate weight) newNextP
       (newRenamer,
        newNextP,
        newRDefs)   = liftProcess renamer nextP
   liftProcess renamer (PrefixComponent (t, RateTimed (RateId n mr)) nextP)  =
       (newRenamer, newProc, newRDefs)
       where
       newProc      = PrefixComponent (t, RateTimed $ RateId n mr) newNextP
       (newRenamer,
        newNextP,
        newRDefs)   = liftProcess renamer nextP
   liftProcess renamer (PrefixComponent (t, RateTimed rExp) nextP)           =
       (newRenamer, newProcess, newRDef : newRDefs)
       where
       newRDef      = (newIdent, rExp)
       newProcess   = PrefixComponent newTrans newNextP
       newTrans     = (t, RateTimed $ RateId newIdent $ Just rExp)
       newIdent     = Unqualified $ snd newName
       newName      = getName renamer "liftedRate_"

       (newRenamer,
        newNextP,
        newRDefs)   = liftProcess (fst newName) nextP
   -}
   liftProcess renamer (CondBehaviour cond nextP)                            =
      (newRenamer, newProc, newRDefs)
      where
      newProc       = CondBehaviour cond newNextP
      (newRenamer,
        newNextP,
        newRDefs)   = liftProcess renamer nextP
   liftProcess renamer (ComponentSum left right)                             =
       (newRenamerRight, newProcess, rDefsLeft ++ rDefsRight)
       where
       newProcess   = ComponentSum newLeft newRight
       (newRenamerLeft,
        newLeft,
        rDefsLeft)  = liftProcess renamer left
       (newRenamerRight,
        newRight,
        rDefsRight) = liftProcess newRenamerLeft right
   -- Parallel processes should not really require this as they
   -- shouldn't contain any transitions, but we may later change
   -- our minds about that, and this doesn't hurt.
   liftProcess renamer (Cooperation left actions right)                      =
       (newRenamerRight, newProcess, rDefsLeft ++ rDefsRight)
       where
       newProcess   = Cooperation newLeft actions newRight
       (newRenamerLeft,
        newLeft,
        rDefsLeft)  = liftProcess renamer left
       (newRenamerRight,
        newRight,
        rDefsRight) = liftProcess newRenamerLeft right
   liftProcess renamer proc@(ProcessArray _ _ _)                             =
       (renamer, proc, [])
   liftProcess renamer (Hiding proc coopActs)                                =
       (newRenamer, newProc, newRDefs)
       where
       newProc      = Hiding newNextP coopActs
       (newRenamer,
        newNextP,
        newRDefs)   = liftProcess renamer proc


{-
  Remove all the rate definitions from the model resulting
  in one which has reduced rate expressions. In general this will
  mean reducing to a rate value (ie a double) but might not
  fully reduce the expression due to functional rates.
-}
reduceModelRateExprs :: ParsedModel -> ParsedModel
reduceModelRateExprs model =
  -- Perform this reduction using the modifyModelTransitions function
  model { modelRateSpecs   = []
        , modelProcessDefs = newPdefs
        , modelSystemEqn   = modifyProcess $ modelSystemEqn model
        }
  where
  newPdefs = map modifyProcessDef $ modelProcessDefs model

  -- A process definition then is modified simply by modifying
  -- the associated process
  modifyProcessDef :: ProcessDef -> ProcessDef
  modifyProcessDef = second modifyProcess

  -- For each process then we must modify the transition of any
  -- prefix component, and for any process arrays we must
  -- modify their size expressions by reducing them.
  modifyProcess :: ParsedComponent -> ParsedComponent
  modifyProcess proc@(StopProcess)                = proc
  modifyProcess proc@(IdProcess _)                = proc
  modifyProcess (PrefixComponent trans nextP)     =
    PrefixComponent (modifyTrans trans) $ modifyProcess nextP
  modifyProcess (CondBehaviour cond left)         =
    CondBehaviour cond $ modifyProcess left
  modifyProcess (ComponentSum left right)         =
    ComponentSum (modifyProcess left) 
                 (modifyProcess right)
  modifyProcess (Cooperation left actions right)  =
    Cooperation (modifyProcess left)
                actions
                (modifyProcess right)
  modifyProcess (ProcessArray comp size mActions) =
    ProcessArray (modifyProcess comp)
                 (reduceExpr size)
                 mActions
  modifyProcess (Hiding comp actions)             =
    Hiding (modifyProcess comp) actions
 

  -- The modify transition function then reduces the rate expressions within
  -- the given transition using the constant mapping
  -- Note that here we must modify both the conditions and the rate expressions
  -- since the rate expressions in question may well be functional rates.
  modifyTrans :: ParsedTrans -> ParsedTrans
  modifyTrans tr = 
    tr { pepaTransConditions = newConds
       , pepaTransRate       = renameRate $ pepaTransRate tr
       }
    where
    newConds   = map reduceExpr $ pepaTransConditions tr
    renameRate = modifyRate reduceExpr

  -- The reduced constant mappings, this makes the constant
  -- (or rate) definitions in their reduced form suitable
  -- for substitution into the model.
  constantMapping = reduceConstantMappings $ modelRateSpecs model
  -- So reduction of rate expression is in terms of this constant mapping
  reduceExpr :: RateExpr -> RateExpr
  reduceExpr = normaliseRateExpr constantMapping


    

{- 
   Remove parallel definitions
   So this is a bit of a misnomer, it doesn't remove the parallel
   definitions, but it does expand them in the main system component.
   Perhaps it should then march down the process definitions and
   remove them from the list.
   However this is essentially what 'qualifyModel' does.
-}
removeParallelDefs :: ParsedModel -> ParsedModel
removeParallelDefs model =
  model { modelSystemEqn = newSystem }
  where
  newSystem  = expandParallelAliases [] $ modelSystemEqn model
  
  -- Again here if the process is inherently a sequential process
  -- we do not bother recursing to the sub-process, for example
  -- prefix components we do not recurse to the next process
  -- since this should not contain any parallel components 
  -- (idents or otherwise)
  -- We also take in a list of 'seen' components to avoid a loop
  -- caused by a definition such as P = Q <> P ;
  expandParallelAliases :: [ ParsedComponentId ] 
                        -> ParsedComponent 
                        -> ParsedComponent
  expandParallelAliases _seen (StopProcess)     =
    StopProcess
  expandParallelAliases seen  (IdProcess ident) =
    case expandIdent seen ident of
      Nothing -> IdProcess ident
      Just p  -> p
  expandParallelAliases _seen proc@(PrefixComponent _ _)    = proc
  expandParallelAliases _seen proc@(ComponentSum _ _)       = proc
  expandParallelAliases _seen proc@(CondBehaviour _ _)      = proc
  -- Notice that process arrays although strictly speaking parallel
  -- are not removed, the reason for this is that if we are doing
  -- aggregation then we want to allow them to continue as an array.
  -- Note that here is currently a bug here in that we might have
  -- a process array in which the component in question is a parallel
  -- def.
  expandParallelAliases _seen proc@(ProcessArray _ _ _)     = proc
  expandParallelAliases seen  (Cooperation left acts right) =
    Cooperation (expandParallelAliases seen left) acts
                (expandParallelAliases seen right)
  expandParallelAliases seen  (Hiding proc acts)            =
    Hiding (expandParallelAliases seen proc) acts

  expandIdent :: [ ParsedComponentId ] -> ParsedComponentId
              -> Maybe ParsedComponent
  expandIdent seen ident
    | elem ident seen = error "alias loop found while removing parallel defs"
    | otherwise       =
      case lookup ident $ modelProcessDefs model of
        -- Shouldn't really happen, undefined process
        Nothing                     -> Nothing
        Just (IdProcess ident')     -> 
          expandIdent newSeen ident'
        Just p@(Cooperation _ _ _)  -> Just $ expandParallelAliases newSeen p
        Just p@(ProcessArray _ _ _) -> Just $ expandParallelAliases newSeen p
        Just p@(Hiding _ _)         -> Just $ expandParallelAliases newSeen p
        -- finally if it is a sequential component leave it as is
        Just _                      -> Nothing
      where
      newSeen = ident : seen


type AliasMap = Map ParsedComponentId ParsedComponentId

{-| 
   Remove needless aliases.
   Sometimes it is convenient to write :
   @ P = S ; @
   This tranformation replaces every occurrence of @P@ in the model
   with a @S@.
   We must be careful to avoid cyclic replacements ie if we have
   @ P = S ;
     S = P
   @
   then we must at least do the right thing here. The static-analysis
   phase should catch this case, but the user may have suppressed static
   analysis.

   NOTE: Care required; this does not remove the alias definitions
   only the need for them. They can be removed with
   'removeUnusedDefinitions'
-}
removeAliases :: ParsedModel -> (ParsedModel, AliasMap)
removeAliases model = 
  ( newModel, aliasMap )
  where
  newModel = model { modelProcessDefs = newPdefs 
                   , modelSystemEqn   = expandProcess $ modelSystemEqn model
                   }
  newPdefs = map (second expandProcess) pDefs
  pDefs    = modelProcessDefs model

  -- First build up a list mapping aliases to their unaliased
  -- definition.
  aliasMap :: AliasMap
  aliasMap = Map.fromList $ mapMaybe (expandAliasPair . fst) pDefs

  expandAliasPair :: ParsedComponentId
                  -> Maybe (ParsedComponentId, ParsedComponentId)
  expandAliasPair ident =
    do ident' <- expandAlias [] ident
       return (ident, ident')

  expandAlias :: [ ParsedComponentId ] -> ParsedComponentId
              -> Maybe ParsedComponentId
  expandAlias seen ident
    -- It is not our job to find alias loops
    | elem ident seen = Nothing
    | otherwise       =
      -- So basically we want to look up the identifier
      -- in the process definitions, if it returns another
      -- alias then we look that up, if not then we return
      -- the given identifier since it's the end of an alias
      -- chain.
      case lookup ident pDefs of
        -- It's also not our job to find undefined processes
        Nothing                 -> Just ident
        Just (IdProcess ident') -> expandAlias (ident : seen) ident'
        Just _                  -> Just ident

  -- Shouldn't we also expand functional rates?? I really think we should.
  expandProcess :: ParsedComponent -> ParsedComponent
  expandProcess (StopProcess)                    =
    StopProcess
  expandProcess (IdProcess ident)                =
    case Map.lookup ident aliasMap of
      Nothing -> IdProcess ident
      Just i  -> IdProcess i
  expandProcess (PrefixComponent t nextP)        =
    PrefixComponent t $ expandProcess nextP
  expandProcess (ComponentSum left right)        =
    ComponentSum (expandProcess left) (expandProcess right)
  expandProcess (CondBehaviour cond nextP)       =
    CondBehaviour cond $ expandProcess nextP
  expandProcess (Cooperation left actions right) =
    Cooperation (expandProcess left) actions (expandProcess right)
  expandProcess (ProcessArray comp size actions) =
    ProcessArray (expandProcess comp) size actions
  expandProcess (Hiding proc actions)            =
    Hiding (expandProcess proc) actions



{-
  Remove process arrays.
  This tranformation removes process arrays, one of two things can be done.
  Either a process such as
  @ P[3] @
  can be turned into
  @ P <> P <> P @
  or we leave it as simply
  @ P @
  but return a concentration database.
-}
expandProcessArrays :: ParsedModel -> ParsedModel
expandProcessArrays model =
   model { modelProcessDefs = newPdefs
         , modelSystemEqn   = expandProcess $ modelSystemEqn model
         }
   where
   newPdefs = map (second expandProcess) $ modelProcessDefs model
   -- Notice for example that we do not recursively expand
   -- prefix components, since they should not contain
   -- parallel components. The same for component sums.
   expandProcess :: ParsedComponent -> ParsedComponent
   expandProcess proc@(IdProcess _)               = proc
   expandProcess proc@(StopProcess)               = proc
   {- Okay these three all have sub-processes which could
      be processed recursively but since they are all sequential
      processes they should not have any process arrays within
      them.
   -}
   expandProcess proc@(PrefixComponent _ _)       = proc
   expandProcess proc@(ComponentSum _ _)          = proc
   expandProcess proc@(CondBehaviour _ _)         = proc
   expandProcess (Cooperation left actions right) =
       Cooperation (expandProcess left) actions (expandProcess right)
   expandProcess (Hiding proc actions)              =
       Hiding (expandProcess proc) actions
   expandProcess (ProcessArray comp i mActions)
       | size < 1     = error "Process array with less than 1"
       | otherwise    = expandArray size
       where
       size    = rateExpressionInt i
       actions = Maybe.fromMaybe [] mActions
       newComp = expandProcess comp
       expandArray :: Int -> ParsedComponent
       expandArray x
         | x ==  1   = newComp
         | otherwise = Cooperation newComp (ActionSet actions) $ 
                                           expandArray (x - 1)

{-|
   Remove the conditional behaviour based upon the values of rates
   within the model. This should clearly be applied after any rates
   have been overridden or selected from a set definition.

   TODO: we should reduce the rate expressions such that they are
   all simple rate numbers, but for now I will leave that for later.
-}
removeConditionalBehaviour :: ParsedModel -> ParsedModel
removeConditionalBehaviour model =
   model { modelProcessDefs = newProcessDefs
         , modelSystemEqn   = removeFromProcess $ modelSystemEqn model
         }
   where
   newProcessDefs = map (second removeFromProcess) $ modelProcessDefs model

   removeFromProcess :: ParsedComponent -> ParsedComponent
   removeFromProcess (CondBehaviour cond next) =
     case evaluateCondition cond of
       IsTrue    -> removeFromProcess next
       IsFalse   -> StopProcess
       IsDynamic -> 
          error ( "unimplemented the evaluation of c expressions " ++
                        "to remove conditional behaviour")
   removeFromProcess (ComponentSum left right)     =
      case (newLeft, newRight) of
         (StopProcess, _) -> newRight
         (_, StopProcess) -> newLeft
         _                -> ComponentSum newLeft newRight
      where
      newLeft  = removeFromProcess left
      newRight = removeFromProcess right
   removeFromProcess p@(IdProcess _)               = p
   removeFromProcess p@(StopProcess)               = p
   removeFromProcess (PrefixComponent trans next)  =
      PrefixComponent trans $ removeFromProcess next
   removeFromProcess (Cooperation left acts right) =
      Cooperation (removeFromProcess left)
                  acts
                  (removeFromProcess right)
   removeFromProcess p@(ProcessArray _ _ _)        = p
   removeFromProcess (Hiding comp actions)         =
      Hiding (removeFromProcess comp) actions 

   -- Obviously this is not quite sophisticated enough
   evaluateCondition :: RateExpr -> CBoolResult
   evaluateCondition (Creal 0.0)   = IsFalse
   evaluateCondition (Cconstant 0) = IsFalse
   evaluateCondition (Creal _ )    = IsTrue
   evaluateCondition (Cconstant _) = IsTrue
   evaluateCondition _             = IsDynamic
       
-- Returned from evaluating a C boolean expression
data CBoolResult = IsTrue | IsFalse | IsDynamic
                   deriving Eq

{-|
   This function performs a similar function to the previous
   'removeConditionalBehaviour' but it may be used ahead of the
   instantiation of rate identifiers. It moves the conditional
   behaviour inside the /ParsedTrans/ by making it a functional
   rate which is either the normal value or zero depending on
   the condition.
   Note that this function only works if the conditional
   behaviour is a prefix operator.

   In the other behaviour we could conceivably create a whole
   new process but for now we simply reject any other attempt.
-}
relegateConditionalBehaviour :: ParsedModel -> ParsedModel
relegateConditionalBehaviour model =
   model { modelProcessDefs = newProcessDefs
         , modelSystemEqn   = removeFromProcess $ modelSystemEqn model
         }
   where
   newProcessDefs = map (second removeFromProcess) $ modelProcessDefs model

   removeFromProcess :: ParsedComponent -> ParsedComponent
   removeFromProcess (CondBehaviour condExp (PrefixComponent trans next)) =
     PrefixComponent newTrans $ removeFromProcess next
     where
     newTrans = trans { pepaTransConditions = newConds }
     newConds = condExp : (pepaTransConditions trans)
   removeFromProcess (CondBehaviour _condExp _)                           =
     error "unimplemented conditional behaviour in relegateConditionalBehaviour"
   removeFromProcess (ComponentSum left right)                            =
      case (newLeft, newRight) of
         (StopProcess, _) -> newRight
         (_, StopProcess) -> newLeft
         _                -> ComponentSum newLeft newRight
      where
      newLeft  = removeFromProcess left
      newRight = removeFromProcess right
   removeFromProcess p@(IdProcess _)                                      = p
   removeFromProcess p@(StopProcess)                                      = p
   removeFromProcess (PrefixComponent trans next)                         =
      PrefixComponent trans $ removeFromProcess next
   removeFromProcess (Cooperation left acts right)                        =
      Cooperation (removeFromProcess left)
                  acts
                  (removeFromProcess right)
   removeFromProcess p@(ProcessArray _ _ _)                               = p
   removeFromProcess (Hiding comp actions)                                =
      Hiding (removeFromProcess comp) actions 

{-|
   Remove unused definitions. This should obviously occur after we have checked
   for unused definitions otherwise the type checker \/ analyser will not pick
   this up. However for example the srmc compiler must select process
   definitions and therefore we do not wish to compile out unnecessary definitions.
   In particular as well rate arrays should not be ranged over if the rate
   is never used.

   Rather than calling 'Language.Pepa.Analysis.Analysis.analyseModel' ourselves
   this function takes in an analysis report. This is in general the correct
   thing to do because there may be multiple functions which use an analysis
   report and we do not wish to compute it several times. Additionally here
   we wish to make sure that analysis has already occurred and accepting the
   report as an argument ensure that at least the analysis has occurred if
   maybe the results have been ignored.

   ARGH: unfortunately the current analysis only tells whether a rate is used
   based on whether or not it appears in the model and *not* whether or not
   that process is used. Hence unfortunately we will have to re-analyse
   the model after removing the unused processes, sucks but then, what
   can you do?
-}
removeUnusedDefinitions :: AnalysisReport -> ParsedModel -> ParsedModel
removeUnusedDefinitions report model =
  interModel { modelRateSpecs = newRateDefs }
  where
  -- The interModel is the original model with the unused process
  -- definitions removed. We must re-analyse this to find out which
  -- rates are not used.
  interModel = model { modelProcessDefs = newProcessDefs }
  pNames     = usedProcessNames report

  reReport   = analyseModel interModel
  rNames     = usedRateNames reReport

  newProcessDefs = filter ((flip elem pNames) . fst) $ modelProcessDefs model
  newRateDefs    = filter ((flip elem rNames) . fst) $ modelRateSpecs   model


{- Removing the virtual components is quite simple, we can of course
   just set the virtual component definitions to zero. We must also
   return a virtual map. In addition we are currently not allowing
   virtual components to be used in functional rates but we probably
   should. In such a case we would have to reduce all the rate expressions.
   If however we did that, I would probably recommend doing so at the
   same time as qualifying the model.

   UPDATE: yes I'm now actually doing this, in that I'm modifying all
   the rates in the model using the virtual map.
-}
removeVirtualComponents :: ParsedModel -> (ParsedModel, NameRateMap)
removeVirtualComponents model
  | null virtualComps = (model, Map.empty)
  | otherwise         = ( modifiedModel { modelVirtualComps = [] }
                        , virtualMap
                        )
  where
  modifiedModel  = modifyModelExpressions modifyRateExpr model
  modifyRateExpr = remapRateExpr virtualMap
  virtualMap     = Map.fromList $ map (first Qualified.textual) virtualDefs
  virtualDefs    = reduceConstantMappings virtualComps
  virtualComps   = modelVirtualComps model

{- The type used to record which process definitions we have "seen" -}
type Seen = [ ParsedComponentId ]

{-|
   The idea of qualifying a model is to take a model and make unique
   every occurrence of a process identifier in the main composition.
   So for example something like, @ P <> P @, becomes
   @ P_1 <> P_2 @.
   This has consequences for later looking it up in the process
   definition list of course.
   
   Note: once we re-introduce parallel definitions, we will have to
   work out how to deal with those, perhaps we just compile them
   out before hand?
   
   Also I think it would be desirable to leave as unqualified the first
   occurrence so that in the case that there is only one occcurrence of
   any process we do not do any qualification.

   It is important to note that there are one or two functional
   dependencies on this model. First of all there are no aliases
   or parallel definitions. Therefore any used process name we
   can assume is a sequential definition.

   UPDATE: I really would like to redo this in a cleaner way
   in particular I only want to qualify a name where it is necessary.
   However be careful, any attempt to do so must make sure to cover
   the case that a process derivative is used, that is:
   P = a.Q ;
   Q = a.P ;
   P <> Q
   Here we may think that Q does not need to be qualified but it does.
-}
qualifyModel :: ParsedModel -> (ParsedModel, Map String RateExpr)
qualifyModel model =
  (fullyQualified, dirtyMapping)
  where
  -- Dirty mapping is the mapping from the original names used in the
  -- model to rate expressions which now refer to these.
  dirtyMapping   = Map.map gatherNames qualifiedMapping
  gatherNames :: [ ParsedComponentId ] -> RateExpr
  gatherNames [] = Cconstant 0
  gatherNames l  = foldr1 Cadd $ map Cident l
  -- use this mapping to go through and change all the 
  -- transitions in the model.
  dirtyQualified = model { modelProcessDefs = qualifiedDefs
                         , modelSystemEqn   = qualifiedSystem
                         }
  fullyQualified = modifyModelExpressions modifyRateExpr dirtyQualified
  modifyRateExpr = remapRateExpr dirtyMapping

  -- We first qualify the model, by making sure that each process name
  -- is unique. However this will place the model in a 'bad' state because
  -- all of functional rates involving a process name intended to refer
  -- to all instances of that process name now only refer to the first.
  -- We must therefore be careful to update all rates and conditions.
  (qualifiedMapping,
   qualifiedSystem,
   qualifiedDefs) = qualifyPar Map.empty $ modelSystemEqn model
    

  -- Qualify an identifier with the given qualifying number.
  -- If that number is zero then we needn't do any qualification.
  qualifyIdentifier :: Int -> ParsedComponentId -> ParsedComponentId
  qualifyIdentifier 0 ident = ident
  qualifyIdentifier n ident = qualifyQName ident n


  -- Qualifying a parallel process requires that we maintain a mapping
  -- from names to the number of their occurrences we have thus far
  -- seen. This mapping is returned as we must accumulate the mapping
  -- for each branch of a coopeartion. Instead of mapping to a number
  -- we map to a list of qualified instances of the name as the returned
  -- mapping can therefore be used to create the mapping from original
  -- names to rate expressions used to update rate expressions and
  -- conditions.
  qualifyPar :: Map String [ ParsedComponentId ]
             -> ParsedComponent
             -> ( Map String [ ParsedComponentId ]
                , ParsedComponent
                , [ ProcessDef ])
  qualifyPar currentMapping (IdProcess ident)                 =
    (newMapping, newComponent, newDefs)
    where
    newComponent                   = IdProcess newName
    (newMapping, newDefs, newName) = qualifyProcess currentMapping ident
  qualifyPar currentMapping (Cooperation left actions right)  =
    (newMapping, newComponent, newDefs)
    where
    newMapping                          = rightMapping
    newComponent                        = Cooperation newLeft actions newRight
    newDefs                             = leftDefs ++ rightDefs
    (leftMapping, newLeft, leftDefs)    = qualifyPar currentMapping left
    (rightMapping, newRight, rightDefs) = qualifyPar leftMapping right
  qualifyPar currentMapping (ProcessArray comp size mActions) = 
    (newMapping, newComponent, newDefs)
    where
    newComponent                   = ProcessArray newComp size mActions
    (newMapping, newComp, newDefs) = qualifyPar currentMapping comp
  qualifyPar currentMapping (Hiding comp actions)             = 
    (newMapping, newComponent, newDefs)
    where
    newComponent                   = Hiding newComp actions
    (newMapping, newComp, newDefs) = qualifyPar currentMapping comp
  qualifyPar _currentMapping (PrefixComponent _ _)            = 
    error "Sequential component found in system equation"
  qualifyPar _currentMapping (ComponentSum _ _)               = 
    error "Sequential component found in system equation"
  qualifyPar _currentMapping  (CondBehaviour _ _)             = 
    error "Sequential component found in system equation"
  qualifyPar _currentMapping (StopProcess)                    = 
    error "Sequential component found in system equation"
    
  -- To qualify a given process we must first gather up all the definitions
  -- that it uses. We then check if any of these names need to be qualified
  -- (that is, are they in the dirty mapping). If so then we qualify the
  -- whole lot. Otherwise we just place them into the dirty mapping
  -- and use the definitions as is.
  qualifyProcess :: Map String [ ParsedComponentId ]
                 -> ParsedComponentId
                 -> ( Map String [ ParsedComponentId ]
                    , [ ProcessDef ]
                    , ParsedComponentId )
  qualifyProcess currentMapping startId
    | num == 0  = ( newMapping, usedDefs, startId)
    | otherwise = ( newMapping, newDefs, newStartId)
    where
    newMapping = foldl (flip insertName) currentMapping usedIds 
    insertName :: ParsedComponentId -> Map String [ ParsedComponentId ]
                                    -> Map String [ ParsedComponentId ]
    insertName ident =
      Map.insertWith (++) (Qualified.textual ident)
                          [ qualifyIdentifier num ident ]
    usedDefs  = PepaUtils.reachableProcessDefs model startId
    usedIds   = map fst usedDefs
    usedNames = map Qualified.textual usedIds
    num       = sum $ map getQualification usedNames
    getQualification :: String -> Int
    getQualification name = maybe 0 length $ Map.lookup name currentMapping

    newStartId = qualifyIdentifier num startId
    newDefs    = map qualifyDefinition usedDefs

    qualifyDefinition :: ProcessDef -> ProcessDef
    qualifyDefinition (defName, defComp) = 
      ( qualifyIdentifier num defName
      , qualifySequential defComp )
    qualMapping = Map.fromList $ map makeEntry usedIds
    makeEntry :: ParsedComponentId -> (ParsedComponentId, ParsedComponentId)
    makeEntry ident = (ident, qualifyIdentifier num ident)

    qualifySequential :: ParsedComponent -> ParsedComponent
    qualifySequential (IdProcess ident)
      | Just newIdent <- Map.lookup ident qualMapping  = IdProcess newIdent
      | otherwise                                     = IdProcess ident
    qualifySequential (PrefixComponent trans next)    =
      PrefixComponent trans $ qualifySequential next
    qualifySequential (ComponentSum left right)       =
      ComponentSum (qualifySequential left) 
                   (qualifySequential right)
    qualifySequential (CondBehaviour rExpr comp)      =
      CondBehaviour rExpr $ qualifySequential comp
    qualifySequential (StopProcess)                   = 
      StopProcess
    qualifySequential (Cooperation _ _ _)             =
      error "Parallel component found in sequential definition: after simplify"
    qualifySequential (ProcessArray _ _ _)            =
      error "Parallel component found in sequential definition: after simplify"
    qualifySequential (Hiding _ _)                    =
      error "Parallel component found in sequential definition: after simplify"

{-
   (fullyQualified, dirtyMapping) -- defined near the end of this function
   where
   -- The dirtyQualified model is one which has qualified all
   -- the definitions but has not updated the transition conditions
   -- and rates to reflect this.
   dirtyQualified  = model { modelProcessDefs = qualifiedDefs
                           , modelSystemEqn   = newSystem
                           }
   (_id, 
    newSystem,
    qualifiedDefs) = qualifyPar 0 $ modelSystemEqn model
   processDefs     = modelProcessDefs model
   qualifyPar :: Int -> ParsedComponent
             -> (Int, ParsedComponent, [ ProcessDef ])
   qualifyPar idNum (ProcessArray comp size mActions)  =
      (idNum1, ProcessArray newComp size mActions, newDefs)
      where
      (idNum1, newComp, newDefs) = qualifyPar idNum comp
   qualifyPar idNum (IdProcess ident)                  =
      (idNum + 1, newPar, newDefs)
      where
      (_seen, newDefs) = qualifyProcess idNum [ ident ] ident
      newIdent         = qualifyQName ident idNum
      newPar           = IdProcess newIdent
   qualifyPar idNum (Cooperation left actions right)   =
      (idNum2, newCoop, leftDefs ++ rightDefs)
      where
      newCoop  = Cooperation newLeft actions newRight
      (idNum1, newLeft, leftDefs)   = qualifyPar idNum left
      (idNum2, newRight, rightDefs) = qualifyPar idNum1 right
   qualifyPar idNum (Hiding hComp actions)              =
      (idNum1, Hiding newHComp actions, newDefs)
      where
      (idNum1,  newHComp, newDefs) = qualifyPar idNum hComp
   qualifyPar _idNum StopProcess                        =
      error "Sequential component found in parallel composition"
   qualifyPar _idNum (PrefixComponent _ _)              =
      error "Sequential component found in parallel composition"
   qualifyPar _idNum (ComponentSum _ _)                 =
      error "Sequential component found in parallel composition"
   qualifyPar _idNum (CondBehaviour _ _)                =
      error "Sequential component found in parallel composition"   


    {-
      Qualify process must search in the *original* definition list
      for the name we are qualifying and return the list of
      qualified definitions. It doesn't need to return the list
      of seen process names (*I think*) because because if there
      is a recursive definition the name is added before we do
      the rest of the definition.

      Also note that it doesn't *generate* names, this is because
      all we are doing is applying the current renaming to the
      processes. However once parallel definitions are allowed this
      would change, since in the case of a parallel definition the
      process identifiers contained therein would have to be given
      new names (as they are essentially just new occurences within
      the main composition).
    -}
   qualifyProcess :: Int -> Seen -> ParsedComponentId 
                 -> (Seen, [ ProcessDef ])
   qualifyProcess idNum seen ident =
      (newSeen, newProcessDef : newDefs)
      where 
      newProcessDef               = (newIdent, newSequ)
      newIdent                    = qualifyQName ident idNum
      (newSeen, newSequ, newDefs) = qualifySeqWith idNum seen process
      process                     = maybe err id $ 
                                    lookup ident processDefs
      err                         = 
        error $ "There should be no undefined processes: " ++
                (Qualified.hprintQualifiedName ident)



    {-
      Qualifying a definition does not return an integer, because
      we want to qualify all the names within the definition with
      the same integer. We are not at this point introducing names.

      Note: this would not be the case for parallel definitions.
      
      We require the integer with which to qualify all of the names
      and also the names of the definitions we have already seen
      in order that we do not go into a loop.

      Note also that we are currently not doing anything with rate
      definitions, but since these should ultimately contain
      process names within functional rates we definitely should.
      *What* we should do is not entirely obvious.
    -}
 
   qualifySeqWith :: Int -> [ParsedComponentId] -> ParsedComponent
                  -> (Seen, ParsedComponent, [ ProcessDef ])
   qualifySeqWith idNum seen (IdProcess ident)
      | elem ident seen = ( seen, sequ, [])
      | otherwise       = ( newSeen, sequ, newDefs )
      where
      sequ               = IdProcess $ qualifyQName ident idNum
      tmpSeen            = ident : seen
      (newSeen, newDefs) = qualifyProcess idNum tmpSeen ident
   qualifySeqWith idNum seen (PrefixComponent trans sequ) =
      (newSeen, newPrefix, newDefs)
      where
      {- Note here we do nothing with the trans, might have to
      when we introduce functional rates -}
      (newSeen, newSequ, newDefs) = qualifySeqWith idNum seen sequ
      newPrefix = PrefixComponent trans newSequ
   qualifySeqWith idNum seen (ComponentSum left right) =
      (seenRight, newSum, leftDefs ++ rightDefs)
      where 
      newSum = ComponentSum newLeft newRight
      (seenLeft,  newLeft,  leftDefs)  = qualifySeqWith idNum seen left
      (seenRight, newRight, rightDefs) = qualifySeqWith idNum seenLeft right

   qualifySeqWith _idNum seen proc@StopProcess             =
      (seen, proc, [])
   qualifySeqWith idNum seen (CondBehaviour cond proc)     =
      (newSeen, CondBehaviour cond newP, newDefs)
      where
      (newSeen, newP, newDefs) = qualifySeqWith idNum seen proc
   qualifySeqWith _idNum _seen (Cooperation _ _ _)         =
      error "parallel definition (coop) found while qualifying the model"
   qualifySeqWith _idNum _seen (Hiding _ _ )               =
      error "parallel definition (hiding) found while qualifying the model"
   qualifySeqWith _idNum _seen (ProcessArray _ _ _)        =
      error "parallel definition (process array) found while qualifying the model"


   {-
     UPDATE: Now I return from this the mapping so the measurement 
     specifications can be updated.
     UPDATE: yes we do, and we SHOULD also update the measurement specifications
     but for now we just have two different ways of evaluating conditions one
     for transitions conditions which we assume have already been qualified
     and another for measurement specifications which we assume are not.
     UPDATE: we don't actually do this now because in 'States' we actually
     assume that all conditions and rate expressions are talking about
     unqualified names and just match on the original name. Probably this
     is the better way to do it, however if we do it this way then
     we will have to update the state measurements here as well!!
     That is, all of the simplify functions (or at least this one) should
     take in the measurements and modify those as well since those will
     speak about unqualified names. The downside to this is that you cannot
     then post-hoc give a measurement specification.

     Having qualified a model we must change transition conditions and
     transition (functional rates) to note the new change.
     So for example the expression @if P > 0 then r else r1@ must become
     something of the form
     @if (P_1 + P_2 + P_3) > 0 then r else r1@
     where @P_1@, @P_2@ and @P_3@ represent all the instances of @P@
     in the newly qualified model.
   -}
   -- get the used names of the newly qualified model.
   dirtyUsed    = usedProcessNames $ analyseModel dirtyQualified
   -- Create a mapping based on those used names
   dirtyMapping = foldl insertName Map.empty dirtyUsed
   insertName :: Map String RateExpr -> ParsedComponentId -> Map String RateExpr
   insertName m ident = 
     Map.insertWith addRateExprs (showOrig ident) (namedRateExp ident) m

   -- use this mapping to go through and change all the 
   -- transitions in the model.
   fullyQualified = modifyModelExpressions modifyRateExpr dirtyQualified
   modifyRateExpr = remapRateExpr dirtyMapping
-}

{-| Modifies all the expressions (which are all rate expressions even
    when not used as a rate) in a model
-}
modifyModelExpressions :: (RateExpr -> RateExpr) -> ParsedModel -> ParsedModel
modifyModelExpressions modifyExpr model =
  model { modelRateSpecs    = map modifyRateSpec $ modelRateSpecs model
        , modelProcessDefs  = map modifyProcessDef $ modelProcessDefs model
        , modelVirtualComps = map modifyVirtualSpec $ modelVirtualComps model
        -- Note we leave the main system equation untouched as it should
        -- contain no expressions
        }
  where
  modifyRateSpec    = second modifyExpr
  modifyVirtualSpec = second modifyExpr
  modifyProcessDef  = second modifyProcess

  modifyProcess :: ParsedComponent -> ParsedComponent
  modifyProcess proc@(StopProcess)            = proc
  modifyProcess proc@(IdProcess _)            = proc
  modifyProcess (PrefixComponent trans nextP) =
    PrefixComponent (modifyTrans trans) $ modifyProcess nextP
  modifyProcess (CondBehaviour cond left)     =
    CondBehaviour cond $ modifyProcess left
  modifyProcess (ComponentSum left right)     =
    ComponentSum (modifyProcess left) (modifyProcess right)
  modifyProcess (Cooperation _ _ _)           =
    error "parallel defs should have been removed before qualification"
  modifyProcess (ProcessArray _ _ _)          =
    error "parallel defs should have been removed before qualification"
  modifyProcess (Hiding __ _)                 =
    error "parallel defs should have been removed before qualification"

  modifyTrans :: ParsedTrans -> ParsedTrans
  modifyTrans tr =
   tr { pepaTransConditions = newConds
      , pepaTransRate       = newRate
      }
   where
   newRate    = modifyRate modifyExpr $ pepaTransRate tr
   newConds   = map modifyExpr $ pepaTransConditions tr

{-| 
  Allows the modification of each transition in a pepa model given by
  a transition function.
  Note: if you are using this to work on expressions then the expression
  in a 'CondBehaviour' remains untouched, therefore either do these separately
  or we require a function to operatate on all of a model's expressions.
  Alternatively you could use a model transformation to remove all conditional
  behaviour first. Recall also though there are virtual definitions
-}
modifyModelTransitions :: (ParsedTrans -> ParsedTrans) -> ParsedModel -> ParsedModel
modifyModelTransitions modifyTrans model =
  model { modelProcessDefs = newProcessDefs }
  where
  newProcessDefs    = map modifyProcessDef $ modelProcessDefs model
  modifyProcessDef  = second modifyProcess

  modifyProcess :: ParsedComponent -> ParsedComponent
  modifyProcess proc@(StopProcess)            = proc
  modifyProcess proc@(IdProcess _)            = proc
  modifyProcess (PrefixComponent trans nextP) =
    PrefixComponent (modifyTrans trans) $ modifyProcess nextP
  modifyProcess (CondBehaviour cond left)     =
    CondBehaviour cond $ modifyProcess left
  modifyProcess (ComponentSum left right)     =
    ComponentSum (modifyProcess left) (modifyProcess right)
  modifyProcess (Cooperation _ _ _)           =
    error "parallel defs should have been removed before qualification"
  modifyProcess (ProcessArray _ _ _)          =
    error "parallel defs should have been removed before qualification"
  modifyProcess (Hiding __ _)                 =
    error "parallel defs should have been removed before qualification"


{- The type of the action map, mapping action identifiers to new identifiers -}
type Mapping a b = [ (a, b) ]
type ActionMap = Mapping ActionIdentifier ActionIdentifier

{-|
   The next transformation removes hiding from the model by renaming
   the actions. Note that this must be performed on a model which
   has already been qualified.

   For this reason we provide a function to remove hiding from an unqualifed
   model. Note that this simplfy calls 'qualifyModel' first.
   Perhaps later if we wish to do pepa->pepa translations which remove
   hiding then we can worry about doing this while only qualifying what
   is necessary.
-}
removeHidingUnqualifiedModel :: ParsedModel -> ParsedModel
removeHidingUnqualifiedModel = removeHiding . fst . qualifyModel


{-|
   Removing hiding from a qualified model. This if fairly simple
   we need only rename those actions which are hidden we do not
   need to create new definitions only modify current ones.
   Since each definition is only used by one process so the actions
   which may be performed by each defined process are either hidden
   or not. (Not for example hidden in one use and not in another.)
-}
removeHiding :: ParsedModel -> ParsedModel
removeHiding model =
   model { modelProcessDefs = hiddenDefs
         , modelSystemEqn   = newMainComp
         }
   where
   -- rateDefs    = modelRateSpecs model
   processDefs = modelProcessDefs model
   mainComp    = modelSystemEqn model
   (_ns, hiddenDefs, newMainComp) = hideComp initialNameSupply [] [] mainComp

   hideComp :: NameSupply -> ActionMap -> Seen -> ParsedComponent
            -> ( NameSupply, [ ProcessDef ], ParsedComponent )
   hideComp names _aMap _seen proc@StopProcess                 =
      (names, [], proc)
   hideComp names aMap seen proc@(IdProcess ident)
      | elem ident seen = (names, [], proc)
      | otherwise       = 
         case lookup ident processDefs of
            Nothing -> error $ concat [ "Undefined process detected "
                                      , "while removing hiding: "
                                      , Qualified.textual ident
                                      , "\n\""
                                      , show ident
                                      , "\"\n"
                                      , show seen
                                      ]
            Just p  -> 
               ( newNs, newDef : newDefs, proc)
               where
               newDef                  = (ident, newP)
               (newNs, newDefs, newP) = hideComp names aMap (ident : seen) p

   hideComp names aMap seen (PrefixComponent trans next)       =
      (newNs, newDefs, newPrefix)
      where
      (newNs, newDefs, newNext) = hideComp names aMap seen next
      newPrefix                  = PrefixComponent newTrans newNext
      newTrans                   = trans { pepaTransAction = newAction }
      newAction                  = hideAction aMap $ pepaTransAction trans
   hideComp names aMap seen (ComponentSum left right)          =
      ( newNsRight
      , unionBy equalDefs leftDefs rightDefs
      , ComponentSum newLeft newRight
      )
      where
      (newNsLeft, leftDefs, newLeft)    = hideComp names aMap seen left
      (newNsRight, rightDefs, newRight) = hideComp newNsLeft aMap seen right

      -- Although the model has been qualified a component sum may
      -- still have the same component in either half, consider
      -- @ P = (a, r).Q + (b, r) . Q @
      -- if we did the simple thing of hiding the left and right of the
      -- component sum we would duplicate the definitions for Q.
      -- Hence the final result above uses 'unionBy' and here we just
      -- define to definitions as equal if they defined the same process.
      equalDefs :: ProcessDef -> ProcessDef -> Bool
      equalDefs = Function.on (==) fst

   hideComp names aMap seen (Cooperation left coopSet right)  =
      ( newNsRight
      , leftDefs ++ rightDefs
      , Cooperation newLeft newCoopSet newRight
      )
      where
      newCoopSet = 
        case coopSet of
          ActionSet actions -> ActionSet $ map (lookupParsedAction aMap) actions
          WildCard          -> WildCard
      (newNsLeft, leftDefs, newLeft)    = hideComp names aMap seen left
      (newNsRight, rightDefs, newRight) = hideComp newNsLeft aMap seen right
   hideComp names aMap seen (ProcessArray comp size actions)  =
     ( newNS, newDefs, newP )
     where
     newP                   = ProcessArray proc size newActions
     (newNS, newDefs, proc) = hideComp names aMap seen comp
     newActions             = liftM (map $ lookupParsedAction aMap) actions
   hideComp names aMap seen (CondBehaviour cond proc)          =
      (newNs, newDefs, CondBehaviour cond newP)
      where
      (newNs, newDefs, newP) = hideComp names aMap seen proc
   {- The case for actual hiding -}
   hideComp names aMap seen (Hiding comp actions)              =
      hideComp newNames newActionMap seen comp
      where
      newActionMap    = snd newNamesAndAMap
      newNames        = fst newNamesAndAMap
      newNamesAndAMap =foldl addParsedAction (names, aMap) actions
      addParsedAction :: (NameSupply, ActionMap) -> ParsedAction
                      -> (NameSupply, ActionMap)
      addParsedAction (nSupply, m) (Action ident)    = 
         (newSupply, newM)
         where
         newM                 = (ident, newIdent) : m
         newIdent             = Unqualified newName
         (newSupply, newName) = getName nSupply $ "tau__" ++ (showOrig ident)
      -- Currently com actions are treated the same, but we may wish to
      -- say that you shouldn't be hiding a communication action??
      addParsedAction (nSupply, m) (ComAction ident) = 
         (newSupply, newM)
         where
         newM                 = (ident, newIdent) : m
         newIdent             = Unqualified newName
         (newSupply, newName) = getName nSupply $ "tauCA__" ++ (showOrig ident)
      addParsedAction _m (Tau _ )                    =
         error "A tau action in a hide set? shouldn't happen"

   {- These functions are merely for looking up the actions in an
      action map and are essentially just wrappers around the action
      data types (and the maybe).
   -}
   hideAction :: ActionMap -> ParsedAction -> ParsedAction
   hideAction m (Action    ident) = Action   $ lookupAction ident m
   hideAction m (Tau       ident) = Tau      $ lookupAction ident m
   hideAction m (ComAction ident) = ComAction $ lookupAction ident m

   lookupParsedAction :: ActionMap -> ParsedAction -> ParsedAction
   lookupParsedAction actionMap (Action    ident) = 
      Action    $ lookupAction ident actionMap
   lookupParsedAction actionMap (Tau       ident) =
      Tau       $ lookupAction ident actionMap
   lookupParsedAction actionMap (ComAction ident) =
      ComAction $ lookupAction ident actionMap

   lookupAction :: ActionIdentifier -> ActionMap -> ActionIdentifier
   lookupAction ident =
    fromMaybe ident . lookup ident



{-
  If you call this initially with the empty mapping as the canonMapping
  then the second argument will be reduced until it's all just constant
  mappings or we detect that there is a loop.

  Question is my logic correct here, in that if we do not reduce the
  number of equation on any single run then there is no hope to do so?

  NOTE: This should be stress tested quite a bit, I'm not sure what happens
  if we have cyclic definitions for example.
-}
reduceConstantMappings :: [ (QualifiedName, RateExpr) ]
                       -> [ (QualifiedName, RateExpr) ]
reduceConstantMappings originalDefs =
  iterReduce [] originalDefs
  where
  normaliseRateDefs = map . (second . normaliseRateExpr) 

  iterReduce :: [ (QualifiedName, RateExpr) ]
          -> [ (QualifiedName, RateExpr) ]
          -> [ (QualifiedName, RateExpr) ]
  iterReduce canonMapping origMapping
    | null origMapping = canonMapping
    | null noncanon    = newCanonMapping
    -- Okay not quite, if we have functional rates as a definition
    -- then this should be allowed.
    --    | null canon       = error "constant expressions cannot be reduced"
    | null canon       = canonMapping ++ noncanon
    | otherwise        = iterReduce newCanonMapping noncanon
    where
    newCanonMapping    = canonMapping ++ canon
    reducedOrigMapping = normaliseRateDefs canonMapping origMapping
    (canon, noncanon)  = partition (nonReducible . snd) reducedOrigMapping
    nonReducible :: RateExpr -> Bool
    nonReducible = Rates.isIrreducible $ map fst originalDefs

    
