{-| 
   This module implements a transformation in which all needlessly
   exported actions are hidden. So if a component within a model
   performs an action which it never cooperates over then it is
   hidden.
-}
module Language.Pepa.Transform.HideAll
   ( hideAllActions )
where

{- Standard Libraries modules imported -}
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set
  ( Set )
{- External Library modules imported -}
{- Local modules imported imported -}
import Language.Pepa.Syntax
  ( ParsedModel       ( .. )
  , ParsedComponent   ( .. )
  , CooperationSet    ( .. )
  , ActionIdentifier
  , ParsedAction
  , nameOfAction
  )
import Language.Pepa.Analysis.Analysis
  ( AnalysisReport    ( .. )
  , componentPerformsActions
  )
{- End of imports -}


{-|
   The main function exported by the model.
   It expects there to be no parallel definitions as the definitions
   are left unchanged. This is a simple transformation over the main
   composition which will simply turn process identifiers into
   a hiding component. Such as @P => P\/{a,b}@.
-}
hideAllActions :: AnalysisReport -> ParsedModel -> ParsedModel
hideAllActions report model =
   model { modelSystemEqn = hideComponent Set.empty $ modelSystemEqn model }
   where
   hideComponent :: Set ActionIdentifier -> ParsedComponent -> ParsedComponent
   hideComponent coopSet component@(IdProcess ident)          =
      case Map.lookup ident actionMap of
         Nothing   -> error "process not in the analysis's action map"
         Just aSet -> 
            case createHideSet coopSet aSet of
               [] -> component
               l  -> Hiding component l
   hideComponent coopSet (Cooperation left actions right)     =
      Cooperation (hideComponent newCoopSet left)
                  actions
                  (hideComponent newCoopSet right)
      where
      newCoopSet  = Set.union coopSet $ Set.map nameOfAction coopActions
      coopActions =  
        case actions of
          (ActionSet a) -> Set.fromList a
          (WildCard)    -> Set.intersection leftActions rightActions
      leftActions  = componentPerformsActions actionMap left
      rightActions = componentPerformsActions actionMap right
   hideComponent coopSet (Hiding comp actions)                =
      Hiding (hideComponent newCoopSet comp) actions
      where
      newCoopSet = Set.union coopSet $ Set.fromList $ map nameOfAction actions
   -- Process arrays are actually just the same as identifiers
   hideComponent coopSet (ProcessArray comp size Nothing)     =
     ProcessArray (hideComponent coopSet comp) size Nothing
   hideComponent coopSet (ProcessArray comp size (Just acts)) =
     ProcessArray (hideComponent newCoopSet comp) size $ Just acts
     where
     newCoopSet = Set.union coopSet $ Set.fromList (map nameOfAction acts)
   -- The sequential components which should not be in the main
   -- composition
   hideComponent _coopSet (PrefixComponent _ _)               =
      error "hideAllActions: sequential component found in main composition"
   hideComponent _coopSet (StopProcess)                       =
      error "hideAllActions: sequential component found in main composition"
   hideComponent _coopSet (CondBehaviour _ _)                 =
      error "hideAllActions: sequential component found in main composition"
   hideComponent _coopSet (ComponentSum _ _)                  =
      error "hideAllActions: sequential component found in main composition"

   -- Takes in the cooperation set and the set of actions performed by
   -- a component and returns the set of actions which should be hidden.
   createHideSet :: Set ActionIdentifier -> [ ParsedAction ] -> [ ParsedAction ]
   createHideSet coopSet =
      filter ( not . ((flip Set.member coopSet) . nameOfAction) )

   actionMap = processActionsMap report