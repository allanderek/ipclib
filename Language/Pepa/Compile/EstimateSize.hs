module Language.Pepa.Compile.EstimateSize
  ( estimatePepaSize )
where

{- Standard Library Modules Imported -}
import Control.Monad
  ( liftM )
import qualified Data.Map as Map
import Data.Map
  ( Map )
import qualified Data.List as List
{- External Library Modules Imported -}
{- Local Modules Imported -}
import Language.Pepa.Rates
  ( rateExpressionInt ) -- from process array sizes
import Language.Pepa.Syntax
  ( ParsedModel     ( .. )
  , ParsedComponent ( .. )
  , CooperationSet  ( .. )
  , ParsedComponentId
  , ParsedTrans
  , ActionIdentifier
  , nameOfAction
  , actionNameOfTrans
  )
import qualified Language.Pepa.MainControl as MainControl
import Language.Pepa.MainControl
  ( MainControl )
import Language.Pepa.PepaUtils
  ( possibleTransitions
  , transitiveDerivatives
  )
{- End of Module Imports -}


{-
  This should be parameterised over the type of leaf-component.
  If this is done then it may even be possible to use this in the
  actual representation of a PEPA model.
-}
data System = Coop  System [ ActionIdentifier ] System
            | Array System Int [ ActionIdentifier ]
            | Component [ ParsedComponentId ] [ ParsedTrans ]


{-|
  The main exported function. Estimates the state size of a model by
  first compiling to a timed system equation
  (a computation inexpensive transformation)
  NOTE: this should probably return a MainControl.
-}
estimatePepaSize :: ParsedModel -> MainControl Int
estimatePepaSize =
  liftM estimateSystem . pepaToSystem

actionsOfSystem :: System -> [ ActionIdentifier ]
actionsOfSystem (Coop left _ right) = List.union (actionsOfSystem left)
                                                 (actionsOfSystem right)
actionsOfSystem (Array left _ _)    = actionsOfSystem left
actionsOfSystem (Component _ trans) = map actionNameOfTrans trans

{-
  Turn a PEPA model into a 'System'
-}
pepaToSystem :: ParsedModel -> MainControl System
pepaToSystem model =
  MainControl.valueResult system "estimate-system" logInformation
  where
  logInformation = hprintSystem system

  system         = translateSystem $ modelSystemEqn model

  translateSystem :: ParsedComponent -> System
  translateSystem (IdProcess ident)                   = 
    translateIdent ident
  translateSystem (ProcessArray comp size mActions)   =
    Array (translateSystem comp) (rateExpressionInt size) actions
    where
    actions = maybe [] (map nameOfAction) mActions
  translateSystem (Cooperation left coopSet right)    =
    Coop tLeft actions tRight
    where
    tLeft   = translateSystem left
    tRight  = translateSystem right
    actions = case coopSet of
                ActionSet a -> map nameOfAction a
                WildCard    -> List.union (actionsOfSystem tLeft)
                                          (actionsOfSystem tRight)
  translateSystem (Hiding _p _actions)                =
    error "Hiding should have been removed from the model before now"
  translateSystem (PrefixComponent _ _)               =
    error $ "pepaToTimedSystemEqn: prefix components " ++ badEqnError
  translateSystem (ComponentSum _ _)                  =
    error $ "pepaToTimedSystemEqn: component sums " ++ badEqnError
  translateSystem (CondBehaviour _ _)                 =
    error $ "pepaToTimedSystemEqn: conditional behaviour " ++ badEqnError
  translateSystem (StopProcess)                       =
    error $ "pepaToTimedSystemEqn: stop processes " ++ badEqnError

  translateIdent :: ParsedComponentId -> System
  translateIdent ident =
    Component derives transitions
    where
    derives     = List.nub $ ident : (transitiveDerivatives model ident)
    transitions = concatMap (possibleTransitions model) derives

  badEqnError = "should not be in the main system equation"


estimateSystem :: System -> Int
estimateSystem system =
  fst $ estimate system
  where
  estimate :: System -> (Int, Map ActionIdentifier Int)
  estimate (Coop left actions right) =
    ( calculatedSize
    , transMap
    )
    where
    calculatedSize
      | coopSize <= 1 = possible
      | otherwise     = calculated

    -- Calculated size must be no bigger than the possible
    calculated   = min possible analysed

    -- This is the largest state space possible
    possible     = sizeLeft * sizeRight

    -- The analysed size, this is our main idea about how to
    -- calculate the size, the rest is mostly just handling
    -- corner cases.
    analysed     = adjust + (leftFactor * rightFactor)

    adjustLeft   = sizeLeft * (createAdjust $ Map.foldWithKey addAction (0,0) mapLeft)
    adjustRight  = sizeRight * (createAdjust $ Map.foldWithKey addAction (0,0) mapRight)

    createAdjust (coops, allTrans) = div coops allTrans

    addAction :: ActionIdentifier -> Int
              -> (Int, Int) -> (Int, Int)
    addAction a i (coops, allTrans)
      | elem a actions = (coops + i, allTrans + i)
      | otherwise      = (coops, allTrans + i)

    adjust       = max adjustLeft adjustRight

    -- The total number of estimated transitions that can be done
    -- by the whole cooperation.
    -- totalTrans   = Map.fold (+) 0 transMap

    -- The new map which we add return mapping the action names to the
    -- number of transitions involving them.
    transMap     = Map.unionWithKey joinActions mapLeft mapRight

    joinActions :: (ActionIdentifier -> Int -> Int -> Int)
    joinActions action
      | elem action actions = (*)
      | otherwise           = (+)


    -- Note these are only used in the case that coopSize is at least 2
    -- since otherwise it is just the product of the two.
    leftFactor   = max 1 $ sizeLeft - adjustLeft
    rightFactor  = max 1 $ sizeRight - adjustRight

    coopSize     = length actions
    (sizeLeft,
     mapLeft)    = estimate left
    (sizeRight,
     mapRight)   = estimate right

  estimate (Array _ _ _)             =
    error "Not handling arrays in the estimation as of yet"
  estimate (Component derivatives transitions) =
    ( length derivatives, transMap )
    where
    -- The transition map, maps each action identifier to the number of
    -- transitions which perform this action.
    transMap     = Map.fromList $ map groupToKeyPair actionGroups

    -- All of the actions, note that this (intentionally) includes duplicates
    -- we are just taking the name of the action from each transition
    allActions   = map actionNameOfTrans transitions
    -- Group together into lists the action identifiers, so each list is
    -- a list of identical action identifiers, the length of each such list
    -- is how many transitions involve the given action identifier.
    actionGroups = List.group allActions
    
    groupToKeyPair :: [ ActionIdentifier ] -> (ActionIdentifier, Int)
    groupToKeyPair [] = error "An empty action group, impossible"
    groupToKeyPair h  = (head h, length h)


{-
  Clearly not in any way the correct thing to do but it will get us started

estimateTimedSize :: Timed.TimedSystemEquation -> Int
estimateTimedSize (TSEderivatives derivs _actions)    = length derivs
estimateTimedSize (TSEcooperation left actions right)
  | coopSize <= 1  = possible
  | otherwise      = calculated
  where
  -- Calculated size must be no bigger than the possible
  calculated  = min possible analysed

  -- This is the largest state space possible
  possible    = sizeLeft * sizeRight

  -- The analysed size, this is our main idea about how to
  -- calculate the size, the rest is mostly just handling
  -- corner cases.
  analysed    = adjust + (leftFactor * rightFactor)

  -- Note these are only used in the case that coopSize is at least 2
  -- since otherwise it is just the product of the two.
  leftFactor  = max 1 $ sizeLeft - adjust
  rightFactor = max 1 $ sizeRight - adjust
  adjust      = coopSize - 1

  coopSize    = length actions
  sizeLeft    = estimateTimedSize left
  sizeRight   = estimateTimedSize right
-}


hprintSystem :: System -> String
hprintSystem _s = "Haven't gotten around to that yet."