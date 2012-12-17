{-|
   A module for converting Pepa models to prism models.
-}

module Language.Pepa.Compile.Prism
   ( prismFromPepa
   , makePrismFile
   , getPrismExplictModel
   )
where

{- Standard Library Imports -}
import qualified Data.Map as Map
{- External Library Imports -}
{- Local Module Imports -}
import Language.Pepa.Rates
  ( rateNumber )
import qualified Language.Pepa.Compile.States as States
import Language.Pepa.Compile.States
  ( StateSpace        ( .. )
  , State             ( .. )
  , StateMove
  , moveRate
  , moveTarget
  )
import Language.Pepa.Syntax
   ( ParsedModel  )
import Language.Prism.Syntax
   ( PrismModelFile  ( .. )
   , PrismModel      ( .. )
{-
   , PrismModelKind  ( .. )
   , PrismModule     ( .. )
   , PrismVariable   ( .. )

   , PrismTransition ( .. )
   , PrismAction
   , PrismCondition
   , PrismRate
-}
   )
{- End of Imports -}



{-| The main function exported from this module. Translates a pepa
   model into a prism model.
   The first option is whether or not to reduce the rate expressions.
-}
prismFromPepa :: Bool -> ParsedModel -> PrismModel
prismFromPepa _reduceRates = undefined   

{-| Turns a prism model in to a full-blown prism file ready for output.
   Note that the type of this is likely to change.
-}
makePrismFile :: PrismModel -> PrismModelFile
makePrismFile = undefined


{-|
  Takes in a state space and returns a file suitable for explicit model
  import to Prism.
  Obviously perhaps this should be in the 'Language.Pepa.Compile.Prism'
-}
getPrismExplictModel :: StateSpace -> String
getPrismExplictModel space = 
  unlines $ headLine : transitionLines
  where
  -- The first line must declare the number of states and
  -- the number of transitions.
  headLine        = unwords [ show $ length states
                            , show $ length transitions
                            ]
  -- Following the head line is a list of transitions
  transitionLines = map makeTransLine transitions

  -- Formatting a transition line is trivial it is a space separated
  -- line of "source-state target-state rate"
  makeTransLine :: (Int, Int, Double) -> String
  makeTransLine (i, j, r) = unwords [ show i, show j, show r ]

  tangibleMap     = spaceTangible space
  states          = Map.elems tangibleMap
  
  -- All the transitions in the model, there are got by mapping
  -- each state to its list of outgoing transitions.
  transitions     = concatMap getTrans states
  
  -- A transition is simple the source state, the target state and
  -- rate of the transition.
  getTrans :: State -> [ (Int, Int, Double) ]
  getTrans state =
    map makeTriple $ stateMovements state
    where
    makeTriple :: StateMove -> (Int, Int, Double)
    makeTriple move =
      case Map.lookup (moveTarget move) tangibleMap of
        -- We have to negate by one because state numbers are
        -- indexed from one but for prism from zero.
        Just targetState -> ( (States.stateNumberInt state) - 1
                            , (States.stateNumberInt targetState) - 1
                            , rateNumber $ moveRate move
                            )
        Nothing           ->
          error "getPrismExplictModel: serious fatal flaw, state not found"
