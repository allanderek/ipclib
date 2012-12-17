module Language.Pepa.Compile.Pddl
  ( statespaceToPddl )
where

{- Standard Library Modules Imported -}
import qualified Data.Maybe as Maybe
{- External Library Modules Imported -}
{- Local Modules Imported -}
import qualified Language.Pepa.Syntax as Pepa
import qualified Language.Pepa.QualifiedName as Qualified
import qualified Language.Pepa.Compile.States as States
import Language.Pepa.Compile.States
  ( State       ( .. )
  , StateSpace  ( .. )
  , StateMove
  )
{- End of Module Imports -}

data PddlAction = PddlAction { pddlActionName         :: String
                             , pddlActionPreCondition :: String
                             , pddlActionEffect       :: String
                             , pddlActionComment      :: String
                             }

pddlPrintAction :: PddlAction -> String
pddlPrintAction action = 
  parens $ unlines [ ":action " ++ (pddlActionName action)
                   , "# " ++ (pddlActionComment action)
                   , ":precondition"
                   , "    " ++ (parens $ pddlActionPreCondition action)
                   , ":effect"
                   , "    " ++ (parens $ pddlActionEffect action)
                   ]

parens :: String -> String
parens s = concat [ "(", s, ")" ]

statespaceToPddl :: StateSpace -> String
statespaceToPddl statespace = 
  unlines $ map pddlPrintAction allActions
  where
  allStates      = States.allStatesInSpace statespace
  allActions     = concat $ map stateToActions allStates
  
  
  stateToActions :: State -> [ PddlAction ]
  stateToActions state =
    map translateMovement $ zip [0 ..] (stateMovements state)
    where
    sName = stateName state
    translateMovement :: (Int, StateMove) -> PddlAction
    translateMovement (i, (trans, targetRepr)) =
      PddlAction { pddlActionName         = concat [ sName
                                                   , "__"
                                                   , aName
                                                   , "__"
                                                   , show i
                                                   ]
                 , pddlActionPreCondition = sName
                 , pddlActionEffect       = parens $ unlines effects
                 , pddlActionComment      = comment
                 }
      where
      comment = unwords [ "We're in state: "
                        , States.stateReprName $ stateRep state ]
      aName   = Qualified.textual $ Pepa.actionNameOfTrans trans
      effects = [ parens $ concat [ "not "
                                  , parens sName
                                  ]
                , parens $ stateName target
                ]
      target   = Maybe.fromMaybe (error "No such state") $
                 States.findStateRepr statespace targetRepr
    

stateName :: State -> String
stateName = ("state_" ++) . show . States.stateNumberInt
  
  