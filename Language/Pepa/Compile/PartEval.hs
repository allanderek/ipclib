module Language.Pepa.Compile.PartEval
  ( partEvalPepaModel )
where

{- Standard Library Modules Imported -}
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
{- External Library Modules Imported -}
{- Local Modules Imported -}
import qualified Language.Pepa.QualifiedName as Qualified
import qualified Language.Pepa.Rates as Rates
import qualified Language.Pepa.Syntax as Pepa
import Language.Pepa.Syntax
  ( ParsedModel       ( .. )
  , ParsedComponent   ( .. )
  , ProcessDef
  )
import Language.Pepa.MainControl
  ( MainControl )
import qualified Language.Pepa.Transform.Simplify as Simplify
import qualified Language.Pepa.Compile.States as States
import Language.Pepa.Compile.States
  ( State       ( .. )
  , StateSpace  ( .. )
  )
{- End of Module Imports -}

partEvalPepaModel :: ParsedModel -> MainControl ParsedModel
partEvalPepaModel pModel =
  do sResult <- Simplify.simplify [] pModel
     let sModel = Simplify.simplifiedModel sResult
     -- It seems wrong to be just throwing away the simplified rate
     -- and virtual mappings?
     statespace <- States.getModelStateSpace States.defaultGenerateOptions sModel
     createModelFromStateSpace statespace

createModelFromStateSpace :: StateSpace -> MainControl ParsedModel
createModelFromStateSpace statespace =
  return $ ParsedModel { modelSystemEqn    = systemComp
                       , modelProcessDefs  = processDefs
                       , modelRateSpecs    = []
                       , modelVirtualComps = virtualDefs
                       }
  where
  systemComp  = IdProcess $ stateName systemState
  systemState = Maybe.fromMaybe (error "Initial state not found") $
                Map.lookup (spaceInitial statespace) tangibleMap
  virtualDefs = map getVirtualDef $ Set.elems (spaceAllDerivs statespace)
  processDefs = map getProcessDef $ Map.elems $ spaceTangible statespace
  
  allStates      = States.allStatesInSpace statespace
  tangibleMap    = spaceTangible statespace
    
  getProcessDef :: State -> ProcessDef
  getProcessDef state =
    (stateName state, stateProcess)
    where
    stateProcess
      | null prefixes = StopProcess
      | otherwise     = foldr1 ComponentSum prefixes
    prefixes = map getPrefix $ stateMovements state
    getPrefix :: States.StateMove -> ParsedComponent
    getPrefix (transition, targetRepr) = 
      PrefixComponent newTransition $ IdProcess targetName
      where
      newTransition = Pepa.modifyTransitionRate rateModify transition
      rateModify    = Rates.modifyRate Rates.Creal
      targetName    = stateName targetState
      targetState   = Maybe.fromMaybe errorVal mTargetState
      mTargetState  = Map.lookup targetRepr tangibleMap
      errorVal      = error "State representation not found in part-eval"
      
  getVirtualDef :: Qualified.QualifiedName -> Pepa.VirtualSpec
  getVirtualDef name =
    (name, rateExpr)
    where
    rateExpr
      | null stateExprs = Rates.Cconstant 0
      | otherwise       = foldr1 Rates.Cadd stateExprs
    stateExprs = Maybe.mapMaybe getStateExpr allStates
    getStateExpr :: State -> Maybe Rates.RateExpr
    getStateExpr s = do i <- Map.lookup name $ stateConcentrations s
                        case i of 
                          0 -> Nothing
                          1 -> return $ Rates.Cident (stateName s)
                          _ -> return $ multiplySI (stateName s) i

    multiplySI :: Qualified.QualifiedName -> Int -> Rates.RateExpr
    multiplySI s i = Rates.Cmult (Rates.Cident s) (Rates.Cconstant i)


stateName :: State -> Qualified.QualifiedName
stateName = Qualified.unqualified . States.stateReprName . stateRep
