{-| This module is designed return a hierarchical model from
    those which are defined by L.P.C.Model.
-}
module Language.Pepa.Compile.Hierarchical
  ( modelToHier
  , HierModel   ( .. )
  , MotionTree  ( .. )
  , Motion      ( .. )
  )
where

{- Standard Library Modules Imported -}
import qualified Data.Map as Map
{- External Library Modules Imported -}
{- Local Modules Imported -}
import qualified Language.Pepa.QualifiedName as Qualified
import qualified Language.Pepa.Syntax as Pepa
import Language.Pepa.Syntax
  ( ParsedTrans
  , Transition   ( .. )
  )
import qualified Language.Pepa.Compile.States as States
import qualified Language.Pepa.Compile.Model as Model
import Language.Pepa.Compile.Model
  ( Model
  , Parallel     ( .. )
  , Derivative
  ) 
import qualified Language.Pepa.Rates as Rates
import Language.Pepa.QualifiedName
  ( QualifiedName )
{- End of Module Imports -}

-- We need a data structure for our hierarchical models:
data HierModel  = HierModel { modelTransitions :: MotionTree
                            , modelComponents  :: [ [ Component ] ]
                            , modelInitial     :: [ (Component, Int) ]
                            }
type Component  = QualifiedName
data MotionTree = Leaf [ Motion ]
                | Coop [ MotionKind ] MotionTree MotionTree
data Motion     = Motion { motionDecrements :: [ Component ]
                         , motionIncrements :: [ Component ]
                         , motionSpeed      :: Speed
                         -- a field for motion rate definitions.
                         , motionKind       :: String
                         }
type MotionKind = String
type Speed      = Rates.ModelRate
type SpeedDefs  = (QualifiedName, Rates.RateExpr)

modelToHier :: Model -> HierModel
modelToHier model =
  HierModel { modelTransitions = modelToMotionTree model
            , modelComponents  = components 
            , modelInitial     = initial
            }
  where
  components = map Model.derivativesOfComponent modelComps
  modelComps = Model.sequentialsOfModel model
  initial    = Map.toList $ States.getInitialConcentrations model

modelToMotionTree :: Model -> MotionTree
modelToMotionTree (Cooperate left actions right) =
  Coop mKinds (modelToMotionTree left) (modelToMotionTree right)
  where
  mKinds = map Qualified.textual actions
modelToMotionTree (Sequential _ mapping)           =
  Leaf $ concatMap translateTrans (Map.toList mapping)
  where
  translateTrans :: (Derivative, [ (ParsedTrans, Derivative) ]) -> [ Motion ]
  translateTrans (source, localTrans) =
    map mkMotion localTrans
    where
    mkMotion :: (ParsedTrans, Derivative) -> Motion
    mkMotion (trans, target) =
      -- Watch out, what if source == target, we should probably make
      -- those the empty set in that case.
      Motion { motionDecrements = [ source ]
             , motionIncrements = [ target ]
             , motionSpeed      = pepaTransRate trans
             , motionKind       = Qualified.textual name
             }
      where
      name = Pepa.nameOfAction $ pepaTransAction trans


motionTreeToTransitions :: MotionTree -> [ Motion ]
motionTreeToTransitions (Leaf motions)            = motions
motionTreeToTransitions (Coop actions left right) =
  []  
  where
  leftTrans  = motionTreeToTransitions left 
  rightTrans = motionTreeToTransitions right
