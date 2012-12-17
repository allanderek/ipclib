{-|
   A module for converting Pepa models to hydra models
-}

module Language.Pepa.Compile.Dizzy
   ( pepaToDizzy )
where


{- Imported Standard Libraries -}
import qualified Data.Map as Map
{- External Library Modules Imported -}
{- Imported Local Libraries -}
import Language.Pepa.Syntax
  ( ParsedModel )
import Language.Pepa.Compile.TimedSystemEquation
  ( pepaToRateEquations )
import Language.Dizzy.Syntax
  ( DizzyModel   ( .. ) )

import Language.Pepa.Analysis.Analysis
  ( getInitialConcentrations )
import Language.Pepa.MainControl
  ( MainControl )
{- End of Imports -}


{-| 
  Convert a pepa model into a dizzy model
-}
pepaToDizzy :: ParsedModel -> MainControl DizzyModel
pepaToDizzy model =
  do rateEqns  <- pepaToRateEquations model
     return $ DizzyModel initConcentrations rateEqns
  where
  initConcentrations = Map.toList $ getInitialConcentrations model
