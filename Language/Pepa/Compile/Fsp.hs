{-|
   A module for converting Pepa models to Fsp models
-}

module Language.Pepa.Compile.Fsp
   ( pepaModelToFspModel )
where

{- Imported Standard Libraries -}
import Control.Arrow
  ( second )
{- Imported Local Libraries -}
import Language.Pepa.QualifiedName 
  ( QualifiedName      ( .. ) )
import Language.Pepa.Rates
  ( rateExpressionInt ) -- for process array sizes
import Language.Pepa.Syntax
  ( ParsedModel        ( .. )
  , ProcessDef
  , ParsedComponent    ( .. )
  , Transition         ( .. )
  , nameOfAction
  ) 
import Language.Fsp.Syntax
   ( FspModel           ( .. )
   , FspProcessDef
   , FspProcess         ( .. )
   )
{- End of Imports -}

{- This doesn't do some modification to the model to get the
   cooperating (and non-cooperating) actions correct, so we still
   have to do that.
-}
pepaModelToFspModel :: ParsedModel -> FspModel
pepaModelToFspModel model =
   FspModel $ convertedDefs ++ [ mainDef ]
   where
   mainDef       = convertDef ( Unqualified "System"
                              , modelSystemEqn model
                              )

   convertedDefs = map convertDef $ modelProcessDefs model

   convertDef :: ProcessDef -> FspProcessDef
   convertDef = second convertProcess

convertProcess :: ParsedComponent -> FspProcess
convertProcess (StopProcess)     =
   FspEnd
convertProcess (IdProcess ident) =
   FspIdProcess ident
convertProcess (PrefixComponent trans next)           =
   FspPrefix (nameOfAction $ pepaTransAction trans) $ convertProcess next
convertProcess (ComponentSum left right)              =
   FspChoice (convertProcess left)
             (convertProcess right)
{-The parallel components -}
convertProcess (Cooperation left _actions right)      =
   FspComposition (convertProcess left)
                  (convertProcess right)
convertProcess (ProcessArray (IdProcess ident) size _actions)     =
   -- TOOD: similar to below, find out if FspReplicator can really
   -- take an expression as the size, probably not so this is
   -- probably correct.
   FspReplicator ident (rateExpressionInt size)
convertProcess (ProcessArray _ _ _)                   =
   -- TODO: Find out if fsp can actually handle complex components
   -- in the array, if so this is an easy fix we just update the
   --- FspReplicator constructor. If not then we probably can't do
   -- anything here, or it will at least involve returning new
   -- definitions.
   error "Complex process array: we cannot translate such an array"
convertProcess (Hiding _ _)                           =
   error "Compile.Fsp: Hiding should have been removed from the model"
convertProcess (CondBehaviour _ _)                    =
   error $ "Compile.Fsp: Conditional behaviour should have been removed "
           ++ "from the model"
