{-|
   A small module for performing some analysis of hydra models.
   In the ipc compiler we use this to provide some sanity checking
   of the compiled model.
-}
module Language.Hydra.Analysis
   ( sanityChecks
   )
where

{- Imported Standard Libraries -}
{- Imported Local Libraries -}
import Language.Pepa.QualifiedName
  ( hprintQualifiedName )
import Language.Hydra.Syntax
  ( DNAMmodelFile         ( .. )
  , DNAMmodel             ( .. )
  , DNAMtransition        ( .. )
  )
{- End of Imports -}

{-|
  Just for now we return a list of error messages but I think
  in the future we will return something more sane.
-}
sanityChecks :: DNAMmodelFile a b -> [ String ]
sanityChecks (DNAMmodelFile model _sCont _pMeas) =
  selfLoopErrors model


selfLoopErrors :: DNAMmodel a b -> [ String ]
selfLoopErrors model =
  concatMap emptyAction $ modelTransitions model
  where
  emptyAction :: DNAMtransition a b -> [ String ]
  emptyAction t 
    | null $ transActions t = 
      [ unwords [ "The transition"
                ,  hprintQualifiedName $ transName t
                , "contains no actions and is hence a self loop"
                ]
      ]
    | otherwise             =
      []
  