{-|
   A suite of data types describing the abstract syntax of Fsp models.
   As usual for compilation target languages this does not attempt to
   be a complete abstract syntax for fsp models only the fragment to
   which we compile Pepa models (though naturally it could be extended).
-}
module Language.Fsp.Syntax
   ( FspModel        ( .. )
   , FspProcessDef
   , FspProcess      ( .. )
   )
where

{- Imported Standard Libraries -}
{- Imported Local Libraries -}
import Language.Pepa.QualifiedName
   ( QualifiedName )
{- End of Imports -}

{-| The type of an identifier used in an fsp model -}
type FspIdentifier = QualifiedName

{-| The type of fsp actions which are the first part of a
    prefix process
-}
type FspAction     = FspIdentifier

{-| The type of an fsp model -}
data FspModel      = FspModel [ FspProcessDef ]

{-| The type of process definitions in fsp -}
type FspProcessDef = ( FspIdentifier, FspProcess )

{-| The type of a process in fsp -}
data FspProcess    = FspIdProcess   FspIdentifier
                   | FspPrefix      FspAction FspProcess
                   | FspChoice      FspProcess FspProcess
                   | FspComposition FspProcess FspProcess
                   | FspReplicator  FspIdentifier Int
                   | FspEnd

