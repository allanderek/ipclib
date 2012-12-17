{-|
   A suite of data types describing the abstract syntax of Dizzy models.
   As usual for compilation target languages this does not attempt to
   be a complete abstract syntax for dizzy models only the fragment to
   which we compile Pepa models (though naturally it could be extended).
-}
module Language.Dizzy.Syntax
  ( DizzyModel   ( .. )
  , InitialConcentration
  )
where

{- Imported Standard Libraries -}
{- Imported Local Libraries -}
import Language.Pepa.QualifiedName
  ( QualifiedName )
import Language.Pepa.Compile.TimedSystemEquation
  ( RateEquation )
{- End of Imports -}


{-| The type of identifiers in dizzy model file -}
type DizzyIdent = QualifiedName

{-| The type of a dizzy model -}
data DizzyModel = DizzyModel [ InitialConcentration ] [ RateEquation ]

{-| The type of initial concentrations in dizzy models -}
type InitialConcentration = (DizzyIdent, Int)