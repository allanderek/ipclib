{-|
   A suite of data types describing the abstract syntax of Srmc models.
   Since Srmc is a super-set of Pepa it will draw heavily upon the
   'Language.Pepa.Syntax' module.
-}
module Language.Pepa.Srmc.Syntax
   ( ServiceIdent
   , SrmcDef      ( .. )
   , SrmcModel    ( .. )
   , SrmcSystem
   )
where

{- Imported Standard Libraries -}
{- Imported Local Libraries -}
import Language.Pepa.QualifiedName
  ( QualifiedName )
import Language.Pepa.Rates
  ( RateExpr )
import Language.Pepa.Syntax
   ( RateIdentifier
   , ParsedComponentId
   , ParsedComponent
   )
{- End of Imports -}

{-  A rate definition in srmc can be a set of rates from which one is selected -}
-- type RateDef = (RateIdentifier, [ ParsedRate ])

{-| The type of a service identifier -}
type ServiceIdent = QualifiedName

{-| The type of definitions which are extra to that of standard Pepa -}
data SrmcDef = ServiceDef   ServiceIdent [ SrmcDef ]
             | NameSpaceSet ServiceIdent [ ServiceIdent ]
             | RateSet      RateIdentifier [ RateExpr ]
             | ProcessSet   ParsedComponentId [ ParsedComponent ]
             | VirtualDef   ParsedComponentId RateExpr



{-| The type of an Srmc model, basically the same as a pepa model
    with the added capability of defining services.
-}
data SrmcModel = SrmcModel [ SrmcDef ] SrmcSystem

{-| The type of the main system equation in an srmc model -}
type SrmcSystem = ParsedComponent