{-|
   This module is a formatter for Fsp models.
   As usual we attempt to make this vaguely human readable but
   there is no massive effort as this will be mostly read
   by a program.
   Also as usual we could see this module being split into
   two, one producing human readable and the other simply
   machine readable.
-}
module Language.Fsp.Print
   ( printFspModel
   )
where


{- Imported Standard Libraries -}
{- Imported Local Libraries -}
import Language.Pepa.QualifiedName
  ( hprintQualifiedName )
import Language.Fsp.Syntax
   ( FspModel             ( .. )
   , FspProcessDef
   , FspProcess           ( .. )
   )
{- End of Imports -}

{-| The printing of an fsp model -}
printFspModel :: FspModel -> String
printFspModel (FspModel defs) = 
   unlines $ map printFspProcessDef defs

{- The printing of fsp process definitions, straight forward -}
printFspProcessDef :: FspProcessDef -> String
printFspProcessDef (ident, process) =
   unwords [ hprintQualifiedName ident
           , "= ( "
           , printFspProcess process
           , ") ."
           ]

{- The printing of fsp processes -}
printFspProcess :: FspProcess -> String
printFspProcess ( FspIdProcess ident )        = hprintQualifiedName ident
printFspProcess ( FspEnd )                    = "End"
printFspProcess ( FspPrefix action process )  =
   concat [ hprintQualifiedName action
          , "->"
          , printFspProcess process
          ]
printFspProcess ( FspChoice left right )      =
   unwords [ printFspProcess left
           , "|"
           , printFspProcess right
           ]
printFspProcess ( FspComposition left right ) =
   unwords [ printFspProcess left
           , "||"
           , printFspProcess right
           ]
printFspProcess ( FspReplicator ident size )  =
   unwords [ "forall[i:1.." ++ show size ++ "]"
           , hprintQualifiedName ident ++ "(i)"
           ]
