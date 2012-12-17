{-|
   A printing module for Dizz files.
-}
module Language.Dizzy.Print
   ( printDizzyModelFile
   )
where


{- Imported Standard Libraries -}
{- Imported Local Libraries -}
import Language.Pepa.QualifiedName
  ( hprintQualifiedName )
import Language.Dizzy.Syntax
  ( DizzyModel   ( .. )
  , InitialConcentration
  )
import Language.Pepa.Compile.TimedSystemEquation
  ( printRateEquation )
{- End of Imports -}


{-| The main exported printing function of this module.
    Prints a prism model file.
-}
printDizzyModelFile :: DizzyModel -> String
printDizzyModelFile (DizzyModel initConcs rateEqns) =
  unlines $ concat [ map printInitConcentration initConcs
                   , map printRateEquation rateEqns
                   ]
  where
  printInitConcentration :: InitialConcentration -> String
  printInitConcentration (ident, i) =
    unwords [ hprintQualifiedName ident
            , "="
            , show i
            , ";"
            ]
  -- "There are : " ++ (show $ length rateEqns) ++ " rate equations"




