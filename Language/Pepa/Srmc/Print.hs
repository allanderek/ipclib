{-|
  This module is intended to be a pretty printer for pepa models.
  It should be updated to use 'Text.PrettyPrint.HughesPJ'
-}
module Language.Pepa.Srmc.Print
  ( hprintSrmcModel )
where


{- Imported Standard Libraries -}
{- Imported Local Libraries -}
import Language.Pepa.Srmc.Syntax
  ( SrmcModel      ( .. )
  , SrmcDef        ( .. )
  )
import qualified Language.Pepa.Print as Print
import Language.Pepa.QualifiedName
  ( hprintQualifiedName )
import Language.Pepa.Utils
  ( mkCSlist )
import Language.Hydra.Print
  ( printCexp )
{- End of Imports -}



{-|
   The main exported function of this model. 
   Prints an srmc model in a human readable format.
-}
hprintSrmcModel :: SrmcModel -> String
hprintSrmcModel (SrmcModel sDefs composition) =
   unlines $ (map hprintServiceDef sDefs)
          ++ ("" : [ Print.hprintComponent composition ] ) 

hprintServiceDef :: SrmcDef -> String
hprintServiceDef (ServiceDef ident sDefs)     =
   unlines $ [ (hprintQualifiedName ident) ++ "::{" ]
          ++ (map hprintServiceDef sDefs)
          ++ [ "}" ]
hprintServiceDef (NameSpaceSet ident names)   =
   unwords [ hprintQualifiedName ident
           , "="
           , "{"
           , mkCSlist $ map hprintQualifiedName names
           , "}"
           , ";"
           ]
hprintServiceDef (VirtualDef ident oneRate)   =
   unwords [ hprintQualifiedName ident
           , "= ["
           , printCexp oneRate
           , "] ;"
           ]
hprintServiceDef (RateSet ident [ oneRate ])  =
   unwords [ hprintQualifiedName ident
           , "="
           , printCexp oneRate
           , ";"
           ]
hprintServiceDef (RateSet ident rates)        =
   unwords [ hprintQualifiedName ident
           , "="
           , "{"
           , mkCSlist $ map printCexp rates
           , "}"
           , ";"
           ]
hprintServiceDef (ProcessSet ident [ oneP ] ) =
   unwords [ hprintQualifiedName ident
           , "="
           , Print.hprintComponent oneP
           , ";"
           ]
hprintServiceDef (ProcessSet ident comps)     =
   unwords [ hprintQualifiedName ident
           , "="
           , "{"
           , mkCSlist $ map Print.hprintComponent comps
           , "}"
           , ";"
           ]

