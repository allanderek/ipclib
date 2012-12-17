{-|
   A printing module for Prism files.
-}
module Language.Prism.Print
   ( printPrismModelFile
   )
where


{- Imported Standard Libraries -}
import qualified Data.List as List
{- Imported Local Libraries -}
import Language.Prism.Syntax
   ( PrismModelFile      ( .. )
   , PrismModel          ( .. )

   , PrismModelKind      ( .. )
   , PrismModule         ( .. )
   , PrismVariable       ( .. )
   , PrismTransition     ( .. )
   , PrismCondition  --  ( .. )
   , PrismAction     --  ( .. )
   , PrismRate
   )
import Language.Hydra.Syntax
   ( DNAMcondition       ( .. )
   , DNAMcassignment     ( .. )
   , DNAMspeed           ( .. )
   )
import Language.Hydra.Print
   ( printCexp
   )
import Language.Pepa.QualifiedName
   ( hprintQualifiedName )
{- End of Imports -}


{-| The main exported printing function of this module.
    Prints a prism model file.
-}
printPrismModelFile :: PrismModelFile -> String
printPrismModelFile (PrismModelFile model) =
   printPrismModel model

printPrismModel :: PrismModel -> String
printPrismModel (PrismModel kind modules) =
   unlines ( printPrismModelKind kind :
             (map printPrismModule modules) )
          

printPrismModelKind :: PrismModelKind -> String
printPrismModelKind PrismMdm  = "mdm"
printPrismModelKind PrismDtmc = "dtmc"
printPrismModelKind PrismCtmc = "ctmc"

printPrismModule :: PrismModule -> String
printPrismModule (PrismModule name variables transitions) =
   unlines $ concat [ [ headLine ]
                    , map printPrismVariable variables
                    , map printPrismTransition transitions
                    , [ "endmodule" ]
                    ]
   where
   headLine = "module " ++ (hprintQualifiedName name)

printPrismVariable :: PrismVariable -> String
printPrismVariable variable =
   unwords [ hprintQualifiedName $ prismVarname variable
           , "["
           , show $ prismVarLow variable
           , ".."
           , show $ prismVarHigh variable
           , "]"
           , "init"
           , show $ prismVarInit variable
           , ";"
           ]
   
printPrismTransition :: PrismTransition -> String
printPrismTransition transition =
   unwords [ "[]"
           , conditions
           , "->"
           , printPrismSpeed $ prismTransRate transition
           , ":"
           , actions
           , ";"
           ]
   where
   conditions    = unwords condStrings
   condStrings   = map printPrismCondition $ prismTransGuard transition
   actions       = List.intercalate " & " actionStrings
   actionStrings = map printPrismAction $ prismTransActions transition


printPrismSpeed :: PrismRate -> String
printPrismSpeed (DNAMrate cexp) = printCexp cexp
printPrismSpeed (DNAMweight _)  =
   error "Immediate actions unimplemented in Prism compilation"

printPrismCondition :: PrismCondition -> String
printPrismCondition (DNAMcond cexp)         = printCexp cexp
printPrismCondition (DNAMprocpresent ident) =
   (hprintQualifiedName ident) ++ " > 0"


printPrismAction :: PrismAction -> String
printPrismAction (DNAMidentIncr ident) =
   updateVariable (hprintQualifiedName ident) "+ 1"
printPrismAction (DNAMidentDecr ident) =
   updateVariable (hprintQualifiedName ident) "- 1"
printPrismAction (DNAMcassignment _ _) =
   error "Unimplemented prism update"

-- Creates a prism variable updated with the second string.
updateVariable :: String -> String -> String
updateVariable ident oper =
   unwords [ ident, "=", ident, oper ]