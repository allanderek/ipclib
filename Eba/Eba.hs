module Eba.Eba
  ( compileFile )
where

{- Standard Library Modules Imported -}
import System.Exit
  ( ExitCode    ( .. ) )
{- External Library Modules Imported -}
{- Local Modules Imported -}
import Language.BioPepa.Syntax
  ( defaultModelOptions )
import Language.Ptrees.Syntax
  ( Ptree     ( .. )
  , ModelTree ( .. )
  )
import qualified Language.Ptrees.Evaluate as Evaluate
{- End of Module Imports -}

compileFile :: FilePath -> IO ExitCode
compileFile file =
  Evaluate.evaluateAndDisplay Nothing options ptree
  where
  options = []
  ptree   = Pmodel $ Mtranslate $ MbiopepaFile defaultModelOptions file
