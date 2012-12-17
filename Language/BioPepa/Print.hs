module Language.BioPepa.Print
  ( hprintBioPepaModel
  , pprintBioPepaModel
  )
where

{- Standard Library Modules Imported -}
import qualified Text.PrettyPrint.HughesPJ as Pretty
import Text.PrettyPrint.HughesPJ
  ( Doc )
{- External Library Modules Imported -}
{- Local Modules Imported -}
import Language.BioPepa.Syntax
  ( Model      ( .. )
  , RateDef
  , RateExpr   ( .. )
  , Component  ( .. )
  , ComponentDef
  , PrefixOp   ( .. )
  )
import qualified Language.Pepa.Print as PepaPrint
import Language.Pepa.Print
  ( pprintActionList
  , actionToDoc
  , pprintAngles
  )
import Language.Pepa.QualifiedName
  ( pprintQualifiedName )
import qualified Language.Pepa.Utils as Utils
{- End of Module Imports -}

hprintBioPepaModel :: Model -> String
hprintBioPepaModel = Pretty.render . pprintBioPepaModel



pprintBioPepaModel :: Model -> Doc
pprintBioPepaModel pModel =
  Pretty.vcat [ Pretty.vcat $ map rateDefToDoc (modelRateDefs pModel)
              , Pretty.text ""
              , Pretty.vcat $ map compDefToDoc (modelCompDefs pModel)
              , Pretty.text ""
              , compToDoc $ modelSystemEqn pModel
              ]

rateDefToDoc :: RateDef -> Doc
rateDefToDoc (ident, rateExpr) =
  Pretty.hsep [ pprintQualifiedName ident
              , Pretty.text "="
              , Pretty.brackets $ rateExprToDoc rateExpr
              , Pretty.text ";"
              ]

rateExprToDoc :: RateExpr -> Doc
rateExprToDoc (RateName ident)      = pprintQualifiedName ident
rateExprToDoc (RateConstant d)      = Utils.pprintDouble d
rateExprToDoc (RateMult left right) = Pretty.sep [ rateExprToDoc left
                                                 , Pretty.text "*"
                                                 , rateExprToDoc right
                                                 ]
rateExprToDoc (RateDiv  left right) = Pretty.sep [ rateExprToDoc left
                                                 , Pretty.text "/"
                                                 , rateExprToDoc right
                                                 ]
rateExprToDoc (RateAdd  left right) = Pretty.sep [ rateExprToDoc left
                                                 , Pretty.text "+"
                                                 , rateExprToDoc right
                                                 ]
rateExprToDoc (RateSub  left right) = Pretty.sep [ rateExprToDoc left
                                                 , Pretty.text "-"
                                                 , rateExprToDoc right
                                                 ]



compDefToDoc :: ComponentDef -> Doc
compDefToDoc (ident, comp) = 
  Pretty.hsep [ pprintQualifiedName ident
              , Pretty.text "="
              , compToDoc comp
              , Pretty.text ";"
              ]

compToDoc :: Component -> Doc
compToDoc (Named ident)                     = 
  pprintQualifiedName ident
compToDoc (PrefixComp action sto oper comp) =
  Pretty.hsep [ Pretty.parens prefix
              , prefixOpToDoc oper
              , pprintQualifiedName comp
              ]
  where
  prefix = Pretty.sep [ actionToDoc action
                      , Pretty.comma
                      , Pretty.text sto
                      ]
compToDoc (Choice left right)               =
  Pretty.sep [ compToDoc left
             , compToDoc right
             ]
compToDoc (Cooperation left actions right)  =
  Pretty.sep [ compToDoc left
             , pprintAngles $ pprintActionList actions
             , compToDoc right
             ]
compToDoc (CompConcent ident conc)          =
  Pretty.hcat [ pprintQualifiedName ident
              , Pretty.parens $ Pretty.text (show conc)
              ]

prefixOpToDoc :: PrefixOp -> Doc
prefixOpToDoc Reactant    = Pretty.text "<<"
prefixOpToDoc Product     = Pretty.text ">>"
prefixOpToDoc Activator   = Pretty.text "act"
prefixOpToDoc Inhibitor   = Pretty.text "inh"
prefixOpToDoc GenModifier = Pretty.text "gen"

