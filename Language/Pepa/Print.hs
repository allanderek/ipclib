{-|
  This module is intended to be a pretty printer for pepa models.
  It should be updated to use 'Text.PrettyPrint.HughesPJ'

  The prefix pprint means the function goes from the argument to
  a pretty printed Doc. The prefix hprint means it goes from the
  argument to a pretty printed string. Generally
  @hprintX = Pretty.render . pprintX@
-}
module Language.Pepa.Print
  ( hprintPepaModel
  , pprintPepaModel
  , hprintProcessDef
  , hprintVirtualDef
  , pprintVirtualDef
  , hprintComponent
  , hprintComponentName
  , pprintComponentName
  , hprintCooperationSet
  , hprintParsedAction
  , pprintActionIdentifier
  , hprintActionIdentifier
  , hprintRateDef
  , hprintRateIdentifier
  , pprintParsedRate
  , hprintParsedRate
  , hprintRateExpr
  , pprintRateExpr
  , pprintCommaList
  , hprintActionIdentList
  , pprintActionList
  , actionToDoc
  , pprintAngles
  )
where


{- Imported Standard Libraries -}
import qualified Text.ParserCombinators.Parsec as Parsec
import qualified Text.PrettyPrint.HughesPJ as Pretty
import Text.PrettyPrint.HughesPJ
  ( Doc
  , (<>)
  )
{- Imported Local Libraries -}
import qualified Language.Pepa.QualifiedName as Qualified
import Language.Pepa.QualifiedName
  ( QualifiedName )
import qualified Language.Pepa.Rates as Rates
import Language.Pepa.Rates
  ( Rate                 ( .. )
  , RateExpr             ( .. )
  )
import Language.Pepa.Syntax
  ( ParsedModel          ( .. )
  , ProcessDef
  , VirtualSpec
  , RateSpec
  , RateIdentifier
  , ParsedRate
  , ParsedComponentId
  , ParsedComponent      ( .. )
  , CooperationSet       ( .. )
  , Transition           ( .. )
  , ParsedAction         ( .. )
  , ActionIdentifier
  )
import qualified Language.Pepa.Parser as PepaParser
import qualified Language.Pepa.Utils as Utils
{- End of Imports -}


{-|
   The main exported function of this model. 
   Prints a pepa model in a human readable format.
-}
hprintPepaModel :: ParsedModel -> String
hprintPepaModel =
  Pretty.render . pprintPepaModel

{-| Pretty print to a document a pepa model -}
pprintPepaModel :: ParsedModel -> Doc
pprintPepaModel pModel =
  Pretty.vcat [ Pretty.vcat $ map pprintRateDef (modelRateSpecs pModel)
              , Pretty.text ""
              , Pretty.vcat $ map pprintProcessDef (modelProcessDefs pModel)
              , Pretty.text ""
              , Pretty.vcat $ map pprintVirtualDef (modelVirtualComps pModel)
              , Pretty.text ""
              , pprintComponent $ modelSystemEqn pModel
              ]

{-| Pretty prints a rate definition to a String -}
hprintRateDef :: RateSpec -> String
hprintRateDef = Pretty.render . pprintRateDef

{-| Pretty prints to a document a rate specification -}
pprintRateDef :: RateSpec -> Doc
pprintRateDef (ident, rExpr) = 
  Pretty.hsep [ Qualified.pprintQualifiedName ident
              , Pretty.text "="
              , pprintRateExpr rExpr
              , Pretty.text ";"
              ]

{-| Pretty prints a virtual component definition to a String -}
hprintVirtualDef :: VirtualSpec -> String
hprintVirtualDef = Pretty.render . pprintVirtualDef

{-| Pretty prints to a document a virtual component definition -}
pprintVirtualDef :: VirtualSpec -> Doc
pprintVirtualDef (ident, rExpr) = 
  Pretty.hsep [ pprintComponentName ident
              , Pretty.text "="
              , Pretty.brackets $ pprintRateExpr rExpr
              , Pretty.text ";"
              ]

{-| Pretty prints a component definition to a string -}
hprintProcessDef :: ProcessDef -> String
hprintProcessDef = Pretty.render . pprintProcessDef

{-| Pretty prints a component definition -}
pprintProcessDef :: ProcessDef -> Doc
pprintProcessDef (ident, component) =
  Pretty.hsep [ pprintComponentName ident
              , Pretty.text "="
              , pprintComponent component
              , Pretty.text ";"
              ]

{-| Pretty prints a component name to a string -}
hprintComponentName :: ParsedComponentId -> String
hprintComponentName = Pretty.render . pprintComponentName


{-| Pretty prints a component name to a document -}
pprintComponentName :: Qualified.QualifiedName -> Doc
pprintComponentName = pprintStructuredName PepaParser.pepaComponentId 

{-| Pretty prints a component to a string -}
hprintComponent :: ParsedComponent -> String
hprintComponent = Pretty.render . pprintComponent

{-| Pretty prints a component to a document -}
-- See the comment below for pretty printing rate expressions
-- basically we put parentheses round all binary operators
-- At a later date we may see about changing that.
pprintComponent :: ParsedComponent -> Doc
pprintComponent (StopProcess)                          = Pretty.text "Stop"
pprintComponent (IdProcess ident)                      = pprintComponentName ident
pprintComponent (ComponentSum left right)              = 
  Pretty.parens $ Pretty.sep [ pprintComponent left
                             , Pretty.text "+"
                             , pprintComponent right
                             ]
pprintComponent (PrefixComponent trans next)
  | null conditions  = printedPrefix
  | otherwise        =
    Pretty.sep [ Pretty.text "if"
               , Pretty.sep (Pretty.punctuate (Pretty.text "&&") $
                             map pprintRateExpr conditions)
               , printedPrefix
               ]
  where
  conditions    = pepaTransConditions trans
  printedPrefix = Pretty.hcat [ pprintTrans
                              , Pretty.text "."
                              , pprintComponent next
                              ]
  pprintTrans :: Doc
  pprintTrans 
    | isDefaultImm rate = pprintParsedAction $ pepaTransAction trans
    | otherwise         =
      Pretty.parens $ 
      Pretty.sep [ Pretty.hcat [ pprintParsedAction $ pepaTransAction trans
                               , Pretty.text ", "
                               ]
                 , pprintRate pprintRateExpr rate
                 ]
    where
    rate = pepaTransRate trans
    isDefaultImm :: ParsedRate -> Bool
    isDefaultImm (RateImmediate (Creal 1.0)) = True
    isDefaultImm _                           = False
pprintComponent (CondBehaviour cond next)              =
  Pretty.sep [ Pretty.text "if"
             , pprintRateExpr cond
             , pprintComponent next
             ]
pprintComponent (Cooperation left WildCard right)      = 
  Pretty.parens $ Pretty.sep [ pprintComponent left
                             , Pretty.text "<*>"
                             , pprintComponent right
                             ]
pprintComponent (Cooperation left (ActionSet []) right) = 
  Pretty.parens $ Pretty.sep [ pprintComponent left
                             , Pretty.text "||"
                             , pprintComponent right
                             ]
pprintComponent (Cooperation left coopSet right)       = 
  Pretty.parens $ Pretty.sep [ pprintComponent left
                             , pprintCooperationSet coopSet
                             , pprintComponent right
                             ]
pprintComponent (ProcessArray component size actions)  = 
  Pretty.hcat [ pprintComponent component
              , Pretty.brackets $ pprintRateExpr size
              , maybe Pretty.empty printActions actions
              ]
  where
  -- So print the actions surrounded by square brackets, of course
  -- this is only used if actions does not equal Nothing.
  printActions = Pretty.brackets . pprintParsedActionList
pprintComponent (Hiding comp actions)                  =
  Pretty.hcat [ pprintComponent comp
              , Pretty.text "/"
              , Pretty.braces $ pprintParsedActionList actions
              ]



{-| This is not the same as printing a rate identifier because
    a component name maybe used in a rate expression which MAY be
    a string literal.
-}
pprintRateName :: Qualified.QualifiedName -> Doc
pprintRateName = pprintStructuredName PepaParser.rateExprIdent

pprintStructuredName :: (Parsec.Parser QualifiedName) -> QualifiedName -> Doc
pprintStructuredName parser ident =
  case Parsec.parse (PepaParser.wholeParser parser) "no-file" text of
    -- Parse error, output as a string literal
    Left _  -> Pretty.doubleQuotes $ Pretty.text text
    Right _ -> Pretty.text text
  where
  text = Qualified.textual ident

-- Note that we do not include the angled brackets since this is used
-- also in an array cooperation set, and indeed also for hiding.
pprintCooperationSet :: CooperationSet -> Doc
pprintCooperationSet (WildCard)          = Pretty.text "<*>"
pprintCooperationSet (ActionSet [])      = Pretty.text "||"
pprintCooperationSet (ActionSet actions) = 
  pprintAngles $ pprintParsedActionList actions 

hprintCooperationSet :: CooperationSet -> String
hprintCooperationSet = Pretty.render . pprintCooperationSet

{-| Pretty prints a rate to a document -}
pprintRate :: (a -> Doc) -> Rate a -> Doc
pprintRate f (RateTimed a)     = f a
pprintRate f (RateTop a)       = (Pretty.text "infty") <> (f a)
pprintRate f (RateImmediate a) = (Pretty.text "immediate") <> (f a)

{-| Pretty print a parsed rate to a document -}
pprintParsedRate :: ParsedRate -> Doc
pprintParsedRate (RateTimed e)         = pprintRateExpr e
pprintParsedRate (RateTop (Creal 1.0)) = Pretty.text "infty"
pprintParsedRate (RateTop e)       = 
  (Pretty.text "infty") <> (pprintRateExpr e)
pprintParsedRate (RateImmediate e) = 
  (Pretty.text "immediate") <> (pprintRateExpr e)


{-| Pretty print in a parsed model rate to a string -}
hprintParsedRate :: ParsedRate -> String
hprintParsedRate = Pretty.render . pprintParsedRate

{-| Pretty prints a rate identifier to a string -}
hprintRateIdentifier :: RateIdentifier -> String
hprintRateIdentifier = Qualified.hprintQualifiedName

{- Pretty prints a rate expression to a string -}
hprintRateExpr :: RateExpr -> String
hprintRateExpr = Pretty.render . pprintRateExpr

{- Pretty prints a rate expression to a document -}
-- Note this relies on the fact that we always wrap up binary expressions
-- in parentheses. For example (CNot e) is represented as (! e)
-- and not: !(e), so that say e is x + y it must be written as (x + y)
-- otherwise we would get (!x + y), which is not what was intended
pprintRateExpr :: RateExpr -> Doc
pprintRateExpr (Cconstant i)     = Pretty.int i
pprintRateExpr (Cident ident)    = pprintRateName ident
pprintRateExpr (Creal d)         = Utils.pprintDouble d
pprintRateExpr (Cadd e1 e2)      = pprintBinopRateExpr e1 "+" e2
pprintRateExpr (Csub e1 e2)      = pprintBinopRateExpr e1 "-" e2
pprintRateExpr (Cmult e1 e2)     = pprintBinopRateExpr e1 "*" e2
pprintRateExpr (Cdiv e1 e2)      = pprintBinopRateExpr e1 "/" e2
pprintRateExpr (Cifte e1 e2 e3)  = 
  Pretty.parens $ Pretty.sep [ Pretty.text "if"
                             , pprintRateExpr e1
                             , Pretty.text "then"
                             , pprintRateExpr e2
                             , Pretty.text "else"
                             , pprintRateExpr e3
                             ]
pprintRateExpr (Cand left right) = pprintBinopRateExpr left "&&" right
pprintRateExpr (Cor  left right) = pprintBinopRateExpr left "||" right
pprintRateExpr (Cgt  left right) = pprintBinopRateExpr left ">" right
pprintRateExpr (Cge  left right) = pprintBinopRateExpr left ">=" right
pprintRateExpr (Clt  left right) = pprintBinopRateExpr left "<" right
pprintRateExpr (Cle  left right) = pprintBinopRateExpr left "<=" right
pprintRateExpr (Ceq  left right) = pprintBinopRateExpr left "==" right
pprintRateExpr (Cnot e)          = 
  Pretty.parens $ Pretty.sep [ Pretty.text "!"
                             , pprintRateExpr e
                             ]
pprintRateExpr (Cminimum e1 e2)  =
  Pretty.parens $ Pretty.sep [ Pretty.text ipcMinimumString
                             , Pretty.parens $
                               Pretty.sep [ pprintRateExpr e1
                                          , Pretty.comma
                                          , pprintRateExpr e2
                                          ]
                             ]

-- auxiliary for pretty printing rate expressions
-- currently we just wrap up all binary operator expressions
-- in parentheses so there can be no ambiguity.
pprintBinopRateExpr :: RateExpr -> String -> RateExpr -> Doc
pprintBinopRateExpr r1 operator r2 =
  Pretty.parens $ Pretty.sep [ pprintRateExpr r1
                             , Pretty.text operator
                             , pprintRateExpr r2
                             ]

{-| Pretty print an action to a string -}
hprintParsedAction :: ParsedAction -> String
hprintParsedAction = Pretty.render . pprintParsedAction

{-| Pretty print an action to a doc -}
pprintParsedAction :: ParsedAction -> Doc
pprintParsedAction (Action ident)    = Qualified.pprintQualifiedName ident
pprintParsedAction (ComAction ident) = Qualified.pprintQualifiedName ident 
pprintParsedAction (Tau    ident)    = 
  (Pretty.text "`") <> (Qualified.pprintQualifiedName ident)


pprintParsedActionList :: [ ParsedAction ] -> Doc
pprintParsedActionList = pprintCommaList . (map pprintParsedAction)

ipcMinimumString :: String
ipcMinimumString = "min"

hprintActionIdentList :: [ ActionIdentifier ] -> String
hprintActionIdentList = Pretty.render . pprintActionList

pprintActionList :: [ ActionIdentifier ] -> Doc
pprintActionList = pprintCommaList . (map actionToDoc)

hprintActionIdentifier :: ActionIdentifier -> String
hprintActionIdentifier = Qualified.hprintQualifiedName

pprintActionIdentifier :: ActionIdentifier -> Doc
pprintActionIdentifier = Qualified.pprintQualifiedName

actionToDoc :: ActionIdentifier -> Doc
actionToDoc = Qualified.pprintQualifiedName

pprintAngles :: Doc -> Doc
pprintAngles doc = Pretty.hcat [ Pretty.text "<"
                               , doc
                               , Pretty.text ">"
                               ]

pprintCommaList :: [ Doc ] -> Doc
pprintCommaList =
  Pretty.sep . Pretty.punctuate Pretty.comma
