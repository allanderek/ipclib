module Language.Pepa.Transform.Rules.Syntax
  ( Rules
  , Rule           ( .. )
  , ComponentName
  , Pattern        ( .. )
  , ActionsPattern ( .. )
  , PatternVariable
  , ExprPattern    ( .. )
  , Replacement

  , concatRules
  , makeRuleSet
  , patternVariables
  , replacementVariables

  , hprintRules
  , pprintRules
  )
where

{- Standard Library Modules Imported -}
import qualified Text.PrettyPrint.HughesPJ as Pretty
import Text.PrettyPrint.HughesPJ
  ( Doc )
{- External Library Modules Imported -}
{- Local Modules Imported -}
import Language.Pepa.QualifiedName
  ( QualifiedName
  , hprintQualifiedName
  )
import Language.Pepa.Syntax
  ( ActionIdentifier )
import qualified Language.Pepa.Print as Print
{- End of Module Imports -}

{-| Many re-write rules -}
type Rules = [ Rule ]

{-| The data structure to hold a re-write rule.
-}
data Rule = Rule { rulePattern :: Pattern
                 , ruleReplace :: Replacement
                 }
                 deriving (Show, Read)


{- | The type of a component name in a rule specification is merely a
     string.
-}
type ComponentName = QualifiedName

{-| The type of a pattern, generally we will match something in
    the system equation to this and the replace the matching part
    with the given replacement.
    However it may not be quite so easy since we probably want to
    match generally and have the replacement depend upon the part
    that is matched.

    We have a constructor for partial evaluation, clearly one cannot
    match against this, but since currently the replacement is a
    pattern we need a way to represent this. 
-}
data Pattern = IdentPat ComponentName
             | VarPat PatternVariable
             | CoopPat Pattern ActionsPattern Pattern
             | ArrayPat Pattern ExprPattern (Maybe ActionsPattern)
             | PartEval Pattern
             deriving (Show, Read)

{-| The type of replacement, one may think that we could have
    here a 'Language.Pepa.Syntax.ParsedComponent' but we must
    allow slightly more complex entities since we wish to allow
    the user to refer to parts of the pattern that are matched.
    For now replacements are actually the same as patterns,
    but that may change.
-}
type Replacement = Pattern

{-| The type of a pattern for an action list, we need more than
    simply an action identifier list because we may want to say
    "Match any list of actions" or 
    "Match any list of actions that contains at least act"
    The 'AnyActions' is slightly misleading, it means the second
    of the above sentences, where the second argument (the list
    of action names) is empty then it will match any action list
    at all. Where it is non-empty it will match any list which
    contains at least those mentioned.
-}
data ActionsPattern = 
     Actions     [ ActionIdentifier ]
   | AnyActions  PatternVariable [ ActionIdentifier ]
   deriving (Show, Read)

{-| In patterns we commonly wish to match "anything" but then
    reproduce that "anything" in the replacement, for example
    P <?actions> Q ==> P <?actions, a> Q
    which would mean change any cooperation over P and Q such
    that whatever the cooperation is it also has 'a' in it.
    Or we might say something like:
    ?P <?actions> Q ==> ?P[2]<?actions>Q
    We need a type for pattern variables.
-}
type PatternVariable = QualifiedName


{-|
   An 'ExprPattern' is used to match against an expression
   usually the size of a process array.
   It's kind of a shame that we must define a whole new type
   which (should) look exactly like the expressions used in
   the PEPA syntax. Perhaps we could make those expressions
   paramaterised by the expression factors. So in the real
   expressions you would only have constants and rate names
   here we would need constants, rate names AND pattern variables.
-}
data ExprPattern =
    AnyExpr      PatternVariable
  | ConstantPat  Int
  | RealPattern  Double
  | PlusPattern  ExprPattern ExprPattern
  | MinusPattern ExprPattern ExprPattern
  | MultPattern  ExprPattern ExprPattern
  deriving (Show, Read)


{-| concatenates multiple sets of rules into one set -}
concatRules :: [ Rules ] -> Rules
concatRules = concat

{-| Create a set of rules from a list of rules -}
makeRuleSet :: [ Rule ] -> Rules
makeRuleSet = id

{-| Returns the pattern variables defined within a pattern
-}
patternVariables :: Pattern -> [ PatternVariable ]
patternVariables (IdentPat _)                    = []
patternVariables (VarPat ident)                  = [ ident ]
patternVariables (CoopPat left actPat right)     =
  concat [ patternVariables left
         , actionVariables actPat
         , patternVariables right
         ]
patternVariables (ArrayPat comp size Nothing)       =
  concat [ patternVariables comp
         , expressionVariables size
         ]
patternVariables (ArrayPat comp size (Just actPat)) =
  concat [ patternVariables comp
         , actionVariables  actPat
         , expressionVariables size
         ]
patternVariables (PartEval comp)                    =
  patternVariables comp

actionVariables :: ActionsPattern -> [ PatternVariable ]
actionVariables (Actions _)      = []
actionVariables (AnyActions p _) = [p]

expressionVariables :: ExprPattern -> [ PatternVariable ]
expressionVariables (AnyExpr p)               = [ p ]
expressionVariables (ConstantPat _)           = []
expressionVariables (RealPattern _)           = []
expressionVariables (PlusPattern left right)  =
  concat [ expressionVariables left
         , expressionVariables right
         ]
expressionVariables (MinusPattern left right) =
  concat [ expressionVariables left
         , expressionVariables right
         ]
expressionVariables (MultPattern left right)  =
  concat [ expressionVariables left
         , expressionVariables right
         ]

{-| Returns the pattern variables used within a replacement -}
replacementVariables :: Replacement -> [ PatternVariable ]
replacementVariables = patternVariables


{-| Pretty print a set of rules -}
hprintRules :: Rules -> String
hprintRules = 
  Pretty.render . pprintRules

pprintRules :: Rules -> Doc
pprintRules = Pretty.vcat . (map pprintRule)

pprintRule :: Rule -> Doc
pprintRule rule =
  Pretty.sep [ pprintPattern $ rulePattern rule
             , Pretty.text "==>"
             , pprintReplacement $ ruleReplace rule
             ]

-- TODO: need to sort out bracketing.
pprintPattern :: Pattern -> Doc
pprintPattern (IdentPat name)               = 
  Print.pprintComponentName name
pprintPattern (VarPat var)                  = pprintPatternVar var
pprintPattern (CoopPat left actPat right)   = 
  Pretty.sep [ pprintPattern left
             , pprintActionPattern actPat
             , pprintPattern right
             ]
pprintPattern (ArrayPat comp size mActions) =
  Pretty.sep [ pprintPattern comp
             , Pretty.brackets $ pprintExprPattern size
             , maybe Pretty.empty 
                     (Pretty.brackets . pprintActionPattern)
                     mActions
             ]
pprintPattern (PartEval comp)               =
  Pretty.sep [ Pretty.text "PartEval"
             , Pretty.parens $ pprintPattern comp
             ]

-- TODO: need to sort out bracketing.
pprintExprPattern :: ExprPattern -> Doc
pprintExprPattern (AnyExpr var)             = pprintPatternVar var
pprintExprPattern (ConstantPat i)           = Pretty.int i
pprintExprPattern (RealPattern d)           = Pretty.double d
pprintExprPattern (PlusPattern left right)  = 
  Pretty.sep [ pprintExprPattern left
             , Pretty.text "+"
             , pprintExprPattern right
             ]
pprintExprPattern (MinusPattern left right) = 
  Pretty.sep [ pprintExprPattern left
             , Pretty.text "-"
             , pprintExprPattern right
             ]
pprintExprPattern (MultPattern left right)  = 
  Pretty.sep [ pprintExprPattern left
             , Pretty.text "*"
             , pprintExprPattern right
             ]

pprintActionPattern :: ActionsPattern -> Doc
pprintActionPattern (Actions [])          = Pretty.text "||"
pprintActionPattern (Actions actions)     =
  angled $ pprintActionList actions
pprintActionPattern (AnyActions var [])   =
  angled $ pprintPatternVar var
pprintActionPattern (AnyActions var rest) =
  angled $ Pretty.sep [ pprintPatternVar var
                      , Pretty.comma
                      , pprintActionList rest
                      ]
pprintActionList :: [ ActionIdentifier ] -> Doc
pprintActionList actions = 
  Pretty.sep $ Pretty.punctuate Pretty.comma actDocs
  where
  actDocs = map Print.pprintActionIdentifier actions

angled :: Doc -> Doc
angled d = Pretty.hcat [ Pretty.text "<"
                       , d
                       , Pretty.text ">"
                       ]

pprintPatternVar :: PatternVariable -> Doc
pprintPatternVar var = 
  Pretty.text $ '?' : (hprintQualifiedName var)

pprintReplacement :: Replacement -> Doc
pprintReplacement = pprintPattern