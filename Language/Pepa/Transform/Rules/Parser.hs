{-| 
    A parser for PEPA-to-PEPA rewrite rules
-}
module Language.Pepa.Transform.Rules.Parser
    ( parseRulesFile
    , parseRule
    , rulesParser
    , ruleParser
    )
where

{- Standard Libraries modules imported -}
import Control.Monad
  ( liftM )
import Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec
  ( Parser )
import Text.ParserCombinators.Parsec.Expr
  ( buildExpressionParser
  , Assoc     ( .. )
  , Operator  ( .. )
  )
{- External Library modules imported -}
{- Local modules imported imported -}
import qualified Language.Pepa.QualifiedName as QualifiedName
import qualified Language.Pepa.FileParser as FileParser
import qualified Language.Pepa.Parser as PepaParser
import Language.Pepa.Parser
  ( angles
  , comma
  , symbol
  , parens
  , braces
  , squares
  )
import Language.Pepa.Transform.Rules.Syntax
  ( Rules
  , Rule           ( .. )
  , ComponentName
  , Pattern        ( .. )
  , ActionsPattern ( .. )
  , PatternVariable
  , ExprPattern    ( .. )
  , Replacement
  )
import Language.Pepa.MainControl
  ( MainControl
  , IOMainControl
  )
{- End of imports -}

{-| A function to parse a rules specification -}
parseRulesFile :: FilePath -> IOMainControl Rules
parseRulesFile = FileParser.parseFile rulesParser

parseRule :: String -> MainControl Rule
parseRule = FileParser.mainControlParse ruleParser

rulesParser :: Parser Rules
rulesParser = Parsec.sepBy ruleParser comma

ruleParser :: Parser Rule
ruleParser = do pattern <- patternParser
                ruleArrow
                replace <- replaceParser
                return Rule { rulePattern = pattern
                            , ruleReplace = replace
                            }

-- The parser for patterns on the left hand side of rules.
patternParser :: Parser Pattern
patternParser = 
  buildExpressionParser compTable compFactor
  <?> "Rule Pattern"
  where 
  compTable     = [ [ Postfix arrayTail ]
                  , [ Infix (twoBars <|> angledActions) AssocLeft ]
                  ]

  -- Some pepa tools force @P || P@ to mean cooperation
  -- over the empty set of actions, so we allow this
  -- in addition to @P <> P@ just so that a model does not
  -- need to be written twice.
  twoBars :: Parser ( Pattern -> Pattern -> Pattern)
  twoBars = do symbol "||"
               return $ makeCoop (Actions [])

  angledActions :: Parser ( Pattern -> Pattern -> Pattern)
  angledActions = liftM makeCoop $ angles actionList

  -- There are three common cases here:
  -- <?a> is just a pattern which matches any list and we should return
  -- AnyActions pvar []
  -- <a,b> is just a list of actions which we must match precisely this
  -- should return Actions [a,b], note that this is true even for <>
  -- where we would return Actions []
  -- <?a, a, b> in which case we wish to return 
  -- AnyActions pvar [a,b]
  actionList :: Parser ActionsPattern
  actionList = 
    ( do pvar    <- patternVariable
         -- Note the comma to begin the optional parser, we need this
         -- to separate the pattern variable from the list of actions
         actions <- option [] $ do comma
                                   sepBy PepaParser.pepaActionId comma
         return $ AnyActions pvar actions
    )
    <|>
    (liftM Actions $ sepBy PepaParser.pepaActionId comma)

  makeCoop :: ActionsPattern -> Pattern -> Pattern -> Pattern
  makeCoop actions left = CoopPat left actions

  arrayTail :: Parser (Pattern -> Pattern)
  arrayTail = do symbol "["
                 size <- exprPatternParser
                 end  <- (do comma
                             actions <- optionMaybe $ braces actionList
                             symbol "]"
                             return actions
                         ) <|>
                         (do symbol "]"
                             optionMaybe $ squares actionList
                         )
                 return $ addArrayTail size end

  addArrayTail :: ExprPattern -> Maybe ActionsPattern -> Pattern -> Pattern
  addArrayTail i a p = ArrayPat p i a

  {- Unfortunately we have to use 'try' here for the partialEvaluation.
     The reason is that the name 'PartEval' could actually be used as
     a process identifier. Certainly a prefix of it could, so a process
     named 'P' so if we attempt to parse partialEvaluation then we will
     fail after having consumed the 'P' so we must backtrack to there
     to re-attempt to parse it as something else.
  -}
  compFactor  = Parsec.choice [ try partialEvaluation 
                              , namePattern
                              , liftM VarPat patternVariable
                              , parens patternParser
                              ]
                <?> "simple pattern"
  namePattern :: Parser Pattern
  namePattern = liftM IdentPat ruleComponentName
 
  partialEvaluation :: Parser Pattern
  partialEvaluation = 
    PepaParser.reserved "PartEval" >> 
    (liftM PartEval $ PepaParser.parens patternParser)
                      

-- We need an expression pattern parser. These parse the size
-- in a process array, it basically looks like an expression
-- parser except that we can have as a factor a pattern variable.
exprPatternParser :: Parser ExprPattern
exprPatternParser =
  buildExpressionParser exprTable exprFactor
  <?> "Expression Pattern"
  where 
  exprTable     = [ [ Infix multOper AssocLeft ]
                  , [ Infix plusOper AssocLeft
                    , Infix minusOper AssocLeft
                    ]
                  ]
  multOper :: Parser (ExprPattern -> ExprPattern -> ExprPattern)
  multOper = symbol "*" >> (return MultPattern)

  minusOper :: Parser (ExprPattern -> ExprPattern -> ExprPattern)
  minusOper = symbol "-" >> (return MinusPattern)

  plusOper :: Parser (ExprPattern -> ExprPattern -> ExprPattern)
  plusOper = symbol "+" >> (return PlusPattern)
  
  -- The factors of an expression
  exprFactor = Parsec.choice [ liftM AnyExpr patternVariable
                             , exprNumber
                             ]
  exprNumber = liftM numberExp PepaParser.naturalOrFloat
  numberExp :: Either Integer Double -> ExprPattern
  numberExp (Left i)  = ConstantPat $ fromIntegral i
  numberExp (Right d) = RealPattern d


-- A pattern variable parser
-- Notice that we provide "_" as a synonym for ?"" essentially this
-- means: match anythign but I whatever it is I won't use in the
-- replacement anyway so you can basically forget what matched it.
patternVariable :: Parser PatternVariable
patternVariable =
  (symbol "_" >> (return $ QualifiedName.unqualified ""))
  <|>
  (symbol "?" >> (liftM QualifiedName.unqualified PepaParser.upperOrLowerId))

-- The parser for replacements on the right hand side of rules
replaceParser :: Parser Replacement
replaceParser = patternParser

{- The parser for a component name within a rule -}
ruleComponentName :: Parser ComponentName
ruleComponentName = PepaParser.pepaComponentId 


{- | Rules generally take "something" to "something else".
     So a rule generally has an arrow separating the pattern
     to match against to the pattern with which to replace it.
-}
ruleArrow :: Parser String
ruleArrow = PepaParser.symbol "==>"