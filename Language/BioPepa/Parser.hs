{-| 
    A parser for Bio-PEPA model files.
-}
module Language.BioPepa.Parser
  ( parseBioPepaFile
  , biopepaModel
  , biopepaRateName
  , biopepaComponentName
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
import qualified Language.Pepa.QualifiedName as Qualified
import qualified Language.Pepa.FileParser as FileParser
import qualified Language.Pepa.Parser as PepaParser
import Language.Pepa.Parser
  ( angles
  , comma
  , symbol
  , parens
  )
import Language.BioPepa.Syntax
  ( Model          ( .. )
  , RateDef
  , RateIdent
  , RateExpr       ( .. )
  , ComponentName
  , Component      ( .. )
  , ComponentDef
  , Concentration
  , Action
  , StoCoeff
  , PrefixOp       ( .. )
  )
import qualified Language.BioPepa.Print as Print

import qualified Language.Pepa.MainControl as MainControl
import Language.Pepa.MainControl
  ( IOMainControl )
{- End of imports -}

{-| A function to parse a rules specification -}
parseBioPepaFile :: FilePath -> IOMainControl Model
parseBioPepaFile file =
  do model <- FileParser.parseFile (FileParser.fileParser biopepaModel) file
     let logKey = "parsed-biopepa-model"
         logInfo   = Print.hprintBioPepaModel model
     MainControl.liftMC $ MainControl.valueResult model logKey logInfo

{-|
    A parser for a whole biopepa model
-}
biopepaModel :: Parser Model
biopepaModel = 
  -- Currently then we insist that all the rate definitions
  -- come first but it won't be a problem to remove this
  -- restriction and do the same trick that we do for PEPA
  -- which is to split the definitions afterwards.
  do rateDefs    <- many1 rateDef
     compDefs    <- many1 componentDef
     composition <- biopepaComponent Nothing
     return Model { modelRateDefs  = rateDefs
                  , modelCompDefs  = compDefs
                  , modelSystemEqn = composition 
                  }

{-
  Note that here we do not require the same trick as
  we do with component definitions to distinguish them
  from the start of the main system equation.
  Currently this is for two reasons, firstly that the
  rate definitions must come before the component definitions
  and secondly because rate names and component names are
  distinguished by the first letter hence we cannot confuse
  the start of a rate definition (a rate name) with the start
  of the main system equation (a component name).
-}
rateDef :: Parser RateDef
rateDef =
  do ident <- biopepaRateName
     symbol "="
     comp  <- PepaParser.squares biopepaRateExpr
     symbol ";"
     return (ident, comp)


{-
  Parsing of a rate expression.
-}
biopepaRateExpr :: Parser RateExpr
biopepaRateExpr = 
  buildExpressionParser rateTable rateFactor
  <?> "Rate expression"
  where 
  rateTable     = [ [ Infix multRate AssocLeft
                    , Infix divRate  AssocLeft
                    ]
                  , [ Infix plusRate AssocLeft
                    , Infix minusRate AssocLeft
                    ]
                  ]
  multRate :: Parser (RateExpr -> RateExpr -> RateExpr)
  multRate = symbol "*" >> (return RateMult)

  divRate :: Parser (RateExpr -> RateExpr -> RateExpr)
  divRate = symbol "/" >> (return RateDiv)

  plusRate :: Parser (RateExpr -> RateExpr -> RateExpr)
  plusRate = symbol "+" >> (return RateAdd)

  minusRate :: Parser (RateExpr -> RateExpr -> RateExpr)
  minusRate = symbol "-" >> (return RateSub)

  rateFactor :: Parser RateExpr
  rateFactor =
    Parsec.choice [ liftM RateName biopepaComponentName
                  , liftM RateName biopepaRateName
                  , liftM RateConstant PepaParser.forgivingFloat
                  , parens biopepaRateExpr
                  ]

{-
  Parsing a component def. Note the use of 'Parsec.try' 
  to begin the definition. The reason is that a process
  definition looks just like a component until we get to
  the equals sign. So we parse the component name and if
  an equals sign follows then we have a component definition
  and may continue. Otherwise we must be in the main system
  equation hence we backtrack and end the list of definitions
  and the main system equation will be able to parse from the
  beginning including the component name.

  We could achieve a similar effect without the use of 'Parsec.try'
  but it does get quite ugly.
  @           
-}
componentDef :: Parser ComponentDef 
componentDef = 
  do ident <- Parsec.try beginComponentDef
     comp  <- biopepaComponent $ Just ident
     symbol ";"
     return (ident, comp)
  where
  beginComponentDef :: Parser ComponentName
  beginComponentDef = do ident <- biopepaComponentName
                         symbol "="
                         return ident

 {- 
  The parser for biopepa components. This must necessarily be
  a little more forgiving than we would like it to be because we
  must accept both parallel and sequential definitions.
-}
biopepaComponent :: Maybe ComponentName -> Parser Component
biopepaComponent mDefName = 
  buildExpressionParser compTable compFactor
  <?> "Bio-PEPA component"
  where
  -- I think actually this is not the way to do this.
  -- Basically I think the forms of definitions are much
  -- more rigid for biopepa than for pepa so we can detect
  -- early on if we have a sequential or parallel definition
  -- so actually I think we *can* have separate types for them.
  -- Although I suppose we should worry about alias definitions.
  compTable     = [ [ Infix compSum AssocLeft
                    , Infix (twoBars <|> angledActions) AssocLeft 
                    ]
                  ]



  prefixOperator :: Parser PrefixOp
  prefixOperator = 
    Parsec.choice [ do PepaParser.reserved "<<"
                       return Reactant
                  , do PepaParser.reserved ">>"
                       return Product
                    -- Note that for these ones I do not use
                    -- PepaParser.parens, that would allow (  . )
                    -- which I think is wrong.
                  , do PepaParser.reserved "(.)"
                       return GenModifier
                  , do PepaParser.reserved "(+)"
                       return Activator
                  , do PepaParser.reserved "(-)"
                       return Inhibitor
                  ]

  compSum :: Parser (Component -> Component -> Component)
  compSum = do symbol "+"
               return Choice

  -- Some pepa tools force @P || P@ to mean cooperation
  -- over the empty set of actions, so we allow this
  -- in addition to @P <> P@ just so that a model does not
  -- need to be written twice.
  twoBars :: Parser ( Component -> Component -> Component)
  twoBars = do symbol "||"
               return $ makeCoop []

  angledActions :: Parser ( Component -> Component -> Component)
  angledActions = liftM makeCoop $ angles actionList

  actionList :: Parser [ Action ]
  actionList = sepBy PepaParser.pepaActionId comma

  makeCoop :: [ Action ] -> Component -> Component -> Component
  makeCoop actions left = Cooperation left actions

  compFactor  = ( Parsec.choice [ nameAndConc
                                , compPrefix
                                , parens $ biopepaComponent mDefName
                                ]
                ) <?> "simple component"

  nameAndConc :: Parser Component
  nameAndConc = do ident  <- biopepaComponentName
                   mTail  <- optionMaybe $ parens concentration
                   return $ addConcTail ident mTail
 
  addConcTail :: ComponentName -> Maybe Concentration -> Component
  addConcTail ident Nothing  = Named ident
  addConcTail ident (Just c) = CompConcent ident c

  concentration :: Parser Concentration
  concentration = PepaParser.forgivingFloat


  -- There are two types of short-hand for a prefix component.
  -- First of all, simply an action name is shorthand for
  -- the one stoichiometry and can be written without the parentheses
  -- so "rw" is the same as "(rw,1)"
  -- Secondly if the name to change is left off we assume it is the
  -- name of the component being defined so
  -- "E = (rw,1)<<" is shorthand for "E = (rw,1)<<E"
  -- Hence combining the two we can write "E = rw<<"
  compPrefix :: Parser Component
  compPrefix = 
    do (action, stoich) <- actionAndStoich
       oper             <- prefixOperator
       name             <- shorthandName
       return $ PrefixComp action stoich oper name

  -- See the comment above but I'm pretty unhappy with using 'Parsec.try'
  -- here. This is required because the start of a prefix looks like
  -- just an ordinary component parenthesis. That is for example
  -- (P <> S) or (P + S).
  actionAndStoich :: Parser (RateIdent, StoCoeff)
  actionAndStoich =
    (Parsec.try longhandActionAndStoich) <|> shorthandActionAndStoich
  longhandActionAndStoich :: Parser (RateIdent, StoCoeff)
  longhandActionAndStoich =
    do symbol "("
       action <- PepaParser.pepaActionId
       comma
       stoich <- stoichiometry
       symbol ")"
       return (action, stoich)
  shorthandActionAndStoich :: Parser (RateIdent, StoCoeff)
  shorthandActionAndStoich =
    do action <- PepaParser.pepaActionId
       return (action, "1")
    
  -- Obviously this is incorrect at the moment.
  stoichiometry :: Parser StoCoeff
  stoichiometry = do i <- PepaParser.intParser
                     return $ show i

  shorthandName :: Parser ComponentName
  shorthandName =
    biopepaComponentName
    <|> ( case mDefName of
            Just p  -> return p
            Nothing -> fail "Shorthand only available in definitions" )


-- Note this will allow white space between the colons
-- and the parts of the name, not sure that I want this?
biopepaComponentName :: Parser ComponentName
biopepaComponentName = 
  do ident     <- PepaParser.pepaComponentId
     Parsec.option ident $ do symbol ":"
                              ident2 <- biopepaComponentName 
                              return $ Qualified.concatQNames ident ":" ident2

biopepaRateName :: Parser RateIdent
biopepaRateName = PepaParser.pepaRateId