{-|
  This is the new parser module built using the @parsec@ parser
  cominator library.
  I think the option parsers should probably be moved to L.P.FileParser
-}
module Language.Pepa.Parser
  ( parseRateArrayOption
  , parseRateOption
  , parseActionArrayOption
  , parseCommaSeparatedLowers
  , parseRateExprOption
  , blindRun
  , wholeParser
   
  -- * Individual parser definitions
  , pepaModel
  , pepaComponent
  , pepaBeginProcessDef
  , pepaComponentDef
  , pepaActionId
  , pepaComponentId
  , pepaRateId
  , pepaRateDefId
  , pepaRate
  , pepaRateExp
  , exprParser
  , rateExprIdent
   
  -- * Utility functions or parsers
  , maybeOption
  , forgivingFloat

  -- * Identifier parsers
  , nameSpace
  , upperId
  , lowerId
  , upperOrLowerId
  , wholeNameSpaceId

  -- * The token parser parsers which use the lexer specific to pepa
  , whiteSpace
  , reserved
  , reservedOp
  , intParser
  , symbol
  , stringLiteral
  , comma
  , braces
  , angles
  , parens
  , natural
  , naturalOrFloat
  , squares
  )
where

{- Standard imported libraries -}
import Control.Monad
  ( liftM )
import qualified Data.Maybe as Maybe
import Text.ParserCombinators.Parsec
  ( Parser
  , CharParser
  , GenParser
  , parse
   
  , ( <|> )
  , ( <?> )
  , try
   
  , eof
  , alphaNum
  , char
  , upper
  , lower
   
  , optional
  , many
  , many1
  , sepBy
  , sepBy1
  , notFollowedBy
  )
import qualified Text.ParserCombinators.Parsec.Token as StdToken
import Text.ParserCombinators.Parsec.Expr
  ( buildExpressionParser
  , Assoc     ( .. )
  , Operator  ( .. )
  )
import Text.ParserCombinators.Parsec.Language
  ( haskellStyle )

{- Local imported libraries -}
import qualified Language.Pepa.PrivateTokenParser as PrvToken

import Language.Pepa.QualifiedName
  ( QualifiedName     ( .. ) )
import Language.Pepa.Rates
  ( RateIdentifier
  , RateExpr          ( .. )
  , Rate              ( .. )
  )
import qualified Language.Pepa.Syntax as Syntax
import Language.Pepa.Syntax 
  ( ActionIdentifier
  , ParsedModel       ( .. )

  , ParsedDefinition  ( .. )
  , ParsedComposition
  , ParsedComponent   ( .. )
  , CooperationSet    ( .. )
  , Transition        ( .. )
  , ParsedTrans
  , ParsedPriority
  , defaultPepaPriority
  , ParsedComponentId
  , ParsedRate
  , ParsedAction      ( .. )
  , nameOfAction
  )
{- End of imports -}


{-|
  Blindly runs the given parser expecting it to succeed.
  The first argument is the error string to give to the failure
  function 'error' in the event that the parser fails. 
  This function is most useful for simple parsers within the program
  such as the parsing of command line options.
  Note that the actual parse error returned from the parser itself is
  ignored, hence the user will not recieve a potentially useful message
  such as 'expecting open bracket'.
-}
blindRun :: String -> Parser a -> String -> a
blindRun err p input = 
  case (parse p "" input) of
    Left err2   -> error (err ++ ": " ++  show err2)
    Right x     -> x

{-|
  When parsing options or fields from web or gui inputs you need to
  be sure of parsing the whole string. In addition you need to be
  sure of ignoring leading white space. This turns a simple parser
  into such a parser. It is therefore good for turning parsers meant
  for part of a whole file parser (for example an expression parser)
  into one suitable for option/field parsing.
-}
wholeParser :: Parser a -> Parser a
wholeParser parser =
  do whiteSpace
     x <- parser
     eof
     return x


{-
  We now begin with the utility parsers that are used out with the
  main parser for pepa files. These include for example the parsing
  of command line options.
  Parses the rate command line option argument, which is of the form
  @ r=0.5,1.5,6.9 @
  the integer given as the first argument is used to select which
  value of the comma separated list of values is returned.
-}

{-|
  The action list parser will produce a list of parsed actions but
  these are not just identifiers, so we have to map them into identifiers.
  We probably should store them in the command line options as parsed actions.
-}
parseActionArrayOption :: String -> [ ActionIdentifier ]
parseActionArrayOption input =
   map nameOfAction actions
   where
   err     = "error parsing a pepa action list option"
   actions = blindRun err pepaActionList input


{-|
   Parses in a list of comma separated names which all begin
   with a lower case letter.
   The first argument is the error to give the second
   the input.
-}
parseCommaSeparatedLowers :: String -> String -> [ String ]
parseCommaSeparatedLowers err =
   blindRun err $ sepBy lowerId comma



{-|
  In contrast to that below, when parsing a rate option
  (as opposed to a rate array option) there is only one
  double to return rather than a list of double from which to 
  choose using the process identifier.
 -} 
parseRateOption :: String -> (RateIdentifier, Double)
parseRateOption input =
    blindRun err rateOptionParser input
    where err = "error parsing a rate constant option"


rateOptionParser :: Parser (RateIdentifier, Double)
rateOptionParser = 
  do ident  <- pepaRateId
     char '='
     number <- forgivingFloat
     return (ident, number)

{-|
  Allows the parsing of a rate option where the rate is set to a
  rate expression rather than a simple constant.
-}
parseRateExprOption :: String -> (RateIdentifier, RateExpr)
parseRateExprOption input =
    blindRun err rateExprOptionParser input
    where err = "error parsing a rate expression option"


rateExprOptionParser :: Parser (RateIdentifier, RateExpr)
rateExprOptionParser = 
  do ident <- pepaRateId
     char '='
     expr  <- pepaRateExp
     eof
     return (ident, expr)

{-|
   Parse a rate array option, such as
   @ --rates r=0.1,0.2,0.3,0.4,0.5 @
-}
parseRateArrayOption :: String -> (RateIdentifier, [Double])
parseRateArrayOption input = 
    blindRun err rateArrayOptionParser input
    where err = "error parsing a rate constant array option"


rateArrayOptionParser :: Parser (RateIdentifier, [Double])
rateArrayOptionParser = 
  do ident   <- pepaRateId
     char '='
     numbers <- rateNumberList
     return (ident, numbers)

{-
  The rate number list can either be a list of floating point numbers
  separated by commas, or a stepped number list.
  So it looks like either 
  @ r=0.2,0.4,0.6,0.8 @
  or
  @ r=0.2..0.8:0.2 @
  So to parse this we have to parse the first number, since in either case
  we are expecting a number. After this we can decide, if we see a comma then
  a comma list is following if we see a dot then a stepped list is following.
  Note that the first number is passed in to create the remainder parsers and
  that is why we needn't return it in the return expression because it will
  already be incorporated into the results of the two parsers.
-}
rateNumberList :: Parser [Double]
rateNumberList = 
  do first      <- floatNumber
     ( steppedNumberList first) <|> ( commaFloatNumberList first )

{-
  There is a known bug involved with this, it comes from how Haskell
  does double addition. It comes from the representation of doubles
  in particular that of @0.2@. The upshot is that something like
  @0.1 + 0.2@ produces @0.3000000000000004@, which obviously looks
  very bad as a result. I will fix this using a floor function to take
  the number to the nearest say six decimal places.
-}
steppedNumberList :: Double -> Parser [Double]
steppedNumberList start = 
  do char '.'
     char '.'
     end   <- floatNumber
     char ':'
     step  <- floatNumber
     return (computeNumberList start end step)
  where 
  computeNumberList :: Double -> Double -> Double -> [Double]
  computeNumberList current end step
    | current > end = []
    | otherwise     = 
      current : (computeNumberList (current + step) end step)








{-
    A parser for a whole pepa model.
-}
pepaModel :: Parser ParsedModel
pepaModel = 
  do definitions          <- pepaDefinitionList
     composition          <- pepaComponent
     let rateSpecs    = Syntax.filterRateSpecifications definitions
         processDefs  = Syntax.filterProcessDefintions  definitions
         virtualComps = Syntax.filterVirtualComps       definitions
     return ParsedModel { modelRateSpecs    = rateSpecs
                        , modelProcessDefs  = processDefs
                        , modelVirtualComps = virtualComps
                        , modelSystemEqn    = composition
                        }

pepaDefinitionList :: Parser [ ParsedDefinition () ]
pepaDefinitionList = many1 pepaDefinition



{-
  A pepa model definition, basically can be either a rate definition,
  a process concentration definition or the definition of a 
  sequential process. Currently though we parse process concentration
  definition as a rate definition because they are indistinguishable
  from the point of view of the parser and sort it out in the semantic
  analysis. But see below that I think it would be nice to separate
  them out syntactically.
-}
pepaDefinition :: Parser (ParsedDefinition ())
pepaDefinition = 
  followedBy (pepaRateDef <|> pepaComponentDef) $ symbol ";" 

{-
  Process concentration definitions are parsed as rate definitions
  and we sort it out after parsing, the reason is that it is a bit
  awkward to distinguish between the two, importantly because
  the identifiers are of the same style. It may be worth considering
  changing this actually. We could for example have a syntax something
  like [-concentration p = 0-], unfortunately concentration seems like
  a rather long key word, perhaps instead [-rate r = 0.1-].
-}
pepaRateDef :: Parser (ParsedDefinition a)
pepaRateDef = 
  do ident <- pepaRateDefId
     symbol "="
     expr  <- pepaRateExp
     return $ RateDef (ident, expr)

{-
  A parser for a component definition, so basically a component
  identifier set equal to a *sequential* component expression.

  Note the slightly questionable use of the [-try-] parser
  combinator here. The purpose is that the main composition at
  the end may not be surrounded by parentheses. It may for example
  be [-P <> P-], if it is, and we didn't use [-try-] to parse the
  identifier and the [-=-] sign then *this* parser would successfully
  parse the [-P-] identifier and then fail because there is no [-=-]
  sign. This would cause the [-pepaModel-] parser to come out of the
  [-many1 pepaDefinition-] and attempt to parse the main composition,
  *however* the input [-P-] has already been 'eaten' by this parser.
  Using [-try-] here means that if we fail because the [-=-] sign is
  not present then we will put the [-P-] (or whatever the component
  identifier is) back on to the input and allow the main composition
  to be parsed.
-}
pepaComponentDef :: Parser (ParsedDefinition a)
pepaComponentDef = 
  do ident     <- pepaBeginProcessDef
     let realDef    = do component <- pepaComponent
                         return $ ProcessDef (ident, component)
         virtualDef = do rexpr     <- squares pepaRateExp
                         return $ VirtualDef (ident, rexpr)
     realDef <|> virtualDef

pepaBeginProcessDef :: Parser ParsedComponentId
pepaBeginProcessDef = 
  try $ do optional $ symbol "#"
           ident <- pepaProcessDefId
           symbol "="
           return ident


{-
   Parser for components.
   I'm now not so sure that 'buildExpressionParser' is the way to go
   here because of the slight difficulty of parsing a prefix
   component. Also doing this means that we allow the parsing of
   for example @ (a,r).(P <> P) @ which is obvious nonsense.
   I think the better way to go is build up the parser myself,
   for now though this isn't working too badly.
   Also I tried putting in a 'Prefix' line in the @compTable@,
   in order to parse prefix components rather than having them
   as a @compFactor@ but I couldn't get that to work.

   UPDATE: I think now the better way to go is to just have two
   separate parsers one for prefix and one for parallel.
   The problem is that in general both are acceptable, since
   we have parallel definitions.
-}
pepaComponent :: Parser ParsedComposition
pepaComponent =
  buildExpressionParser compTable compFactor
  <?> "Pepa component"
  where 
  compTable     = [ [ Postfix arrayTail ]
                  , [ Infix compSum AssocLeft ]
                  , [ Postfix hideTail ]
                  , [ Infix (twoBars <|> angledActions) AssocLeft ]
                  ]

  -- Some pepa tools force @P || P@ to mean cooperation
  -- over the empty set of actions, so we allow this
  -- in addition to @P <> P@ just so that a model does not
  -- need to be written twice.
  twoBars :: Parser (ParsedComponent -> ParsedComponent -> ParsedComponent)
  twoBars = do symbol "||"
               return $ makeCoop (ActionSet [])

  angledActions :: Parser ( ParsedComponent -> ParsedComponent 
                         -> ParsedComponent)
  angledActions = 
    angles $ (do reserved "*"
                 return $ makeCoop WildCard
             ) <|>
             (do actions <- pepaActionList
                 return $ makeCoop (ActionSet actions)
             )

  makeCoop :: CooperationSet -> ParsedComponent 
           -> ParsedComponent -> ParsedComponent
  makeCoop coopSet left = Cooperation left coopSet
             
  compSum :: Parser (ParsedComponent -> ParsedComponent -> ParsedComponent)
  compSum = do symbol "+"
               return ComponentSum
      
  hideTail = do symbol "/"
                actions <- braces pepaActionList
                return $ makeHide actions

  makeHide :: [ParsedAction] -> ParsedComponent -> ParsedComponent
  makeHide actions left = Hiding left actions 

  compFactor  = (try pepaConditional)
                <|> (try pepaPrefix)
                <|> stopProcess
                <|> processName
                <|> parens pepaComponent
                <?> "simple process composition"

  stopProcess :: Parser ParsedComponent
  stopProcess = do reserved "Stop"
                   return StopProcess

  processName :: Parser ParsedComponent
  processName = do ident <- pepaComponentId
                   return $ IdProcess ident

  -- Okay I'm not completely sure about having 'pepaComponent' as the
  -- second alternative to a 'pepaPrefix'.
  -- As you can see I previously had just a simple
  -- ident component there, but I would quite like to allow things such as
  -- @ (r, 1.0).(P + Q) @
  -- I think the use of 'try' here *may* be very inefficient.
  pepaPrefix :: Parser ParsedComponent
  pepaPrefix = 
    do mTrans  <- pepaTrans
       symbol "."
       right   <- ((try pepaPrefix) <|> compFactor)
       return $ PrefixComponent mTrans right

  -- Pepa conditionals are simple if-thens, note no else.
  pepaConditional :: Parser ParsedComponent
  pepaConditional =
    do reserved "if"
       rCond         <- exprParser
       reserved "then"
       right         <- ((try pepaPrefix) <|> compFactor)
       return $ CondBehaviour rCond right

  {- 
    A process tail is only afixable to a process name and allows
    the user to specify an array of processes. In addition to the number
    of processes to run in parallel the user can specify a list of actions
    on which the processes should cooperate. There are two styles for this
    @ P[3][a,b] @ is an array of 3 @P@ processes cooperating over
    the actions @a@ and @b@.
    @ P[3, {a, b}] @ is an alternative style for the same thing.
  -}
  arrayTail :: Parser (ParsedComponent -> ParsedComponent)
  arrayTail = do symbol "["
                 size <- exprParser
                 end  <- (do comma
                             actions <- maybeOption $ braces pepaActionList
                             symbol "]"
                             return actions
                         ) <|>
                         (do symbol "]"
                             maybeOption $ squares pepaActionList
                         )
                 return $ addArrayTail size end

  addArrayTail :: RateExpr -> Maybe [ ParsedAction ] 
               -> ParsedComponent -> ParsedComponent
  addArrayTail size mActs proc = ProcessArray proc size mActs
        
{-
  A parser for a pepa transitions.
  The first parser accepts the standard @(a, r)@ still of transition,
  however we can also put in just a single action name (which is
  lowercase so should not conflict with process identifiers) this is
  an immediate action and is shorthand for @(a, immediate)@
-}
pepaTrans :: Parser ParsedTrans
pepaTrans = 
  normalBracketed  -- so a normal bracketed (a,r) style transition
  <|>
  quickTrans       -- or a single action identifier which will be performed
                   -- as an immediate action, ag @ tau.P @
  where
  normalBracketed :: Parser ParsedTrans 
  normalBracketed =
    parens $ do action   <- pepaAction
                priority <- maybeOption pepaPriority
                comma
                rate     <- pepaRate
                return $ makeTrans action priority rate

  quickTrans :: Parser ParsedTrans
  quickTrans =
    do action   <- pepaAction
       priority <- maybeOption pepaPriority
       return $ makeTrans action priority ( RateImmediate $ Creal 1 )

  makeTrans :: ParsedAction -> Maybe ParsedPriority
            -> ParsedRate -> ParsedTrans
  makeTrans action mPriority rate =
    Transition { pepaTransAction     = action
               , pepaTransCoalsced   = []
               , pepaTransPriority   = priority
               , pepaTransRate       = rate
               , pepaTransConditions = []
               }
    where
    priority = Maybe.fromMaybe defaultPepaPriority mPriority
               

{- 
  Parses a priority which may be affixed to the end of a 
  (semi-markov) pepa action 
-}
pepaPriority :: Parser ParsedPriority
pepaPriority = squares intParser


{-
   Parses a rate. Unfortunately we have to use 'try'
   This is because both immediate and infty may actually be rate expressions.
   The simple case they might must be named longer, eg it might be the rate
   expression: @immediate_rate * r1@ if we didn't use 'try' then the
   @rateImm@ parser would consume the word @immediate@.
   Similarly for @rateTop@ it would consume @infty@  if confronted with
   the rate @infty_like_rate@.
-}
pepaRate :: Parser ParsedRate
pepaRate =
   (try rateTop)
   <|> 
   (try rateImm)
   <|>
   timed
   where
   rateTop :: Parser ParsedRate
   rateTop = do (reserved "infty" <|> reserved "_" <|> reserved "T" )
                mWeight <- maybeOption pepaRateExp
                let weight = Maybe.fromMaybe (Creal 1.0) mWeight 
                return $ RateTop weight 

   -- It may look like we can easily combine rate immediate and
   -- timed rate, just parse a rate expression and then detect if
   -- there is a tail on the end. But we may have an immediate
   -- rate without a weighting. Such as "(a, immediate) . P"
   rateImm :: Parser ParsedRate
   rateImm = do reserved "immediate"
                colon
                mWeight <- maybeOption pepaRateExp
                let weight = Maybe.fromMaybe (Creal 1.0) mWeight
                return $ RateImmediate weight

   timed :: Parser ParsedRate
   timed = liftM RateTimed pepaRateExp

{-
  We build a parser for rate expressions using parsec's expression
  parser building library. Best really to look at the parsec
  documentation, but briefly the [-buildExpressionParser-] takes
  two arguments, the first is a table of operators where each row
  represents an operator precedence. The second argument is the parser
  for the basic terms, that is non-decomposable terms.

  This is quite good, it does not allow an immediate rate to be part of
  a binary rate expression, so for example one cannot have
  @ r * 2:immediate @ as this is obvious nonsense. However because the
  @condRate@ parser explictly calls 'pepaRateExp' rather than being built
  up from the @rateTable@ it is still possible to have something like:
  @ if P then 2:immediate else 1:immediate @
  which might be useful is perhaps a bit difficult to compile.
-}
pepaRateExp :: Parser RateExpr
pepaRateExp = exprParser


{- 
  This parses a small sub-set of C expressions which should be okay for rate
  expressions. It allows both upper and lower cased identifiers.
-}
exprParser :: Parser RateExpr
exprParser =
  (buildExpressionParser exprTable exprFactor )
  <?> "expression"
  where
  exprTable   = [ [ Prefix cNot ]
                , [ op "*" Cmult AssocLeft
                  , op "/" Cdiv  AssocLeft 
                  ]
                , [ op "+" Cadd  AssocLeft
                  , op "-" Csub  AssocLeft 
                  ]
                , [ op ">"  Cgt  AssocLeft
                  , op ">=" Cge  AssocLeft
                  , op "<"  Clt  AssocLeft
                  , op "<=" Cle  AssocLeft
                  , op "==" Ceq  AssocLeft
                  , cNotEqual
                  ]
                , [ op "||" Cor  AssocLeft ]
                , [ op "&&" Cand AssocLeft ]
                ]

  op :: String -> (RateExpr -> RateExpr -> RateExpr)
        -> Assoc -> Operator Char () RateExpr
  op opString f = Infix (reservedOp opString >> return f)

  -- We allow "exp != exp" as a synonym for
  -- "!(exp == exp)" because this is actually quite
  -- convenient.
  cNotEqual :: Operator Char () RateExpr
  cNotEqual = Infix ( do reservedOp "!="
                         return noEq) AssocLeft
              where
              noEq :: RateExpr -> RateExpr -> RateExpr
              noEq r1 = Cnot . Ceq r1

  cNot :: Parser (RateExpr -> RateExpr)
  cNot = (reservedOp "!") >> (return Cnot)

  -- Hmm, actually think that both of if then else and the min
  -- functions should be a 'try'
  exprFactor  = parens exprParser
                <|> rateIfThenElse
                <|> minfunction
                <|> (liftM Cident rateExprIdent)
                <|> exprNumber
                <?> "simple expression"



  -- Parses a conditional rate
  rateIfThenElse :: Parser RateExpr
  rateIfThenElse =
    do reserved "if"
       rCond         <- exprParser
       reserved "then"
       left         <- exprParser
       reserved "else"
       right        <- exprParser
       return $ Cifte rCond left right

  minfunction :: Parser RateExpr
  minfunction =
    do reserved "min"
       parens $ do left  <- exprParser
                   comma
                   right <- exprParser
                   return $ Cminimum left right

exprNumber :: Parser RateExpr
exprNumber =  
  liftM doubleOfEitherNum naturalOrFloat
  where 
  doubleOfEitherNum :: Either Integer Double -> RateExpr
  doubleOfEitherNum (Left i)  = Cconstant $ fromIntegral i
  doubleOfEitherNum (Right d) = Creal d


-- | Parses a name which maybe used within a rate expression.
-- Note that that means it may be a component name since it may
-- be a functional rate. 
-- This in turn means that it may be a string literal.
rateExprIdent :: Parser QualifiedName
rateExprIdent = pepaRateId <|> pepaComponentId

{-
  A pepa action should be either an action identifier or a tau of
  an action identifier, for now though only action identifiers
  are parsed.
-}
pepaAction :: Parser ParsedAction
pepaAction = 
  (liftM Tau (symbol "`" >> pepaActionId ))
  <|>
  (liftM Action pepaActionId)

{-
  Pepa action list, separated by commas.
-}
pepaActionList :: Parser [ParsedAction]
pepaActionList = sepBy pepaAction comma


{-
  The parsers for the simple elements of a pepa file. By that I mean here is
  where we define what a rate identifier, and a process identifier is,
  for example.
 -}

{- 
   Rate identifiers begin with a lower case letter.
   We also take this opportunity to prefix the rate with a name.
   @todo{Rate identifiers should of course be stored as 
   'Language.Pepa.Syntax.QualifiedName' that way we can give good error
   messages (and come to think of it logging information).
     
   Note: if/once we update things so that we do not syntactically
   separate rate names from process names (ie rate names *could* be
   upper case) then this would become a slightly simpler parser
   since the option bit could be the tail.
-}
pepaRateId :: Parser RateIdentifier
pepaRateId = 
     -- attempt to parser an upper name followed by the
     -- namespace separator ::
  do namespace <- maybeOption $ followedBy upperId coloncolon
     case namespace of
       -- If we couldn't  do that then just parse a lower case
       -- rate name.
       Nothing -> liftM Unqualified lowerId
       -- if we could recursively parse a possible namespace
       -- rate name.
       Just p  -> liftM (NameSpaceId p) pepaRateId

{-
   But if it is the start of a definition then it can of course only
   be a non-namespaced name.
-}
pepaRateDefId :: Parser RateIdentifier
pepaRateDefId =
  liftM Unqualified lowerId

{-| Action identifiers begin with a lower case letter -}
pepaActionId :: Parser ActionIdentifier
pepaActionId = liftM Unqualified (lowerId <|> stringLiteral)

{-| Component identifiers begin with an upper case letter
    This is currently equal to a name space, meaning that you can
    use srmc component names in a pepa model but they do NOT have
    the same meaning.
-}
pepaComponentId :: Parser ParsedComponentId
pepaComponentId = nameSpace



{-
     Similarly as for rate definitions, for component definitions the
     name must be a non-namespaced one.
-}
pepaProcessDefId :: Parser ParsedComponentId
pepaProcessDefId =
  liftM Unqualified ( upperId <|> stringLiteral )


{-
  This subsection contains a few generally useful parsers.
  These include things like a lowercase identifier. 
  The main parser can then use these by for example setting a rate identifier
  to be a lowercase identifier.

  Note that below we also allow single colons, but I think that will only
  ever be for names beginning with an upper case letter. The reason is
  that we use colon for labels in probes so an activity name cannot
  contain a colon other wise the probe
  a:start, b:stop
  will be parsed as just two names in sequences, ie
  "a:start", "b:stop".

 -}
alphaNumUnderScore :: Parser Char
alphaNumUnderScore = 
  alphaNum <|> char '_' 

{-
  Note here we also allow colon, provided it is not followed by another colon.
  The reason for this is that we want to allow species names in biopepa such
  as, E:S, but we also want to stop at the first colon if the name is a
  separated name in srmc such as
  UEDIN::Ftp

  This might be a bit of a perfomance hit, in which case I can just
  parse all names as a single name and then later divide up by
  double colons (this is what I probably should do).
-}
alphaNumUnderScoreColon :: Parser Char
alphaNumUnderScoreColon =
  alphaNumUnderScore <|> singleColon
           

singleColon :: Parser Char
singleColon = try $ do char ':'
                       notFollowedBy $ char ':'
                       return ':'

{-| Srmc style namespace names, these must have the component part
    start with a capital letter so this could also be a component
    name.
-}
nameSpace :: Parser QualifiedName
nameSpace =
  do ident  <- ( upperId <|> stringLiteral )
     idTail <- maybeOption $ coloncolon >> pepaComponentId
     return $ addTail ident idTail
  where
  addTail :: String -> Maybe ParsedComponentId -> ParsedComponentId
  addTail ident Nothing  = Unqualified ident
  addTail ident (Just p) = NameSpaceId ident p

{-|
  Parse a whole name space name as a string.
-}
wholeNameSpaceId :: Parser String
wholeNameSpaceId = 
  followedBy ( many1 (alphaNumUnderScore <|> (char ':')) )
             whiteSpace

upperOrLowerId :: Parser String
upperOrLowerId = upperId <|> lowerId

lowerId :: Parser String
lowerId = do firstChar <- lower
             restChars <- many alphaNumUnderScore
             whiteSpace
             return (firstChar : restChars)

upperId :: Parser String
upperId = do firstChar <- upper
             restChars <- many alphaNumUnderScoreColon
             whiteSpace
             return (firstChar : restChars)

floatNumber :: Parser Double
floatNumber = StdToken.float lexer

{-
  'forgivingFloat' is the same as 'floatNumber' except that as
  a short hand for a whole number one can leave off the decimal
-}
forgivingFloat :: Parser Double
forgivingFloat = 
  liftM doubleOfEitherNum naturalOrFloat
  where 
  doubleOfEitherNum :: Either Integer Double -> Double
  doubleOfEitherNum (Left i)  = fromInteger i
  doubleOfEitherNum (Right d) = d

{-
  A comma separated list of floating point numbers
-}
floatNumberList :: Parser [Double]
floatNumberList = sepBy1 floatNumber comma


{-
  A comma separated list of floating point numbers, may not
  seem necessary but for example there are cases where there
  are several options all of which begin with a floating point
  number, if one of the options is a list then we have to parse
  the rest of the list with this.
  For example, the [- --rates -] option, accepts either
  [- r=1.2,1.3,1.4,1.5 -]
  or
  [- r=1.2..1.5:0.1 -]
  In both cases it starts with an initial floating point number,
  then we have to decide whether we parse a list if a comma
  follows or the range and step if two dots follow.
-}
commaFloatNumberList :: Double -> Parser [Double]
commaFloatNumberList first = 
  do comma
     numbers <- floatNumberList
     return (first : numbers)

{-|
  This parser combinator should be in the parsec library
  but I can't find exactly this.
-}
maybeOption :: GenParser tok st a -> GenParser tok st (Maybe a)
maybeOption p = 
  (liftM Just (try p))
  <|> 
  (return Nothing)
-- option Nothing $ liftM Just (try p)


{-|
  Used for the (relatively) common case where we wish to parse
  an object and then terminate it with some object (usually a single
  token for example a semi-colon).
  This is often written as something like:
  @ do c <- commandParser
       symbol ";"
       return c
  @
-}
followedBy :: Parser a -> Parser b -> Parser a
followedBy p q =
  do result <- p
     q
     return result

{-
  Token Parser Stuff !!!
  The token parser essentially allows us to forget about the
  whitespace.

  The following definition of 'lexer' is required for the token 
  parser stuff to work.
  What I should do is comment this better.

  Note: Although command line options are less likely to contain spaces
  since a space generally separates out a command line option it
  is still possible that they contain spaces by quoting an option
  for example we could have
  @ ipc --rates "r = 1.2 .. 2.1 : 0.1" ... @
  as a command line, hence there are spaces within the arguments
  to the @--rates @ option.
-}
lexer :: StdToken.TokenParser ()
lexer = 
    PrvToken.makeTokenParser langDef
    where 
    langDef1  = PrvToken.translateLanguageDef haskellStyle
    langDef   = langDef1 { PrvToken.reservedOpNames = reservedOperators
                         , PrvToken.reservedNames   = pepaNames
                         , PrvToken.commentLine     = [ "%", "//" ]
                         , PrvToken.commentStart    = "{-"
                         , PrvToken.commentEnd      = "-}"
                         }
    pepaNames = [ "else"
                , "if"
                , "infty"
                , "then"
                , "immediate"
                , "_"
                --, "stop" 
                ]
    reservedOperators = [ "*"
                        , "/"
                        , "+"
                        , "-"
                         -- Probe operators
                        , "?"
                        ]


{-
  For convenience we alias some of the common token parsers.
  Along with a couple commented out because we do not use them
  currently but may at some point.
-}
whiteSpace :: CharParser () ()
whiteSpace = StdToken.whiteSpace lexer


--lexeme     :: CharParser () a -> CharParser () a
--lexeme     = StdToken.lexeme lexer

symbol     :: String -> CharParser () String
symbol     = StdToken.symbol lexer

natural :: CharParser () Integer
natural    = StdToken.natural lexer

naturalOrFloat :: CharParser () (Either Integer Double)
naturalOrFloat = StdToken.naturalOrFloat lexer

integer :: CharParser () Integer
integer    = StdToken.integer lexer

intParser :: CharParser () Int
intParser  = liftM fromInteger integer

stringLiteral :: CharParser () String
stringLiteral = StdToken.stringLiteral lexer

parens     :: CharParser () a -> CharParser () a
parens     = StdToken.parens lexer

angles     :: CharParser () a -> CharParser () a
angles     = StdToken.angles lexer

squares    :: CharParser () a -> CharParser () a
squares    = StdToken.squares lexer

braces     :: CharParser () a -> CharParser () a
braces     = StdToken.braces lexer

--semi       :: CharParser () String
--semi       = StdToken.semi lexer

comma       :: CharParser () String
comma        = StdToken.comma lexer

colon       :: CharParser () String
colon       = symbol ":" -- StdToken.colon lexer

coloncolon :: CharParser () String
coloncolon = colon >> colon

--identifier :: CharParser () String
--identifier = StdToken.identifier lexer

reserved   :: String -> CharParser () ()
reserved   = StdToken.reserved lexer

reservedOp :: String -> CharParser () ()
reservedOp = StdToken.reservedOp lexer

