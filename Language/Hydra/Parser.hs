{- 
Issues: > Declarations in \constant{Decl}{} must start with upper case, use dnamIdUp
        > Declarations in \initial{decl = ..; } must start with lower case, uses dnamIdLo
        > In (DNAMtransition a b) i have set b as DNAMspeed and I couldnt figure out what transActionKind :: a
          was used for so i just intialised its type to Int and set every transition to have value 0. 
        > Notes on exprParser
-}

module Language.Hydra.Parser 
  ( parseDNAMfile
  , exprParser
  )
where

{- External Library Modules Imports -}
{- Standard Library Module Imports -}

import Text.ParserCombinators.Parsec
    ( ParseError 
    , Parser 
    , CharParser
    , GenParser

    , parseFromFile
    , char
    -- , eof
    , (<?>)
    , between
    , many
    , many1
    , (<|>)
    , try
    , lower
    , upper
    , alphaNum
    , anyChar
    , manyTill
    )

import Text.ParserCombinators.Parsec.Perm
    ( permute
    , (<$$>)
    , (<$?>)
    , (<||>)
    , (<|?>)
    )   

import qualified Text.ParserCombinators.Parsec.Token as StdToken

import Text.ParserCombinators.Parsec.Language
    ( haskellStyle )

import Control.Monad
    ( liftM )

{- Local Module Imports -}
import qualified Language.Pepa.PrivateTokenParser as PrvToken
import Language.Pepa.QualifiedName
    ( QualifiedName          ( .. ) )

import Language.Pepa.Parser
  ( exprParser )
import Language.Hydra.Syntax
    ( DNAMmodelFile         ( .. )
    , DNAMmodel             ( .. )

    , DNAMheader            ( .. )
 
    , DNAMconstantDef       ( .. )
 
      --  Initial states
    , DNAMstateVector
    , DNAMstateDesc         ( .. )
 
    , DNAMinitialVector
    -- , DNAMinitialAssign
 
    --  Model transitions
    -- , DNAMmodelTransitions
    , DNAMtransition        ( .. )
    , DNAMcondition         ( .. )
    , DNAMaction
    , DNAMspeed             ( .. )
    , DNAMpriority
    , defaultPriority
 
    , DNAMmodelInvariant    ( .. )
 
      --  Solution Control
    , DNAMsolutionControl   ( .. )
    , DNAMsolutionMethod    ( .. )
    , DNAMperformMeasure    ( .. )

      --  DNAM c constructs.
    , DNAMcassignment       ( .. )
    , DNAMctype             ( .. )
        
    )
{- End of Imports -}


{-
    Need to switch parseMod to (String -> IO (Either ParseError DNAMmodelFile)) 
    , when DNAMmodelFile has been uncommented in Language/Hydra/Syntax.hs,
    and then join the result of solutionParser with the DNAMmodel from modParser
    using Parsec.Perm
    What it would look like if DNAMmodelFile was uncommented would be as follows (untested)
-}
{- Takes a filename a returns a parsed model -}
parseDNAMfile :: String -> IO (Either ParseError (DNAMmodelFile Int DNAMspeed))
parseDNAMfile = 
  parseFromFile $  permute ( ( DNAMmodelFile 
                             <$$> modParser
                             {- Gauss as default -}
                             <|?> (DNAMsolution DNAMgauss, solutionParser)
                             )
                             <|?> ([], performanceMeasureList)
                           )

solutionParser :: Parser DNAMsolutionControl
solutionParser =  do reserved "\\solution"
                     m <- braces method
                     return DNAMsolution {solutionMethod = m}
    where
        method :: Parser DNAMsolutionMethod        
        method = do { reserved "\\method";
                      m <- braces identifier;
                      case m of
                        "gauss"        -> return DNAMgauss
                        "grassman"     -> return DNAMgrassman 
                        "gauss_seidel" -> return DNAMgaussSeidel
                        "sor"          -> return DNAMsor 
                        "bicg"         -> return DNAMbicg 
                        "cgnr"         -> return DNAMcgnr 
                        "bicgstab"     -> return DNAMbicgstab 
                        "bicgstab2"    -> return DNAMbicgstab2
                        "cgs"          -> return DNAMcgs 
                        "tfqmr"        -> return DNAMtfqmr 
                        "ai"           -> return DNAMai 
                        "air"          -> return DNAMair 
                        "automatic"    -> return DNAMautomatic 
                        _              -> fail "unknown solution method"
                    }


{- This is a dummy implementation until I can find the time to implement this properly -}
-- Should obviously be something like:@ many performanceMeasure@
performanceMeasureList :: Parser [ DNAMperformMeasure ]
performanceMeasureList = return []


{- A passage-time measurement looks like the following
 A passage-time measurement is specified using the
 following grammar.
\passage{
        \sourcecondition{<boolean expression>}
        \targetcondition{<boolean expression>}
        \t_start{<real number>}
        \t_stop{<real number>}
        \t_step{<real number>}
-}
-- passageTimeMeasure :: Parser DNAMperformMeasure
-- passageTimeMeasure =
--  do 



{- The main parser that parses a DNAMaca model -}
modParser :: Parser (DNAMmodel Int DNAMspeed)
modParser = do whiteSpace
               reserved "\\model"
               braces modPermParser
            <?> "\\model{ ... }"
    where
        modPermParser :: Parser (DNAMmodel Int DNAMspeed)
        modPermParser = permute (DNAMmodel <$?> ([], many1 headerParser)
                                           <||> concat `liftM` many1 stateVectorParser  
                                           <|?> ([], many1 constantParser)
                                           <||> initialParser
                                           <||> many1 transitionParser
                                           <|?> ([], many1 invariantParser)
                                )

{- Parses a constant definition -}
constantParser :: Parser DNAMconstantDef
constantParser = do { reserved "\\constant";
                      varName <- braces dnamIdUp;  
                      varValue <- braces exprParser;  
                      return $ DNAMconstantDef varName varValue
                    }
                <?> "\\constant{VAR_NAME}{VALUE}"

{- Parses a header definition -}
{- TODO: What if a header contains a {}. Maybe this is an extreme case -}
headerParser :: Parser DNAMheader 
headerParser = do { reserved "\\header";
                    symbol "{";
                    s <- manyTill anyChar (symbol "}");
                    return $ DNAMheader s
                  }
                <?> "\\header{EXPRESSION}"


{- Parses the list of initial definitions -}
initialParser :: Parser DNAMinitialVector 
initialParser = do reserved "\\initial"
                   braces $ many1 initValParser
                <?> "\\initial{VAR = VAL; ...}"
    where
        initValParser :: Parser DNAMcassignment
        initValParser = do { s <- dnamIdLo;
                             symbol "=";
                             v <- exprParser; 
                             semi;
                             return $ DNAMcassignment s v 
                           }

{- Parses the state vector-}
stateVectorParser :: Parser DNAMstateVector
stateVectorParser = do { reserved "\\statevector";
                         s <- braces $ many1 typeSVParser;
                         return $ concat s
                       }
                    <?> "\\statevector{\\type{TYPE}{VAL1,VAL2} ...}"
    where
        typeSVParser :: Parser [DNAMstateDesc]
        typeSVParser = do reserved "\\type";
                          t <- braces primitiveCType 
                          ls <- braces $ commaSep1 dnamIdLo;
                           return $ map (DNAMstateDesc t) ls


primitiveCType :: Parser DNAMctype
primitiveCType = 
  do name <- identifier
     case name of
       "int"   -> return CInt
       "short" -> return CShort
       "long"  -> return CLong
       "char"  -> return CChar
       _       -> fail "unknown primitive c type"
                

{- Parses an invariant boolean expression -}
invariantParser :: Parser DNAMmodelInvariant
invariantParser =  do { reserved "\\invariant";
                        expr <- braces exprParser; 
                        return $ DNAMinvariant expr 
                      }
                   <?> "\\invariant{BOOLEAN EXPRESSION}"

{- Parses a transition -}
transitionParser :: Parser (DNAMtransition Int DNAMspeed)
transitionParser = do reserved "\\transition"
                      name <- braces dnamIdUpOrLo
                      braces $ actPermParser name
                   <?> "\\transition{NAME}{..}"
    where
        actPermParser :: QualifiedName -> Parser (DNAMtransition Int DNAMspeed)
        actPermParser name = permute ( joinToAct <$$> tCondParser
--                                                many1 probably isn't neccessary here, added for flexibility
                                                 <||> many1 tActionParser  
                                                 <||> tRateParser
                                                 <|?> (defaultPriority, tPriorityParser)
                                     )
            where
                joinToAct cond aclist rate prior
                    = DNAMtrans { transName       = name,
                                  transActionKind = 0,
                                  transConditions = [ cond ],
                                  transActions    = concat aclist,
                                  transSpeed      = rate,
                                  transPriority   = prior
                                }

        tPriorityParser :: Parser DNAMpriority 
        tPriorityParser = do { reserved "\\priority";
                               i <- braces integer;
                               return $ fromInteger i
                             }
                          <?> "\\priority{INTEGER}"

        tCondParser :: Parser DNAMcondition
        tCondParser = do { reserved "\\condition"; 
                           expr <- braces exprParser; 
                           return $ DNAMcond expr
                         }
                      <?> "\\condition{BOOLEAN EXPRESSION}"

        tActionParser :: Parser [DNAMaction]
        tActionParser = do reserved "\\action"
                           braces $ many1 tActExprParser
                        <?> "\\action{next->VAR = EXPRESSION; ...}"
            where
{- 
    It seems like a bit excessive to go and check whether it's id = id-1 just to fulfill
    what the data type offers, but here's an implementation anyway. Problem with this is that 
    it expects id1 = id2 +/- n, which might not be general enough.
-}
{-
tActExprParser :: Parser DNAMcassignment 
tActExprParser = do { symbol "next->";
                      id1 <- identifier;   
                      symbol "=";
                      id2 <- identifier;  
                      op <- anyChar;
                      n <- integer; 
                      semi;
                      return $ if (id1++[op]++(show n)) == (id2++"-1") then
                                   DNAMidentDecr $ Unqualified id1
                              else if (id1++[op]++(show n)) == (id2++"+1") then
                                   DNAMidentIncr $ Unqualified id1
                              else DNAMcassignment (Unqualified id1) $ CFreeForm (id2++[op]++(show n)) 
                    }
-}
                {- The safe and general case -}
                tActExprParser :: Parser DNAMcassignment 
                tActExprParser = do { symbol "next->";
                                      id1 <- dnamIdLo;
                                      symbol "=";
                                      expr <- exprParser;
                                      semi;
                                      return $ DNAMcassignment id1 expr
                                    }

        tRateParser :: Parser DNAMspeed
        tRateParser = do { try $ reserved "\\rate";  
                           expr <- braces exprParser; 
                           return $ DNAMrate expr
                         } 
                      <|> do { reserved "\\weight";
                           expr <- braces exprParser; 
                           return $ DNAMweight expr
                         } <?> "\\weight{VAL} or \\rate{VAL}"


lexer :: StdToken.TokenParser ()
lexer = 
    PrvToken.makeTokenParser langDef
    where 
    langDef1  = PrvToken.translateLanguageDef haskellStyle
    langDef   = langDef1 { 
               PrvToken.reservedOpNames = reservedOperators
             , PrvToken.reservedNames   = resNames 
             , PrvToken.commentLine     = [ "%", "//" ]
             , PrvToken.commentStart    = "{-"
             , PrvToken.commentEnd      = "-}"
             }

    resNames = [ 
          "\\model"
        , "\\constant"
        , "\\header"
        , "\\initial"
        , "\\stateVector"
        , "\\type"
        , "\\invariant"
        , "\\transition"
        , "\\condition"
        , "\\action"
        , "\\rate"
        , "\\solution"
        , "\\method"
        , "\\priority"
        , "int"
        , "long"
        , "char"
        , "short"
        ]

    reservedOperators = [ 
              "&&"
            , ">"
            , "<"
            , ">="
            , "<="
            , "||"
            , "=="
            ]

-- For upper case starting varnames in \constant{}{}
dnamIdUp :: Parser QualifiedName 
dnamIdUp = do ident <- upperId
              return $ Unqualified ident
-- pepaComponentId

-- For lower case starting varnames in \initial{}{}
dnamIdLo :: Parser QualifiedName 
dnamIdLo = do ident <- lowerId
              return $ Unqualified ident

dnamIdUpOrLo :: Parser QualifiedName
dnamIdUpOrLo = try dnamIdUp <|> dnamIdLo

lowerId :: Parser String
lowerId = do firstChar <- lower
             restChars <- many alphaNumUnderScore
             whiteSpace
             return (firstChar : restChars)


upperId :: Parser String
upperId = do firstChar <- upper
             restChars <- many alphaNumUnderScore
             whiteSpace
             return (firstChar : restChars)


alphaNumUnderScore :: Parser Char
alphaNumUnderScore = alphaNum <|> char '_'

{- Various parsers combined with the lexer -}

{-
float :: Parser Double
float = StdToken.float lexer
-}

integer :: Parser Integer
integer = StdToken.integer lexer

symbol :: String -> CharParser () String
symbol = StdToken.symbol lexer

whiteSpace :: CharParser () ()
whiteSpace = StdToken.whiteSpace lexer

{-
lexeme :: CharParser () a -> CharParser () a
lexeme = StdToken.lexeme lexer
-}

braces :: GenParser Char () a -> GenParser Char () a
braces = between (symbol "{") (symbol "}")

identifier :: CharParser () String 
identifier = StdToken.identifier lexer

{-
operator :: CharParser () String 
operator = StdToken.operator lexer
-}

reserved :: String -> CharParser () ()
reserved = StdToken.reserved lexer

{-
reservedOp :: String -> CharParser () ()
reservedOp = StdToken.reservedOp lexer
-}

semi :: CharParser () String
semi = StdToken.semi lexer

commaSep1 :: CharParser () a -> CharParser () [a]
commaSep1 = StdToken.commaSep1 lexer

{-
semiSep1 :: CharParser () a -> CharParser () [a]
semiSep1 = StdToken.semiSep1 lexer

parens :: CharParser () a -> CharParser () a 
parens = StdToken.parens lexer
-}