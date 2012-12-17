{-| 
    A parser for stochastic probe definitions.
-}
module Language.Pepa.Probes.Parser
    ( parseProbe
    , probeDefParser
    )
where

{- Standard Libraries modules imported -}
import qualified Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec
    ( Parser
    , CharParser
    , ParseError
    
    , parse

    , ( <|> )
    , ( <?> )
    )
import qualified Text.ParserCombinators.Parsec.Token as StdToken
import Text.ParserCombinators.Parsec.Expr
    ( buildExpressionParser
    , Assoc (..)
    , Operator (..)
    )
import Text.ParserCombinators.Parsec.Language
    ( haskellStyle )
{- External Library modules imported -}
{- Local modules imported imported -}
import Language.Pepa.Probes.Syntax
    ( ProbeDef
    , ProbeR     ( .. )
    )
import Language.Pepa.Parser
    ( maybeOption
    , pepaActionId
    , pepaComponentId
    )
import Language.Hydra.Parser
  ( exprParser )

import qualified Language.Pepa.PrivateTokenParser as PrvToken
{- End of imports -}

{-| A function to parse a probe specfication -}
parseProbe :: String -> Either ParseError ProbeDef
parseProbe = parse probeDefParser "no file"

{-|
  This parses a probe, which may be preceeded by a location.
  Note that because a probe definition may occur as part of
  a file (and not the whole thing), for example within a performance
  tree file, we cannot use PepaParser.wholeParser. This means that
  if you are using this for say a command-line option or a gui field
  then you should wrap it within wholeParser.
-}
probeDefParser :: Parser ProbeDef
probeDefParser = 
  do mCompId <- mLocation
     probe   <- probeParser
     return (mCompId, probe)
  where
  mLocation = Parsec.optionMaybe location
  location = do pcomp <- pepaComponentId
                symbol "::"
                return pcomp

probeParser :: Parser ProbeR
probeParser =
    buildExpressionParser probeTable probeFactor
    <?> "probe specification"
    where 
    probeTable   = [ [ postfixOp "+" SPoom 
                     , postfixOp "*" SPzom
                     , postfixOp "?" SPzoo
                     , labelAction
                     , iterAction
                     ]
                   , [ stateGuard ]

                   , [ hideAction ]

                   , [ infixOp "," SPseq AssocLeft
                     ]

                   , [ infixOp "|" SPchoice AssocRight
                     ]
                   ]
    infixOp opString f = Infix (reservedOp opString >> return f)

    postfixOp opString f = Postfix (reservedOp opString >> return f)

    hideAction = Postfix $ do reservedOp "/"
                              ident <- pepaActionId
                              let f probe = SPwo probe ident
                              return f

    labelAction = Postfix $ do reservedOp ":"
                               ident <- pepaActionId
                               let f probe = SPseq probe $ SPlabel ident
                               return f


    -- Kind of temporary for now.
    stateGuard  = Prefix $ do cexp   <- braces exprParser
                              return $ SPcond cexp

    iterAction  = Postfix $ do symbol "{"
                               num1 <- natural
                               num2 <- maybeOption (do symbol ","
                                                       natural )
                               symbol "}"
                               return $ addTail num1 num2
                  where addTail :: Integer -> Maybe Integer -> ProbeR -> ProbeR
                        addTail n Nothing probe  = SPiter probe (fromInteger n)
                        addTail n (Just m) probe = SPbit probe ( fromInteger n
                                                               , fromInteger m )

    probeFactor  = parens probeParser
                   <|> probeAction
                   <|> probeLabel
                   <?> "probe atomic"

    probeAction :: Parser ProbeR
    probeAction = do ident <- pepaActionId
                     return $ SPact ident

    probeLabel :: Parser ProbeR
    probeLabel = do reservedOp ":"
                    ident <- pepaActionId
                    return $ SPlabel ident

    {- 
    probeAction :: Parser ProbeR
    probeAction = do ident <- pepaActionId
                     tag   <- maybeOption actionTag
                     return $ addActionTag ident tag
                  where
    actionTag :: Parser ActionIdentifier
    actionTag = do colon
                   pepaActionId

    addActionTag :: ActionIdentifier -> Maybe ActionIdentifier -> ProbeR
    addActionTag ident (Just (Unqualified "start")) = SPact $ SPAstart ident
    addActionTag ident (Just (Unqualified "stop"))  = SPact $ SPAstop  ident
    addActionTag ident mLabel         = SPact $ SPAlabel ident mLabel
    -}


{-
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
                --, "stop" 
                ]
    reservedOperators = [ "*"
                        , "?"
                        , ":"
                        , "::"
                        , "@"
                        ]


{-
  For convenience we alias some of the common token parsers.
  Along with a couple commented out because we do not use them
  currently but may at some point.
-}
-- whiteSpace :: CharParser () ()
-- whiteSpace = StdToken.whiteSpace lexer

--lexeme     :: CharParser () a -> CharParser () a
--lexeme     = StdToken.lexeme lexer

symbol     :: String -> CharParser () String
symbol     = StdToken.symbol lexer

natural :: CharParser () Integer
natural    = StdToken.natural lexer

-- naturalOrFloat :: CharParser () (Either Integer Double)
-- naturalOrFloat = StdToken.naturalOrFloat lexer

-- integer :: CharParser () Integer
-- integer    = StdToken.integer lexer

-- intParser :: CharParser () Int
-- intParser  = do i <- integer
--                return $ fromInteger i

parens     :: CharParser () a -> CharParser () a
parens     = StdToken.parens lexer

-- angles     :: CharParser () a -> CharParser () a
-- angles     = StdToken.angles lexer
-- 
-- squares    :: CharParser () a -> CharParser () a
-- squares    = StdToken.squares lexer
-- 
braces     :: CharParser () a -> CharParser () a
braces     = StdToken.braces lexer

-- semi       :: CharParser () String
-- semi       = StdToken.semi lexer

-- comma       :: CharParser () String
-- comma        = StdToken.comma lexer

-- colon       :: CharParser () String
-- colon       = StdToken.colon lexer

-- identifier :: CharParser () String
-- identifier = StdToken.identifier lexer

-- reserved   :: String -> CharParser () ()
-- reserved   = StdToken.reserved lexer

reservedOp :: String -> CharParser () ()
reservedOp = StdToken.reservedOp lexer
