{-|
    This is a module which build upon the parsers developed in
    'Language.Pepa.Parser' to provide a parser for the Srmc language.
-}
module Language.Pepa.Srmc.Parser
   ( parseSrmcFile )
where

{- Standard imported libraries -}
import Text.ParserCombinators.Parsec
  ( Parser
  , ( <|> )
  , try
  , many1
  , sepBy1
  )

{- Local imported libraries -}
import qualified Language.Pepa.FileParser as FileParser
import qualified Language.Pepa.Parser as PepaParser
import Language.Pepa.Parser
   ( pepaBeginProcessDef
   , pepaComponent
   , upperId
   , symbol
   , braces
   , squares
   , pepaRateDefId
   , pepaRateExp
   , comma
   )

import Language.Pepa.QualifiedName
   ( QualifiedName     ( .. ) )

import Language.Pepa.Srmc.Syntax
   ( ServiceIdent
   , SrmcDef            ( .. )
   , SrmcModel          ( .. )
   )
import Language.Pepa.MainControl
  ( IOMainControl )
{- End of imports -}


{-|
   Runs the main srmc model file parser over the given input.
   The first argument is the name of the file containing the srmc
   model such that error messages maybe say which srmc
   file contains the error.
   The second argument is the actual input of that file.
-}
parseSrmcFile :: FilePath                    -- ^ The file to be parsed
              -> IOMainControl SrmcModel
parseSrmcFile = FileParser.parseFile srmcModel

{-
   A parser for a whole srmc model
-}
srmcModel :: Parser SrmcModel
srmcModel =
   do definitions  <- srmcDefinitionList
      composition  <- pepaComponent
      return $ SrmcModel definitions composition

srmcDefinitionList :: Parser [ SrmcDef ]
srmcDefinitionList = many1 srmcDefinition

{-
   Srmc definitions are just like pepa model definitions except
   that they may contain service definitions.
   Okay I've somewhat decided that we should just have rate definitions
   if there is only one rate then that is fine.
-}
srmcDefinition :: Parser SrmcDef
srmcDefinition = do def <- rateSetDefinition 
                           <|> 
                           ( (try serviceDef) <|> 
                             (try namespaceSet) <|>
                              (try virtualProcessDef) <|>
                             processSetDefinition  )
                    symbol ";"
                    return def

serviceDef :: Parser SrmcDef
serviceDef = do ident       <- srmcServiceId
                symbol "::"
                definitions <- braces srmcDefinitionList
                return $ ServiceDef ident definitions

namespaceSet :: Parser SrmcDef
namespaceSet =
  do ident <- srmcServiceId
     symbol "::"
     symbol "="
     names <- enclosedSet nameSpace
     return $ NameSpaceSet ident names

rateSetDefinition :: Parser SrmcDef
rateSetDefinition =
   do ident <- pepaRateDefId
      symbol "="
      exprs <- enclosedSet pepaRateExp
      return $ RateSet ident exprs

virtualProcessDef :: Parser SrmcDef 
virtualProcessDef =
  do ident    <- srmcServiceId
     symbol "="
     rateExpr <- PepaParser.squares pepaRateExp
     return $ VirtualDef ident rateExpr

processSetDefinition :: Parser SrmcDef
processSetDefinition =
   do ident <- pepaBeginProcessDef
      comps <- enclosedSet pepaComponent
      return $ ProcessSet ident comps


{- Service identifiers are all uppercase -}
srmcServiceId :: Parser ServiceIdent
srmcServiceId = do ident <- upperId
                   return $ Unqualified ident

nameSpace :: Parser QualifiedName
nameSpace = PepaParser.nameSpace

------------------------------------------------------
{- Utility parser combinators -}
------------------------------------------------------

{-
   Takes a given parser and returns a new one which parses
   a list of such things accepted by the first. It may accept
   a single one on it's own, or a comma separated list surrounded
   by curly braces.
-}
enclosedSet :: Parser a -> Parser [ a ]
enclosedSet p =
   setOfP <|> singleP
   where
   setOfP = braces $ sepBy1 p comma
   singleP = do x <- p
                return [ x ]