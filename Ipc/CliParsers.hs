{-|
   This module contains some of the parsers required to
   process the command-line options to @ipc@ and many of
   the utility programs.
-}
module Ipc.CliParsers
   ( parseProcessRename
   , parseRateRename
   , joinParseErrorList
   , parseCommaActionsMaybeAssigns
   )
where

{- Standard imported libraries -}
import Text.ParserCombinators.Parsec
   ( Parser
   , ParseError
   , parse

   , many
   )
import Text.ParserCombinators.Parsec.Char
   ( anyChar )
import Text.ParserCombinators.Parsec.Combinator
  ( sepBy1 )
{- External Library modules imported -}
{- Local imported libraries -}
import Language.Pepa.QualifiedName
  ( QualifiedName     ( .. ) )
import Language.Pepa.Syntax
   ( ActionIdentifier )
import Language.Pepa.Parser
   ( blindRun
   , pepaComponentId
   , pepaRateId
   , pepaActionId

   , maybeOption

   , intParser
   , symbol
   , comma
   )
import Language.Pepa.Transform.Replace
   ( ProcessRename 
   , RateRename
   )
{- End of module imports -}


{-| Parses a process-renaming-option argument -}
parseProcessRename :: String -> String -> Either ParseError ProcessRename
parseProcessRename =
   parse processRenameParser

{- The process renamer parser -}
processRenameParser :: Parser ProcessRename
processRenameParser =
   do ident       <- pepaComponentId
      symbol "="
      replacement <- many anyChar
      return (ident, Unqualified replacement)


{-| Parses a rate-renaming-option argument -}
parseRateRename :: String -> String -> Either ParseError ProcessRename
parseRateRename =
   parse rateRenameParser

{- The rate renamer parser -}
rateRenameParser :: Parser RateRename
rateRenameParser =
   do ident   <- pepaRateId
      symbol "="
      replacement <- many anyChar
      return (ident, Unqualified replacement)

{-| Joins together a list of parse results -}
joinParseErrorList :: [ Either ParseError a ] -> Either ParseError [ a ]
joinParseErrorList []                 = Right []
joinParseErrorList ((Left err) : _ )  = Left err
joinParseErrorList ((Right a) : rest) =
   case joinParseErrorList rest of
      Left err -> Left err
      Right l  -> Right (a : l)





{-| Parses a list of comma separated names. Each name may or may not have a
    assignment attached to it. 
-}
parseCommaActionsMaybeAssigns :: String -> String -> [ (ActionIdentifier, Maybe Int) ]
parseCommaActionsMaybeAssigns err =
  blindRun err commaActionsMaybeAssigns


{- 
   A parser for a list of comma separated assignments where the assignment may
   or may not be present.
   For now the assignment may just be to an integer
    but this should be fairly easy to generalise.
-}
commaActionsMaybeAssigns :: Parser [ (ActionIdentifier, Maybe Int) ]
commaActionsMaybeAssigns =
  sepBy1 maybeAssign comma
  where
  maybeAssign :: Parser (ActionIdentifier, Maybe Int)
  maybeAssign =
    do name   <- pepaActionId
       assign <- maybeOption assignTail
       return (name, assign)
  assignTail :: Parser Int
  assignTail =
    do symbol "="
       intParser
       