{-| Some utility functions for parsing the whole of a PEPA file.
    These functions are mainly here so that there is no circular
    dependency between Language.Pepa.Parser and Language.Pepa.Print
    since we wish to print to the log the entire parsed pepa model.
-}
module Language.Pepa.FileParser
  ( parsePepaFile
  , controlParseResult
  , mainControlParse
  , parseFile
  , fileParser
  , wholeParser
  , pepaFile
  )
where

{- Standard Library Modules Imported -}
import Control.Monad.Trans as Trans
import System.Directory
  ( doesFileExist )
{- External Library Modules Imported -}
import qualified  Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec
  ( Parser
  , ParseError
  )
{- Local Modules Imported -}
import qualified Language.Pepa.Parser as PepaParser
import qualified Language.Pepa.Print as PepaPrint
import qualified Language.Pepa.MainControl as MainControl
import Language.Pepa.MainControl
  ( MainControl
  , IOMainControl
  )
import Language.Pepa.Syntax
  ( ParsedModel )
{- End of Module Imports -}


{-|
  Parses a PEPA file, we must do this in the IO monad since
  we must read the contents of the file, but within that
  monad we wish to be within the 'MainControl' monad.
-}
parsePepaFile :: FilePath -> IOMainControl ParsedModel
parsePepaFile file =
  do parsedModel <- parseFile pepaFile file
     let logKey  = "parsed-model"
         logInfo = PepaPrint.hprintPepaModel parsedModel
     MainControl.liftMC $ MainControl.valueResult parsedModel logKey logInfo

{-|
   Parses a file by first checking if the file exists and then
   reading in it's contents and finally parsing the contents
   with the name of the file together with the contents.

   We also return a string as the error, since this allows us
   to return a (string) error in the case that the file does
   not exit.
-}
parseFile :: Parser a -> FilePath -> IOMainControl a
parseFile parser file =
   do fileExists <- Trans.liftIO $ doesFileExist file
      if fileExists
         then do contents <- Trans.liftIO $ readFile file
                 let pResult = Parsec.parse (fileParser parser) file contents
                 MainControl.liftMC $ controlParseResult pResult
         else fail ("file: " ++ file ++ " does not exist")
      

{-|
   Changes a parse function such that it will return an
   'Language.Pepa.MainControl.MainControl' instead of an
   @Either ParseError a@
-}
controlParseResult :: Either ParseError a -> MainControl a
controlParseResult = MainControl.eitherToMainControl

{-|
  Changes a parser returning an 'Either' into one which returns
  a 'MainControl' value.
  Note that this assumes that you have no file to parse from hence
  error messages will not specify the file. If this is what you
  want then either use 'parseFile' or if you want more control then
  parse it yourselve and use 'controlParseResult' to transform the
  result into the 'MainControl' monad.
-}
mainControlParse :: Parser a -> String -> MainControl a
mainControlParse parser = controlParseResult . (Parsec.parse parser "")


{-|
  A function to turn a parser into a parser that will accept a file
  as input. This function must be careful to skip over white space at
  the beginning, the given parser should take care of the whitespace
  for later, it will normally do this using the 'Parsec.Token' library
  to build its parsers. See the documentation for the parsec library
  for further details.
-}
fileParser :: Parser a -> Parser a
fileParser = PepaParser.wholeParser

{-| Re-exported from 'Language.Pepa.Parser.wholeParser' -}
wholeParser :: Parser a -> Parser a
wholeParser = PepaParser.wholeParser

{-|
  The main parser for a pepa model file.
-}
pepaFile :: Parser ParsedModel
pepaFile = fileParser PepaParser.pepaModel
