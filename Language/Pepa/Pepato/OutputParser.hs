{-| 
    A parser for the output of pepato
-}
module Language.Pepa.Pepato.OutputParser
  ( -- * Parsing the output of ode analysis
    parseOdeOutput
    -- * Parsing the output of ctmc analysis
  , PepatoOut     ( .. )
  , parseOutput
  )
where

{- Standard Libraries modules imported -}
import qualified Data.Map as Map
import Data.Map
  ( Map )
import Data.List
  ( isPrefixOf )
import Text.ParserCombinators.Parsec
  ( Parser
  , CharParser
  , ParseError
    
  , parse
  
  , ( <|> )
      
  , many
  , lower
  , char
  , alphaNum
  , string
  )
import qualified Text.ParserCombinators.Parsec.Token as StdToken
import Text.ParserCombinators.Parsec.Language
  ( javaStyle )
{- External Library modules imported -}
{- Local modules imported -}
import qualified Language.Pepa.QualifiedName as Qualified
import Language.Pepa.Parser
  ( forgivingFloat )
{- End of imports -}

type Time = Double
type Name = Qualified.QualifiedName

{-| Parse the output of the ode analysis -}
parseOdeOutput :: String -> [ (Time, Map Name Double) ]
parseOdeOutput outputString
  | null outputString                  = error "Cannot parse data0"
  | null outputLines                   = error "Cannot parse data1"
  | not $ isPrefixOf "%Time," headLine = error "Cannot parse data2"
  | otherwise                          = makeTimeMaps
  where
  outputLines  = dropWhile (not . isPrefixOf "%") $ lines outputString
  headLine     = head outputLines
  nameLine     = drop (length "%Time,") headLine
  dataLines    = tail outputLines

  pNames       = map Qualified.unqualified $ makeNames nameLine

  makeTimeMaps = map makeTime dataLines
  makeTime :: String -> (Time, Map Name Double)
  makeTime s
    | null values = error "Cannot parse data3"
    | otherwise   = (time, mapping)
    where
    values         = makeNumbers s
    time           = head values
    concentrations = tail values
    mapping        = Map.fromList $ zip pNames concentrations

  -- This assumes that there is no white space between the names
  makeNames :: String -> [ String ]
  makeNames = commaSeparated

  makeNumbers :: String -> [ Double ]
  makeNumbers = (map read) . commaSeparated

  commaSeparated :: String -> [ String ]
  commaSeparated ""           = []
  commaSeparated (',' : rest) = commaSeparated rest
  commaSeparated s            = 
    name : (commaSeparated rest)
    where
    (name, rest) = break (== ',') s
  


{-|
  A data type to hold the information from the pepato output
-}
data PepatoOut =
  PepatoOut { pepatoProblems    :: Int
            , pepatoAggregation :: Bool
            , pepatoStates      :: Int
            , pepatoThroughPut  :: [ (String, Double) ]
            , pepatoUtilisation :: (String, [ (String, Double) ] )
            , pepatoPopulation  :: [ (String, Double) ]
            }


{-| A function to parse the output of pepato -}
parseOutput :: String -> Either ParseError PepatoOut
parseOutput = parse pepatoParser "no file"


pepatoParser :: Parser PepatoOut
pepatoParser = 
  do static      <- staticAnalyisParser
     whiteSpace
     agg         <- aggregationParser
     whiteSpace
     _storage    <- storageParser
     whiteSpace
     string "Creating sequential tool"
     whiteSpace
     states      <- statesParser
     whiteSpace
     symbol "--"
     string "Throughput"
     whiteSpace
     symbol "--"
     throughput  <- many throughPutParser
     symbol "--"
     {-
     string "Utilisation"
     string "--"
     utilisation <- utilisationParser
     string "--"
     string "Population"
     string "--"
     population  <- populationParser
     eof-}
     let output = PepatoOut { pepatoProblems    = static
                            , pepatoAggregation = agg
                            , pepatoStates      = states
                            , pepatoThroughPut  = throughput
                            , pepatoUtilisation = undefined -- utilisation
                            , pepatoPopulation  = undefined -- population
                            }
     return output
                            


staticAnalyisParser :: Parser Int
staticAnalyisParser =
  do string "Static analysis. Found"
     i <- intParser
     string "problems"
     return i

--
-- Obviously needs to be updated to allow unenabled.
aggregationParser :: Parser Bool
aggregationParser =
  do string "Aggregation  enabled"
     return True

storageParser :: Parser Int
storageParser =
  do string "Storage requested:"
     intParser

statesParser :: Parser Int
statesParser = 
  do string "Number of states explored:"
     intParser
     
throughPutParser :: Parser (String, Double)
throughPutParser = do ident <- lowerIdent
                      symbol ":"
                      d     <- forgivingFloat
                      return (ident, d)

-- utilisationParser :: Parser (String, [ (String, Double) ] )
-- utilisationParser = undefined

-- populationParser :: Parser [ (String, Double) ]
-- populationParser = undefined




lowerIdent :: Parser String
lowerIdent = do c    <- lower
                rest <- many alphaNumUnderScore
                return $ c : rest


alphaNumUnderScore :: Parser Char
alphaNumUnderScore = alphaNum <|> char '_'



lexer :: StdToken.TokenParser ()
lexer = 
    StdToken.makeTokenParser javaStyle


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

-- natural :: CharParser () Integer
-- natural    = StdToken.natural lexer

-- naturalOrFloat :: CharParser () (Either Integer Double)
-- naturalOrFloat = StdToken.naturalOrFloat lexer

integer :: CharParser () Integer
integer    = StdToken.integer lexer

intParser :: CharParser () Int
intParser  = do i <- integer
                return $ fromInteger i

-- parens     :: CharParser () a -> CharParser () a
-- parens     = StdToken.parens lexer

-- angles     :: CharParser () a -> CharParser () a
-- angles     = StdToken.angles lexer
-- 
-- squares    :: CharParser () a -> CharParser () a
-- squares    = StdToken.squares lexer
-- 
-- braces     :: CharParser () a -> CharParser () a
-- braces     = StdToken.braces lexer

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

-- reservedOp :: String -> CharParser () ()
-- reservedOp = StdToken.reservedOp lexer
