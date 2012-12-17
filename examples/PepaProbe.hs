{-|
    This is a simple example use of the Pepa library.
    Given a pepa model file we produce an equivalent
    timed system view of the model.
-}
module Main
    ( main )
where

{- Standard library modules imported -}
import System.Console.GetOpt
   ( OptDescr  ( .. )
   , ArgDescr  ( .. )
   )
import Text.ParserCombinators.Parsec 
    ( ParseError )
{- External Library modules imported -}
{- Local modules imported -}
import Ipc.Cli
   ( getCliArgs
   , Cli            ( .. )
   , CliOpt         ( .. )
   , toCli
   , baseCliOptions
   , getProbeSpecs
   , getMasterBool
   , getNonStandard
   )
import Language.Pepa.Syntax
    ( ParsedModel )
import Language.Pepa.Parser
    ( parsePepaFile )
import Language.Pepa.Print
    ( hprintPepaModel )

import Language.Pepa.Probes.Syntax
   (  ProbeDef )
import Language.Pepa.Probes.AddProbes
  ( ProbeTranslateFlags    ( .. ) )
import Language.Pepa.Probes.Translate
   (  probeToDotFile )
import Language.Pepa.Probes.Parser
    ( parseProbe )
import Language.Pepa.MainControl
  ( MainControl
  , closeMainControlWith
  , runMainControlT
  )
{- End of module imports -}


main :: IO ()
main = getCliArgs >>= processArgs . toCliPepaProbe

toCliPepaProbe :: [ String ] -> Cli PepaProbeOpts
toCliPepaProbe = toCli version "pepaprobe" pepaProbeOptions
   
-- Processing of the command-line arguments.
processArgs :: Cli PepaProbeOpts -> IO () 
processArgs (CliValid options inputFiles) =
  case parseProbeStrings probeStrings of
    Right probes
      | elem OutputDot probeOpts -> 
        mapM_ (convertToDot trFlags) probes
      | otherwise                -> 
        mapM_ (convertFile options addMaster probes) inputFiles
    Left err                     -> print err
  where
  probeStrings = getProbeSpecs  options
  addMaster    = getMasterBool  options
  probeOpts    = getNonStandard options
  trFlags      = getTrFlags     options
processArgs (CliInfo  _ _ infoString)     = putStrLn infoString
processArgs (CliError _ _ errorString)    = putStrLn errorString


version :: String
version = "This is pepaprobe version 0.01"

data PepaProbeOpts = OutputDot
                   | TranslateFlag ProbeTranslateFlags
                   deriving Eq

pepaProbeOptions :: [ OptDescr ( CliOpt PepaProbeOpts ) ]
pepaProbeOptions = 
   baseCliOptions ++
   [ Option ""     [ "output-dot" ] 
     (NoArg (CliNonStandard OutputDot) )
     "Instead of adding to the model output the probe as a .dot file"

   , Option ""     [ "no-minimise" ] 
     (NoArg (CliNonStandard $ TranslateFlag NoMinimise) )
     "Do not minimise the deterministic finite automata"

   , Option ""     [ "min-self-loops" ]
     (NoArg (CliNonStandard $ TranslateFlag MinSelfLoops) )
     "add the self-loops before performing minimisation (not really tested)"

   , Option ""     [ "no-min-self-loops" ]
     (NoArg (CliNonStandard $ TranslateFlag NoMinSelfLoops) )
     ( "perform minimisation before adding the self-loops " ++
       "(such that they are not minimised)" )

   , Option ""     [ "no-self-loops" ]
     (NoArg (CliNonStandard $ TranslateFlag NoSelfLoops) )
     "do not add the self-loops (useful mostly for debugging)"

   , Option ""     [ "non-deterministic" ]
     (NoArg (CliNonStandard $ TranslateFlag NonDeterm) )
     ( "leave the finite automata non-deterministic" ++
       "(again useful mostly for debugging)" )
   ]

getTrFlags :: [ CliOpt PepaProbeOpts ] -> [ ProbeTranslateFlags ]
getTrFlags options = [ f | TranslateFlag f <- getNonStandard options ]

                   

parseProbeStrings :: [ String ] -> Either ParseError [ ProbeDef ]
parseProbeStrings = foldr (joinParseResults (:) . parseProbe) (Right [])

{-
   The 'addMaster' argument here specifies whether or not to add
   a master probe (essentially whether or not --no-master was in
   in the list of options).
-}
convertFile :: [ CliOpt PepaProbeOpts ] -> Bool -> [ ProbeDef ] 
            -> FilePath -> IO ()
convertFile options addMaster probeDefs file = 
  do parseResult <- runMainControlT $ parsePepaFile file
     let probeResult = parseResult >>= addProbes
         output      = formatResult probeResult
     putStrLn output
  where
  addProbes :: ParsedModel -> MainControl ParsedModel
  addProbes = addProbesToModel options addMaster probeDefs

addProbesToModel :: [ CliOpt PepaProbeOpts ] -> Bool -> [ ProbeDef ] 
                 -> ParsedModel -> MainControl ParsedModel
addProbesToModel _options _addMaster _probeDefs _model = undefined
{- TODO: We need to be able to call Ipc.addProbesToModel, we cannot
   because the kind of flags are different. So I suggest we remove
   from the flags the CliNonStandard. Well essentially I think we
   do need an overall of the command-line arguments. Not sure how
   it should be done though.
   
   I leave the old code here so that I could get something working
   quickly if need be.
-addProbesToModel options False probeDefs model = $
-   addProbeDefs (getTrFlags options) probeDefs model
-addProbesToModel options True  probeDefs model =
-   bindEithers ( Right . addMaster )$ $
-      addProbeDefs (getTrFlags options) probeDefs model
-   where
-   addMaster    = addMasterProbe "yyProbeStopped" "yyProbeRunning" $
-                                 startActs        stopActs
-   startActs    = getStartActions options
-   stopActs     = getStopActions  options
-}

   

{-
   Convert a file into dot format
-}
convertToDot :: [ ProbeTranslateFlags ] -> ProbeDef -> IO ()
convertToDot trFlags =
   putStrLn .
   probeToDotFile trFlags "ProbeName" .
   snd


formatResult :: MainControl ParsedModel -> String
formatResult = closeMainControlWith hprintPepaModel id



joinParseResults :: (a -> b -> c)
                 -> Either ParseError a
                 -> Either ParseError b
                 -> Either ParseError c
joinParseResults _ (Left err) _          = Left err
joinParseResults _ _          (Left err) = Left err
joinParseResults f (Right a)  (Right b)  = Right $ f a b
