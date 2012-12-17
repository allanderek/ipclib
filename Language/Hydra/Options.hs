{-|
  Defines the data type for the contents of an options file
  and outputting that file in the correct format for Hydra
  to read it. Note that the actual outputting of the file
  is abstracted so one possible method would be to use
  Data.Bytestring another is to use the foreign function interface
  to call the relevant formatting routines from the hydra source.
-}
module Language.Hydra.Options
  ( optionsToOPTIONS
  , testOptions
  , main
  )
where

{- Standard Library Modules Imported -}
import System.Cmd
  ( system )
import System.FilePath
  ( addExtension )
{- External Library Modules Imported -}
{- Local Modules Imported -}
-- temporarily taken out while I use runhaskell
-- import Language.Hydra.Syntax
--   ( DNAMsolutionMethod        ( .. ) )
{- End of Module Imports -}

main :: IO ()
main =
  do writeFile "test.Options.c" $ optionsToCFile testOptions
     system $ unwords [ "gcc", "-o genOpts", "test.Options.c" ]
     system "./genOpts test.OPTIONS"
     system $ unwords [ optInspectorCmd, "test.OPTIONS" ]
     return ()

optInspectorCmd :: String
optInspectorCmd =
   "../../../hydra/src/build/hydra-opt-inspect"

optionsToOPTIONS :: FilePath -> HydraOptions -> IO ()
optionsToOPTIONS file options = 
  do writeFile cFile $ optionsToCFile options
     system $ unwords [ "gcc", "-o", genOpts, cFile ]
     system $ unwords [ "./" ++ genOpts, file ]
     return ()
  where
  cFile   = addExtension file "c"
  genOpts = addExtension file "genOpts"

optionsToCFile :: HydraOptions -> String
optionsToCFile options =
  unlines [ "#include <stdio.h>"
          , ""
          , "int main (int argc, char **argv)"
          , "{"
          , "  char * filename = argv[1];"
          , "  int x; "
          , "  double d;"
          , "  char c ;"
          , "  FILE* nf = fopen(filename, \"wb\") ;"
          , optionsToC options
          , "  fclose (nf) ;"
          , "  return 0 ;"
          , "}"
          ]

testOptions :: HydraOptions
testOptions = 
  HydraOptions { noOfTransitions        = length transitions
               -- , sizeOfTransType = 4
               -- , sizeOfInt       = 4
               -- , sizeOfLong      = 4
               , hoTransitions          = transitions
               , hoStateMeasures        = 0
               , hoCountMeasures        = 0
               , hoMaxStates            = 0
               , hoMaxIterations        = 5000
               , hoGenRepInterval       = 100000
               , hoSolRepInterval       = 5
               , hoMaxCPUTime           = 0
               , hoSafeHash             = 0
               , hoAccuracy             = 1e-10
               , hoRelaxParameter       = 1
               , hoDynamic              = 1
               , hoGenRepStyle          = BriefReport
               , hoSolRepStyle          = BriefReport
               , hoSolutionMethod       = DNAMsor
               , hoStateListFile        = "test.STATE"
               , hoSteadyStateFile      = "test.STEADY"
               , hoStartVectorfile      = ""
               , hoTransitionMatrixFile = "test.MATRIX"
               , hoCheckLength          = length checksum
               , hoChecksum             = checksum
               , hoTStart               = 0.1
               , hoTStop                = 10.0
               , hoTStep                = 0.1
               , hoSources              = 0
               , hoTargets              = 0
               , hoExcludeds            = 0
               , hoDoSteady             = True
               , hoDoPassage            = True
               , hoDoTransient          = True
--- stateListFile: "tiny.STATE"
--- steadyStateFile: "tiny.STEADY"
--- startVectorfile: ""
--- transitionMatrixFile: "tiny.MATRIX"
--- length: 33
--- checksum: 5ad25b5c05aa82eb32e207e93d78bd61
--- t_start: 0.1
--- t_stop: 10
--- t_step: 0.1
--- sources: 0
--- targets: 0
--- excludeds: 0
--- doSteady: false
--- doPassage: true
--- doTransient: false
               }
  where
  transitions = [ (Timed, 1)
                , (Timed, 2)
                , (Timed, 3)
                , (Instanteaous, 4)
                , (Timed, 5)
                ]
  checksum    = "5ad25b5c05aa82eb32e207e93d78bd61"


--  As an example this is the options output from hydra-opt-inspector
-- for the tiny.OPTIONS file in the hydra source code.

--- transitions: 8
--- sizeof(TransType): 4
--- sizeof(int): 4
--- sizeof(long): 4
    --- transitions[0]: timed
    --- priority[0]: 1
    --- transitions[1]: timed
    --- priority[1]: 1
    --- transitions[2]: timed
    --- priority[2]: 1
    --- transitions[3]: timed
    --- priority[3]: 1
    --- transitions[4]: timed
    --- priority[4]: 1
    --- transitions[5]: timed
    --- priority[5]: 1
    --- transitions[6]: timed
    --- priority[6]: 1
    --- transitions[7]: timed
    --- priority[7]: 1
--- hoStateMeasures: 0
--- countMeasures: 0
--- maxStates: 0
--- maxIterations: 5000
--- genRepInterval: 100000
--- solRepInterval: 5
--- maxCPUTime: 0
--- safeHash: 0
--- accuracy: 1e-10
--- relaxParameter: 1
--- dynamic: 1
--- sizeof(ReportStyle): 4
--- genRepStyle: brief
--- solRepStyle: brief
--- solutionMethod: sor
--- stateListFile: "tiny.STATE"
--- steadyStateFile: "tiny.STEADY"
--- startVectorfile: ""
--- transitionMatrixFile: "tiny.MATRIX"
--- length: 33
--- checksum: 5ad25b5c05aa82eb32e207e93d78bd61
--- t_start: 0.1
--- t_stop: 10
--- t_step: 0.1
--- sources: 0
--- targets: 0
--- excludeds: 0
--- doSteady: false
--- doPassage: true
--- doTransient: false

data HydraOptions =
  HydraOptions { noOfTransitions        :: Int
               -- , sizeOfTransType :: Int
               -- , sizeOfInt       :: Int
               -- , sizeOfLong      :: Int
               , hoTransitions          :: [ (TransType, Int) ]
               , hoStateMeasures        :: Int
               , hoCountMeasures        :: Int
               , hoMaxStates            :: Int
               , hoMaxIterations        :: Int
               , hoGenRepInterval       :: Int
               , hoSolRepInterval       :: Int
               , hoMaxCPUTime           :: Double
               , hoSafeHash             :: Int
               , hoAccuracy             :: Double
               , hoRelaxParameter       :: Double
               , hoDynamic              :: Int
--- sizeof(ReportStyle): 4
               , hoGenRepStyle          :: ReportStyle
               , hoSolRepStyle          :: ReportStyle
               , hoSolutionMethod       :: DNAMsolutionMethod
               , hoStateListFile        :: FilePath
               , hoSteadyStateFile      :: FilePath
               , hoStartVectorfile      :: FilePath
               , hoTransitionMatrixFile :: FilePath
               , hoCheckLength          :: Int
               , hoChecksum             :: String
               , hoTStart               :: Double
               , hoTStop                :: Double
               , hoTStep                :: Double
               , hoSources              :: Double
               , hoTargets              :: Double
               , hoExcludeds            :: Int
               , hoDoSteady             :: Bool
               , hoDoPassage            :: Bool
               , hoDoTransient          :: Bool
               }

data TransType   = Instanteaous | Timed
data ReportStyle = NoReport | BriefReport | FullReport

optionsToC :: HydraOptions -> String
optionsToC options =
  unlines [ writeInteger   $ noOfTransitions        options
          , writeIntegerArray transTypes
          , writeIntegerArray priorities
          , writeInteger   $ hoStateMeasures        options
          , writeInteger   $ hoCountMeasures        options
          , writeInteger   $ hoMaxStates            options
          , writeInteger   $ hoMaxIterations        options
          , writeInteger   $ hoGenRepInterval       options
          , writeInteger   $ hoSolRepInterval       options
          , writeDouble    $ hoMaxCPUTime           options
          , writeInteger   $ hoSafeHash             options
          , writeDouble    $ hoAccuracy             options
          , writeDouble    $ hoRelaxParameter       options
          , writeInteger   $ hoDynamic              options
          , writeInteger     genReport
          , writeInteger     solReport
          , writeInteger     solMethod
          , writeString    $ hoStateListFile        options
          , writeString    $ hoSteadyStateFile      options
          , writeString    $ hoStartVectorfile      options
          , writeString    $ hoTransitionMatrixFile options
          , writeInteger   $ hoCheckLength          options
          , writeCharArray $ hoChecksum             options
          , writeDouble    $ hoTStart               options
          , writeDouble    $ hoTStop                options
          , writeDouble    $ hoTStep                options
          , writeDouble    $ hoSources              options
          , writeDouble    $ hoTargets              options
          , writeInteger   $ hoExcludeds            options
          , writeBoolean   $ hoDoSteady             options
          , writeBoolean   $ hoDoPassage            options
          , writeBoolean   $ hoDoTransient          options
          ]
  where
  transitions = hoTransitions options
  transTypes  = map (intOfTransType . fst) transitions
  priorities  = map snd transitions

  genReport   = intOfReportStyle $ hoGenRepStyle options
  solReport   = intOfReportStyle $ hoSolRepStyle options
  solMethod   = intOfSolutionMethod $ hoSolutionMethod options

  writeInteger :: Int -> String
  writeInteger i =
    "  x = " ++ show i ++ ";\n" ++
    "  fwrite (&x, sizeof(int), 1, nf) ;"

  -- The array functions shouldn't be written like this
  -- we can be more efficient by outputting a static array.
  writeIntegerArray :: [ Int ] -> String
  writeIntegerArray = unlines . (map writeInteger)


  writeDouble :: Double -> String
  writeDouble d =
    "  d = " ++ show d ++ ";\n" ++
    "  fwrite (&d, sizeof(double), 1, nf) ;"

  writeChar :: Char -> String
  writeChar c =
    "  c = " ++ cShowChar c ++ ";\n" ++
    "  fwrite (&c, sizeof(char), 1, nf) ;"

  cShowChar :: Char -> String
  cShowChar ('\0') = "0"
  cShowChar c      = show c

  writeCharArray :: String -> String
  writeCharArray = unlines . (map writeChar)

  writeString :: String -> String
  writeString s = 
    writeCharArray fullString
    where
    fullString = take maxStringLength $ s ++ (repeat '\0')

  writeBoolean :: Bool -> String
  writeBoolean = writeInteger . intOfBoolean

  intOfBoolean :: Bool -> Int
  intOfBoolean (True)  = 1
  intOfBoolean (False) = 0

  intOfTransType :: TransType -> Int
  intOfTransType (Instanteaous) = 0
  intOfTransType (Timed)        = 1

  intOfReportStyle :: ReportStyle -> Int
  intOfReportStyle (NoReport)    = 0
  intOfReportStyle (BriefReport) = 1
  intOfReportStyle (FullReport)  = 2

  intOfSolutionMethod :: DNAMsolutionMethod -> Int
  intOfSolutionMethod (DNAMgauss)            = 0
  intOfSolutionMethod (DNAMgrassman)         = 2
  intOfSolutionMethod (DNAMgauss_seidel)     = 4
  intOfSolutionMethod (DNAMsor)              = 5
  intOfSolutionMethod (DNAMbicg)             = 8
  intOfSolutionMethod (DNAMcgnr)             = 6
  intOfSolutionMethod (DNAMbicgstab)         = 9
  intOfSolutionMethod (DNAMbicgstab2)        = 10
  intOfSolutionMethod (DNAMcgs)              = 7
  intOfSolutionMethod (DNAMtfqmr)            = error "eh?"
  intOfSolutionMethod (DNAMai)               = 12
  intOfSolutionMethod (DNAMair)              = 13
  intOfSolutionMethod (DNAMautomatic)        = 14

  -- A constant used for string lengths
  maxStringLength :: Int
  maxStringLength = 512


-- From ipc --run-hydra --source a --target a tests/basic/good/test01.pepa
-- 
-- test01.OPTIONS 
-- Initialising OptionsInspector...
-- Running input();
-- Information (0x805e008): reading data from 'tests/basic/good/test01.OPTIONS'.
--- transitions: 18
--- sizeof(TransType): 4
--- sizeof(int): 4
--- sizeof(long): 4
    --- transitions[0]: timed
    --- priority[0]: 1
    --- transitions[1]: timed
    --- priority[1]: 1
    --- transitions[2]: timed
    --- priority[2]: 1
    --- transitions[3]: timed
    --- priority[3]: 1
    --- transitions[4]: timed
    --- priority[4]: 1
    --- transitions[5]: timed
    --- priority[5]: 1
    --- transitions[6]: timed
    --- priority[6]: 1
    --- transitions[7]: timed
    --- priority[7]: 1
    --- transitions[8]: timed
    --- priority[8]: 1
    --- transitions[9]: timed
    --- priority[9]: 1
    --- transitions[10]: timed
    --- priority[10]: 1
    --- transitions[11]: timed
    --- priority[11]: 1
    --- transitions[12]: timed
    --- priority[12]: 1
    --- transitions[13]: timed
    --- priority[13]: 1
    --- transitions[14]: timed
    --- priority[14]: 1
    --- transitions[15]: timed
    --- priority[15]: 1
    --- transitions[16]: timed
    --- priority[16]: 1
    --- transitions[17]: timed
    --- priority[17]: 1
--- stateMeasures: 0
--- countMeasures: 0
--- maxStates: 0
--- maxIterations: 5000
--- genRepInterval: 100000
--- solRepInterval: 5
--- maxCPUTime: 0
--- safeHash: 0
--- accuracy: 1e-10
--- relaxParameter: 1
--- dynamic: 1
--- sizeof(ReportStyle): 4
--- genRepStyle: brief
--- solRepStyle: brief
--- solutionMethod: automatic
--- stateListFile: "tests/basic/good/test01.STATE"
--- steadyStateFile: "tests/basic/good/test01.STEADY"
--- startVectorfile: ""
--- transitionMatrixFile: "tests/basic/good/test01.MATRIX"
--- length: 33
--- checksum: 53c5075f2ac5fa08016ea71a2b0bb8ce
--- t_start: 1
--- t_stop: 10
--- t_step: 0.5
--- sources: 0
--- targets: 0
--- excludeds: 0
--- doSteady: false
--- doPassage: true
--- doTransient: false
--- OptionsInspector powered down...