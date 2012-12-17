module Main
  ( main )
where

{- Standard Library Modules Imported -}
import System.Cmd
  ( system )
import System.Environment
  ( getArgs )
{- External Library Modules Imported -}
{- Local Modules Imported -}
{- End of Imports -}


main :: IO ()
main = getArgs >>= processArgs


processArgs :: [ String ] -> IO ()
processArgs [ "--just-gnu" ]  = runGnuFiles
processArgs [ "--just-ipc" ]  = runIpcCommands
processArgs [ "--clean" ]     = cleanDirectory
processArgs [ "--clean-all" ] = cleanAll
processArgs []                = doEverything
processArgs _                 = putStrLn "Unknown arguments"


doEverything :: IO ()
doEverything =
  do runIpcCommands
     runGnuFiles

runIpcCommands :: IO ()
runIpcCommands = mapM_ runCommand allCommands

allCommands :: [ String ]
allCommands =
  concat [ pdfCommands
         , cdfCommands
         , ratePdfCommands
         , rateCdfCommands
         ]

runGnuFiles :: IO ()
runGnuFiles =
  do writeFile "pdf.gnuplot" pdfGnuContents
     writeFile "cdf.gnuplot" cdfGnuContents
     writeFile "rpdf.gnuplot" ratePdfGnuContents
     writeFile "rcdf.gnuplot" rateCdfGnuContents
     system "gnuplot pdf.gnuplot"
     system "eps2pdf pdfk.eps"
     system "gnuplot cdf.gnuplot"
     system "eps2pdf cdfk.eps"
     system "gnuplot rpdf.gnuplot"
     system "eps2pdf ratepdf.eps"
     system "gnuplot rcdf.gnuplot"
     system "eps2pdf ratecdf.eps"
     return ()


cleanDirectory :: IO ()
cleanDirectory = 
  do system $ unwords [ "rm -f "
                      , "*.OPTIONS *.gen *.gspn-target *.perf"
                      , "*.STATE *.STEADY *.WEIGHT *.INFO *.MATRIX *.TARGETS"
                      , "*.mod *.mod.out *.user.cxx *.user.o"
                      ]
                      
     return ()

cleanAll :: IO ()
cleanAll =
  do cleanDirectory
     system "rm -f *.PT_RESULTS *.gnuplot *.pdf *.png"
     return ()


-- We should probably count up the exit codes.
runCommand :: String -> IO ()
runCommand s = do putStrLn s
                  system s
                  return ()

kNumbers :: [ Int ]
kNumbers = [1..9]

rateValues :: [ Double ]
rateValues = [0.5, 0.8, 1.0, 1.3, 1.5, 2.0]

configurations :: [ (Int, Double) ]
configurations = [ (i, d) | i <- kNumbers, d <- rateValues ]

pdfCommands :: [ String ]
pdfCommands =
  map (mkIpcCommand Pdf) $ zip kNumbers (replicate (length kNumbers) 1.0)

cdfCommands :: [ String ]
cdfCommands =
  map (mkIpcCommand Cdf) $ zip kNumbers (replicate (length kNumbers) 1.0)


ratePdfCommands :: [ String ]
ratePdfCommands =
  map (mkIpcCommand Pdf) $ zip (replicate (length rateValues) 8) rateValues

rateCdfCommands :: [ String ]
rateCdfCommands =
  map (mkIpcCommand Cdf) $ zip (replicate (length rateValues) 8) rateValues


data DistFunction = Cdf | Pdf deriving Eq


-- Builds up an ipc command line which will probe
-- for the given length of activities.
-- The first argument specifies whether we want the
-- pdf or the cdf of the passage-time probabilities.
mkIpcCommand :: DistFunction -> (Int, Double) -> String
mkIpcCommand distFun (i, d) =
  unwords [ ipcCommand
          , "--rate r=" ++ (show d)
          , "--run-hydra"
          , "--start-time 0.1"
          , "--stop-time 20.0"
          , "--time-step 0.5"
          , "--mod-file"
          , modFile
          , pdfOpt
          , probeSpec
          , inputFile
          ]
  where
  (modFile, pdfOpt)
    | distFun == Cdf = (prefix ++ ".cdf.mod", "")
    | distFun == Pdf = (prefix ++ ".pdf.mod", "--pdf")
  prefix    = "erlang" ++ (show i) ++ "." ++ (show d)
  inputFile = "erlang.pepa"
  probeSpec = "--probe \"t:start, " ++ endAction ++ ":stop\""
  endAction = 't' : (show i)



ipcCommand :: String
ipcCommand = "../../../dist/build/ipc/ipc"


{-
  Make the pdf gnuplot file contents
-}
pdfGnuContents :: String
pdfGnuContents = 
  commonGnuContents title "pdfk" pdfPlotLines
  where
  title = "Comparison of pdf functions with rate parameter 1.0 and increasing erlang length"

cdfGnuContents :: String
cdfGnuContents = 
  commonGnuContents title "cdfk" cdfPlotLines
  where
  title = "Comparison of cdf functions with rate parameter 1.0 and increasing erlang length"

ratePdfGnuContents :: String
ratePdfGnuContents =
  commonGnuContents title "ratepdf" rateLines
  where
  rateLines = compareRateLines 8 "pdf"
  title = "Comparison of pfd erlang length 8 with varying rate parameter"

rateCdfGnuContents :: String
rateCdfGnuContents =
  commonGnuContents title "ratecdf" rateLines
  where
  rateLines = compareRateLines 8 "cdf"
  title = "Comparison of cdf of erlang length 8 with varying rate parameter"

commonGnuContents :: String -> String -> String -> String
commonGnuContents title outputPrefix plotLines =
  unlines [ "set terminal postscript color"
          , "set output \"" ++ outputFile ++ "\""
          , ""
          , "set title " ++ (surroundInQuotes title)
          , "set xlabel \"Time\""
          , "set ylabel \"Probability\""
          , plotLines
          ]
  where
  outputFile = outputPrefix ++ ".eps"




{-
  Make the pdf plot lines
-}
pdfPlotLines :: String
pdfPlotLines = makePlotLines "pdf"
                         
{-
  Make the plot lines for the cdf function
-}
cdfPlotLines :: String
cdfPlotLines = makePlotLines "cdf"

{-
  factor out the common functionality for making the plot lines for
  both cdfs and pdfs
-}
makePlotLines :: String -> String
makePlotLines distFun =
  "plot " ++ pdfPlots
  where
  pdfPlots    = commaSeparate pdfPlotters
  pdfPlotters = map makePlot kNumbers
  makePlot :: Int -> String
  makePlot i = unwords [ surroundInQuotes $ makeResultsFile i 1.0 distFun
                       , "title"
                       , "' k = " ++ show i ++ "' with lines" -- points (should be an option)
                       ]

compareRateLines :: Int -> String -> String
compareRateLines erlength distFun =
  "plot " ++ plots
  where
  plots    = commaSeparate plotters
  plotters = map makePlot rateValues
  makePlot :: Double -> String
  makePlot d = unwords [ surroundInQuotes $ makeResultsFile erlength d distFun
                       , "title"
                       , "'\\theta = " ++ show d ++ "' with lines"
                       ]
                        


makeResultsFile :: Int -> Double -> String -> String
makeResultsFile erlength rateValue distFun =
  concat [ "erlang", show erlength, ".", show rateValue
         , ".", distFun, ".PT_RESULTS" ]

commaSeparate :: [ String ] -> String
commaSeparate [] = ""
commaSeparate l  = foldr1 (\a -> \b -> a ++ ", " ++ b) l

surroundInQuotes :: String -> String
surroundInQuotes s = "\"" ++ s ++ "\""