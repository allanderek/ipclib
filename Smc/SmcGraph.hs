{-|
   The main module of the smc compiler, which compiles srmc models to
   batches of pepa models.
-}
module Main
  ( main )
where

{- Standard library modules imported -}
import qualified Data.List as List
import System.Cmd
  ( system )
import qualified System.FilePath as File
{- External Library modules imported -}
{- Local modules imported -}
import Ipc.Cli
  ( getCliArgs
  , toCli
  , CliOpt            ( .. )
  , Cli               ( .. )
  )
import Ipc.Ipc
  ( quoteCommand
  , surroundInQuotes
  )

-- Imports for the global data base stuff
import Smc.Cli
   ( smcVersion
   , SmcOptions       ( .. )
   , smcGraphOptions
   )
import Smc.DataBase
  ( SerialDataBase    ( .. )
  , DataBase          ( .. )
  , DataBaseEntry     ( .. ) 

  , RateValue
  , Time
  , PassageResults

  , SensitivityGraph   ( .. )
  , SensitivityGroup   ( .. )
  , CandleStickGraph   ( .. )
  , getCandleTimes
  , ThreeDGraph
  , TimeGraph          ( .. )

  , getDataBaseTimes
  , collateDataBase

  , printThreeDGraph
  , groupSensitivityGraphs
  , createCandleStick 
  , printCandleStick
  , getTimeGraphData
  )
import Language.Pepa.Syntax
  ( ProcessDef )
import Language.Pepa.Print
  ( hprintComponent
  , hprintComponentName
  )
import qualified Language.Pepa.Utils as Utils
{- End of module imports -}


main :: IO ()
main = getCliArgs >>= (processArgs . toCliSmcGraph)

toCliSmcGraph :: [ String ] -> Cli SmcOptions
toCliSmcGraph = toCli smcVersion "smcgraph" smcGraphOptions
   
processArgs :: Cli SmcOptions -> IO () 
processArgs (CliValid options inputFiles) = 
  mapM_ (processDataBase options) inputFiles
processArgs (CliInfo  _ _ infoString)     =
  putStrLn infoString
processArgs (CliError  _ _ errorString)   =
  putStrLn errorString

-- A type synonym to use for all the functions which require the
-- list of command-line options.
type CliOptions = [ CliOpt SmcOptions ]

processDataBase :: CliOptions -> FilePath -> IO ()
processDataBase options databasefile =
  do database <- getCollatedDataBase databasefile
     let -- obviously need to do some proper command-line arg processing
         sGraphs    = makeSensitivityGraphs options database
         prefix     = File.dropExtensions databasefile
         candles    = makeAllCandle prefix database
         texFile    = File.addExtension prefix "tex"
         tgGroups   = makeTimeGraphGroups options prefix database
         timeGraphs = fst tgGroups
         timeGroups = snd tgGroups
         stringMod  = dbStringModel database
     mapM_ writeOutTimeGraph timeGraphs
     makeCompletePdf options stringMod texFile sGraphs candles timeGroups

{-
  To get collated database we read in the serial database from
  the given file. If this is already a collated data base then
  we are done, if not then we must collate the data base and write
  out the new version to a file with an updated suffix.
-}
getCollatedDataBase :: FilePath -> IO (DataBase PassageResults)
getCollatedDataBase file =
  do dbContents <- readFile file
     let serialDB :: SerialDataBase
         serialDB = read dbContents
         dir      = File.takeDirectory file
     case serialDB of
       Collated    db -> return db
       NonCollated db -> 
         do collated <- collateDataBase dir db
            let newFile  = Utils.switchExtension "db" "col.db" file
                contents = show $ Collated collated
            writeFile newFile contents
            return collated

makeSensitivityGraphs :: CliOptions               -- ^ The command-line options
                      -> DataBase PassageResults  -- ^ The database
                      -> [ SensitivityGroup SensitivityGraph ]
makeSensitivityGraphs _options database =
  sGraphs
  where
  -- We renumber all the sensitivity graphs since there will be more than
  -- one which varies the same rate. We just do this so that we have unique
  -- names.
  sGraphs :: [ SensitivityGroup SensitivityGraph ]
  sGraphs = map renameNumberedGroup numberedGroups

  -- To rename the sensitivity graphs we first zip up the graphs with the
  -- positive integers and then use each associated integer to uniquify
  -- the name of the sensitivity graph.
  -- Then we do the same trick for re-numbering each sensitivity graph
  -- within each group, because even within each group there may/will be
  -- more than one sgraph which varies the same rate.
  renameNumberedGroup :: (Int, SensitivityGroup SensitivityGraph) 
                      -> SensitivityGroup SensitivityGraph
  renameNumberedGroup (i, group) = 
    group { sgroupGraphs =  map renameGraph numberedGraphs }
    where
    numberedGraphs :: [ (Int, SensitivityGraph) ]
    numberedGraphs = zip [0..] $ sgroupGraphs group
    
    renameGraph :: (Int, SensitivityGraph) -> SensitivityGraph
    renameGraph (j, sgGraph) =
      sgGraph { sgName = concat [ sgName sgGraph
                                , "--"
                                , show i
                                , "--"
                                , show j 
                                ] 
              }


  -- Each group of sensitivity graphs is given an identifying number
  numberedGroups :: [ (Int, SensitivityGroup SensitivityGraph) ]
  numberedGroups = zip [0..] sGraphGroups
  
  sGraphGroups :: [ SensitivityGroup SensitivityGraph ]
  sGraphGroups = map groupSensitivityGraphs $ dbGroups database


makeAllCandle :: String -- ^ The base name for the candle stick graphs
              -> DataBase PassageResults 
              -> [ CandleStickGraph ]
makeAllCandle prefix database =
  map makeSingleCandle percents
  where
  -- times are taken from the database but they should also be
  -- somehow configurable via the command-line
  times = chooseTenSpacedEqually $ getDataBaseTimes database

  -- These could also be at least command-line options
  percents = [ (10, 90)
             , (15, 85)
             , (20, 80)
             , (25, 75)
             , (30, 70)
             , (35, 65)
             , (40, 60)
             , (45, 55)
             ]

  makeSingleCandle :: (Int, Int) ->  CandleStickGraph
  makeSingleCandle (lowP, highP) =
    (createCandleStick prefix times lowP highP database)


-- The thing that this returns (inside the IO monad) is just a filepath
-- but conceivably we could return more about it than that.
makeCandlePdf :: CliOptions
           -> CandleStickGraph
           -> IO CandleStickGraph
makeCandlePdf options candle =
  do writeCandle candle
     writeFile gnufile gnucontents
     system gnucommand
     system epstopdfcommand
     return $ candle { candleName = pdfFile }
  where
  file            = candleName candle
  gnufile         = File.addExtension basefile "gnuplot"
  epsFile         = File.addExtension basefile "eps"
  pdfFile         = File.addExtension basefile "pdf"
  basefile        = Utils.dropGivenExt "candle" file
  gnucommand      = unwords [ "gnuplot", gnufile ]
  epstopdfcommand = unwords [ "eps2pdf -f", epsFile ]

  gnucontents     = unlines [ "set terminal postscript eps colour dash 20"
                            , quoteCommand "set output" epsFile
                            , ""
                            , titleCommand
                            , quoteCommand "set xlabel" xaxis
                            , quoteCommand "set ylabel" yaxis
                            , "set yrange [0:1]"
                            , "set xrange " ++ xrange
                            , "set boxwidth 0.2 relative"
                            , ""
                            , "# set pm3d"
                            , "plot " ++ (surroundInQuotes file) ++
                              " using 1:3:2:6:5 lt 1 lw 2 notitle" ++ 
                              -- title 'percentiles'" ++
                              " with candlesticks whiskerbars, \\"
                            , "''         using 1:4:4:4:4 with candlesticks lt -1 lw 1 notitle"
                            ]

  titleCommand    = makeGnuTitleCommand options title
  title           = "A candlestick graph"
  xaxis           = "Time"
  yaxis           = "Probability"


  xrange          = "[0:" ++ show xmax ++ "]"
  xmax            = 1 + (maximum $ getCandleTimes candle)

  writeCandle :: CandleStickGraph -> IO ()
  writeCandle cgraph =
    writeFile (candleName cgraph) $ printCandleStick cgraph


type CandleGraph = CandleStickGraph




writeOutTimeGraph :: TimeGraph -> IO ()
writeOutTimeGraph timeGraph =
  writeFile (timegraphName timeGraph) contents
  where
  contents = unlines $ map showLine (timegraphData timeGraph)
  showLine :: (Int, Double) -> String
  showLine (i, d) = unwords [ show i, show d ]


data TimeGraphGroup = 
  TimeGraphGroup { timeGroupName   :: FilePath
                 , timeGroupGraphs :: [ TimeGraph ]
                 }

makeTimeGraphGroups :: CliOptions
                    -> String                   -- ^ The prefix
                    -> DataBase PassageResults 
                    -> ( [ TimeGraph ], [ TimeGraphGroup ])
makeTimeGraphGroups _options prefix database =
  (timeGraphs, map makeGroup allGroups)
  where
  timeGraphs   = map (getTimeGraphData prefix database) times

  -- times are taken from the data base, but should also be configurable
  -- on the command-line.
  times = getDataBaseTimes database

  allGroups    = singleGroups ++ tGroups
  singleGroups = map (: []) times 
  -- Definitely should be getting these from the options.
  -- Also we can think of a few nicer automatic groups, such that
  -- for example the distance between items decreases, so for
  -- example from 1..10 we get 1 5 8 9 say.
  -- and the reverse increasing so we get say 1 2 4 7 10
  -- So far this should produce the groups consisting of
  -- every second time
  -- every third time
  -- every fourth time ...
  -- and so on until every xth time where x = n
  -- Some of the low numbers here might be a bit rubbish, 
  -- for example the graph with all of them in.
  -- At least the suitable test saying that it must be of length
  -- less than 16 should be overiddable.
  tGroups      = filter (isSuitable . length) $ 
                  map (flip everyNth times) [1..(length times)]
  -- Must be of length greater than two otherwise it falls into the
  -- single groups category and it must be less than 16 to avoid the
  -- dreaded two column key from gnuplot (which vastly narrows the graph).
  isSuitable :: Int -> Bool
  isSuitable i = i > 1 && i < 16

  makeGroup :: [ Time ] -> TimeGraphGroup
  makeGroup doubles = 
    TimeGraphGroup { timeGroupName   = name
                   , timeGroupGraphs = filter isInGroup timeGraphs 
                   }
    where
    name  = prefix ++ (List.intercalate "--" $ map lshowDouble doubles)
    isInGroup :: TimeGraph -> Bool
    isInGroup tg = elem (timegraphTime tg) doubles

    -- in the file name for an includegraphics latex recognises
    -- everything after the first '.' to be the extension, very annoying.
    -- So we cannot add doubles printed out the usual way, instead we'll
    -- change the dots to underscores, is it great? no.
    lshowDouble :: Double -> String
    lshowDouble = (map pointUnderscore) . show
    pointUnderscore :: Char -> Char
    pointUnderscore '.' = '_'
    pointUnderscore c   = c

makeCompletePdf :: -- | The command-line arguments
                   CliOptions
                   -- | The original srmc model in string form
                -> String
                   -- | The tex file to write the latex to
                -> FilePath
                   -- | The sensitivity groups
                -> [ SensitivityGroup SensitivityGraph ]
                   -- | Candle stick graphs
                -> [ CandleGraph ]      
                   -- | The proc no vs time graph groups
                -> [ TimeGraphGroup ]   
                -> IO ()
makeCompletePdf options stringModel texFile sGroups candles timeGroups =
  do pdfCandles    <- mapM (makeCandlePdf options) candles
     pdfSGroups    <- mapM (makeSensitivityGroupPdf options) sGroups
     pdfTimeGroups <- mapM (makeTimeGroupPdf options) timeGroups
     let doc = makeLatexDocument options
                                 stringModel 
                                 pdfSGroups 
                                 pdfCandles 
                                 pdfTimeGroups
     writeFile texFile doc
     system pdfCommand
     system pdfCommand
     return ()
  where
  pdfCommand = unwords [ "pdflatex", texFile ]

makeLatexDocument :: CliOptions
                     -- | String representation of the original srmc model
                  -> String  
                  -> [ SensitivityGroup SensitivityPdfs ] 
                  -> [ CandleGraph ]  
                  -> [ TimeGraphGroup ]
                  -> String
makeLatexDocument options stringModel sgroups candles timeGroups =
  unlines [ "\\documentclass[10pt,a4paper]{article}"
          , "\\usepackage{amssymb,amsbsy,verbatim,fancybox}"
          , "\\usepackage{graphicx}"
          , "\\usepackage[pdftex]{hyperref}"
          , "%"
          , ""
          , "% For displaying a single graph in a figure."
          , "\\newcommand{\\graphicfigure}[3]{"
          , "\\begin{figure}[ht]"
          , "\\includegraphics[scale=0.8]{#1}"
          , "\\caption{"
          , " \\label{#2}"
          , " #3"
          , " }"
          , "\\end{figure}"
          , "}"
          , ""
          , "\\title{Results Graphs}"
          , "\\author{smc}"
          , ""
          , ""
          , "\\begin{document}"
          , "\\maketitle"
          , ""
          , "\\tableofcontents"
          , "\\section{The srmc model}"
          , "\\begin{verbatim}"
          , stringModel
          , "\\end{verbatim}"
          , "\\section{Sensitivity Graphs}"
          , sensitivityGraphsTexIntroduction
          , sgroupSubSections
          , "\\section{Candle Stick Graphs}"
          , candleStickGraphsTexIntroduction
          , candleFigures
          , "\\section{Time-at Graphs}"
          , timeAtGraphsTexIntroduction
          , timeAtFigures
          , "\\listoffigures"
          , "\\end{document}"
          ]
  where
  candleFigures     = unlines $ map makeCandleFigure candles
  timeAtFigures     = unlines $ map makeTimeAtFigure timeGroups
  sgroupSubSections = unlines $ map makeSgroupSection sgroups

  makeCandleFigure :: CandleStickGraph -> String
  makeCandleFigure candle =
    makeGraphFigure file label caption
    where
    file    = candleName candle
    label   = "figure:graph:"  ++ (Utils.dropGivenExt "pdf" file)
    caption = unlines [ "The candle stick graph where the boxes represent"
                      , "a high of " ++ (show $ candleHigh candle)
                      , "and a low of " ++ (show $ candleLow candle)
                      ]

  makeTimeAtFigure :: TimeGraphGroup -> String
  makeTimeAtFigure tgroup =
    makeGraphFigure file label caption
    where
    file     = timeGroupName tgroup
    label    = "figure:graph:timeAt:" ++ (Utils.dropGivenExt "pdf" file)
    caption  = unlines [ "A time-at graph showing probability"
                       , "vs instance of model number at "
                         ++ captionEnd
                       ]

    -- The caption is different for groups of length one, in these groups
    -- we do not want to have a key and the caption should specify the time.
    -- Where there are more than one time graphs we need a key anyway so we
    -- needn't cutter up the caption with all the times.
    captionEnd = case timeGroupGraphs tgroup of
                   [ one ] -> "time: " ++ (show $ timegraphTime one)
                   _       -> "the times shown"

  makeSgroupSection :: SensitivityGroup SensitivityPdfs -> String
  makeSgroupSection sgroup = 
    unlines (header ++ sgraphFigures)
    where
    header        = [ "\\subsection{Sensitivity Graph Group}"
                    , "All the sensitivity graphs in this sub-section"
                    , "Relate to the following choices of process"
                    , "instantiations:"
                    , "\\begin{verbatim}"
                    , unlines $ map latexProcessDef (sgroupProcesses sgroup)
                    , "\\end{verbatim}"
                    , "\\newpage"
                    ]
    sgraphFigures
      | any isIntersperseOptions options = map makeSGraphFigure sgraphs
      | otherwise                        = sgraphSubsections
    sgraphSubsections = concat [ [ "\\subsubsection{The cdf graphs}" ]
                                , sgraphcdfs
                                , [ "\\subsubsection{The pdf graphs}" ]
                                , sgraphpdfs
                                ]
    sgraphcdfs        = map makeSGraphCDFFigure sgraphs
    sgraphpdfs        = map makeSGraphPDFFigure sgraphs
    sgraphs           = sgroupGraphs sgroup

  makeSGraphFigure :: SensitivityPdfs -> String
  makeSGraphFigure sgPdf = 
    unlines [ makeSGraphCDFFigure sgPdf
            , makeSGraphPDFFigure sgPdf
            ]

  makeSGraphCDFFigure :: SensitivityPdfs -> String
  makeSGraphCDFFigure sgPdf =
    unlines [ "The graphs in figure \\ref{" ++ cdfLabel ++ "}"
            , "show the effect of the rate " ++ variedRate ++ "."
            , otherRates
            , "\\begin{figure}[ht]"
            , "\\includegraphics[scale=0.8]{" ++ cdfFile ++ "}"
            , "\\includegraphics[scale=0.8]{" ++ cdfMultiFile ++ "}"
            , "\\caption{"
            , cdfCaption
            , "\\label{" ++ cdfLabel ++ "}"
            , "} % closes caption"
            , "\\end{figure}"              
            , showFileNames cdfFile cdfMultiFile
            , "\\clearpage"
            ]
    where
    sgGraph    = senPdfGraph sgPdf
    rateValues = sgRateValues sgGraph
    variedRate = escapeToLatex $ sgVariedRate sgGraph
    otherRates
      | null rateValues = ""
      | otherwise       = 
        unlines [ "The other rates which are used in this model"
                , "(and are defined by a rate array in the main srmc model)"
                , "are as follows:"
                , "\\begin{verbatim}"
                , unlines $ map latexRateValue rateValues
                , "\\end{verbatim}"
                ]

    showFileNames :: FilePath -> FilePath -> String
    showFileNames f1 f2 =
      unwords [ "The file names are:"
              , escapeToLatex f1
              , "and"
              , escapeToLatex f2
              ]

    cdfFile       = senPdf3dCdfDotPdf    sgPdf
    cdfMultiFile  = senPdfMultiCdfDotPdf sgPdf
    cdfLabel      = "figure:graph:"  ++ (Utils.dropGivenExt "pdf" cdfFile)
    cdfCaption    = concat  [ "Sensitivity graphs of cdfs for the rate: "
                            , variedRate
                            ]

  makeSGraphPDFFigure :: SensitivityPdfs -> String
  makeSGraphPDFFigure sgPdf =
    unlines ["The graphs in figure \\ref{" ++ pdfLabel ++ "}"
            , "show the effect of the rate " ++ variedRate ++ "."
            , otherRates
            , "\\begin{figure}[ht]"
            , "\\includegraphics[scale=0.8]{" ++ pdfFile ++ "}"
            , "\\includegraphics[scale=0.8]{" ++ pdfMultiFile ++ "}"
            , "\\caption{"
            , pdfCaption
            , "\\label{" ++ pdfLabel ++ "}"
            , "} % closes caption"
            , "\\end{figure}"
            , showFileNames pdfFile pdfMultiFile
            , "\\clearpage"
            ]
    where
    sgGraph    = senPdfGraph sgPdf
    rateValues = sgRateValues sgGraph
    variedRate = escapeToLatex $ sgVariedRate sgGraph
    otherRates
      | null rateValues = ""
      | otherwise       = 
        unlines [ "The other rates which are used in this model"
                , "(and are defined by a rate array in the main srmc model)"
                , "are as follows:"
                , "\\begin{verbatim}"
                , unlines $ map latexRateValue rateValues
                , "\\end{verbatim}"
                ]

    showFileNames :: FilePath -> FilePath -> String
    showFileNames f1 f2 =
      unwords [ "The file names are:"
              , escapeToLatex f1
              , "and"
              , escapeToLatex f2
              ]

    pdfFile       = senPdf3dPdfDotPdf    sgPdf
    pdfMultiFile  = senPdfMultiPdfDotPdf sgPdf
    pdfLabel      = "figure:graph:" ++ (Utils.dropGivenExt "pdf" pdfFile)
    pdfCaption    = concat  [ "Sensitivity graphs of pdfs for the rate: "
                            , variedRate
                            ]


  makeGraphFigure :: String -> String -> String -> String
  makeGraphFigure file label caption =
    unlines [ "\\graphicfigure{" ++ file ++ "}"
            , "               {" ++ label ++ "}"
            , "               {" ++ caption ++ "}"
            , "\\clearpage"
            ]

  latexProcessDef :: ProcessDef -> String
  latexProcessDef (name, comp) =
    unwords [ hprintComponentName name, "=", hprintComponent comp ]

  latexRateValue :: RateValue -> String
  latexRateValue (name, value) =
    unwords [ name
            , "="
            , show value
            ]


makeSensitivityGroupPdf :: CliOptions
                        -> (SensitivityGroup SensitivityGraph)
                        -> IO (SensitivityGroup SensitivityPdfs)
makeSensitivityGroupPdf options sgroup =
  do graphs <- mapM (makeSensitivityPdfs options) $ sgroupGraphs sgroup
     return $ sgroup { sgroupGraphs = graphs }

data SensitivityPdfs =
  SensitivityPdfs { senPdfGraph       :: SensitivityGraph
                  , senPdf3dPdfDotPdf :: FilePath
                  , senPdf3dCdfDotPdf :: FilePath
                  , senPdfMultiPdfDotPdf :: FilePath
                  , senPdfMultiCdfDotPdf :: FilePath
                  }


makeSensitivityPdfs :: CliOptions -> SensitivityGraph -> IO SensitivityPdfs
makeSensitivityPdfs options graph =
  do pdfPdf      <- make3dgraphPdf options outputGraphPdf
     cdfPdf      <- make3dgraphPdf options outputGraphCdf
     pdfMultiPdf <- makeMultiPlotPdf options outputMultiPdf
     cdfMultiCdf <- makeMultiPlotPdf options outputMultiCdf
     return SensitivityPdfs { senPdfGraph          = graph
                            , senPdf3dPdfDotPdf    = pdfPdf
                            , senPdf3dCdfDotPdf    = cdfPdf
                            , senPdfMultiPdfDotPdf = pdfMultiPdf
                            , senPdfMultiCdfDotPdf = cdfMultiCdf
                            }
  where
  variedRate      = sgVariedRate graph
  file            = sgName graph

  outputGraphPdf  = 
    Output3d { output3dBase  = fileName ++ "-pdf" 
             , output3dData  = sgDataPdf graph
             , output3dXaxis = variedRate
             , output3dYaxis = "Time"
             , output3dZaxis = "Pd"
             , output3dTitle = "A pdf Sensitivity graph"
             , output3dView  = (130, 40, 1, 1)
             }
  outputGraphCdf  =
    outputGraphPdf { output3dBase  = fileName ++ "-cdf"
                   , output3dData  = sgDataCdf graph
                   , output3dZaxis = "Prob"
                   , output3dTitle = "A cdf Sensitivity graph"
                   , output3dView  = (60, 30, 1, 1)
                   }


  outputMultiPdf  = 
    OutputMultiPlot { outputMultiBase   = multiPdfBase
                    , outputMultiTitle  = "A pdf multi-line sensitivity graph"
                    , outputMultiLines  = map makePdfLine $ sgEntries graph
                    , outputMultiXaxis  = "Time"
                    , outputMultiYaxis  = "Probability Density"
                    , outputMultiKey    = "top right"
                    }
  multiPdfBase    = fileName ++ "-pdf-multi"
  outputMultiCdf  =
    outputMultiPdf { outputMultiBase    = multiCdfBase
                   , outputMultiTitle   = "A cdf multi-line sensitivity graph"
                   , outputMultiLines   = map makeCdfLine $ sgEntries graph
                   , outputMultiKey     = "bottom right"
                   }
  multiCdfBase    = fileName ++ "-cdf-multi"
  fileName
    | extension == "sgraph" = filePart
    | otherwise             = file
  (filePart, extension) = File.splitExtension file

  makeCdfLine :: (Double, DataBaseEntry PassageResults) -> Line2D
  makeCdfLine (d, entry) = (show d, dbPassageCdfCsv entry)

  makePdfLine :: (Double, DataBaseEntry PassageResults) -> Line2D
  makePdfLine (d, entry) = (show d, dbPassagePdfCsv entry)





data Output3d = 
  Output3d { output3dBase  :: FilePath    -- ^ The basename from which to 
                                          --   derive the names of the 
                                          --   intermediate and output files
           , output3dData  :: ThreeDGraph -- ^ data to draw
           , output3dXaxis :: String      -- ^ The name for the x-axis
           , output3dYaxis :: String      -- ^ The name for the y-axis
           , output3dZaxis :: String      -- ^ The name for the z-axis
           , output3dTitle :: String      -- ^ The title for the graph
           , output3dView  :: (Int, Int, Int, Int) -- ^ The viewing angle
           }


make3dgraphPdf :: CliOptions -> Output3d -> IO FilePath
make3dgraphPdf options output3d =
  do writeFile datafile (printThreeDGraph $ output3dData output3d)
     writeFile gnufile gnucontents
     system gnucommand
     system epstopdfcommand
     return pdfFile
  where
  file            = output3dBase output3d
  gnufile         = File.addExtension file ".gnuplot"
  epsfile         = File.addExtension file ".eps"
  pdfFile         = File.addExtension file ".pdf"
  datafile        = File.addExtension file ".dat"
  gnucommand      = unwords [ "gnuplot", gnufile ]
  epstopdfcommand = unwords [ "eps2pdf -f", epsfile ]
     
  gnucontents     = unlines [ "set terminal postscript eps colour dash 14"
                            , "set output \"" ++ epsfile ++ "\""
                            , ""
                            , titleCommand
                            , quoteCommand "set xlabel" $ output3dXaxis output3d
                            , quoteCommand "set ylabel" $ output3dYaxis output3d
                            , quoteCommand "set zlabel" $ output3dZaxis output3d
                            , viewCommand
                            -- , "set cbrange [0:1]"
                            , ""
                            , "set pm3d"
                            , "set ticslevel 0"
                            , "splot \"" ++ datafile ++ 
                              "\" notitle with lines"
                            ]

  (rotX, rotY,
   scaleX, scaleY) = output3dView output3d
  viewCommand      = concat [ "set view "
                            , show rotX, ","
                            , show rotY, ","
                            , show scaleX, ","
                            , show scaleY
                           ]
  titleCommand     = makeGnuTitleCommand options $ output3dTitle output3d



data OutputMultiPlot =
  OutputMultiPlot { outputMultiBase   :: FilePath   -- ^ The base name like
                                                    --   'output3dBase'
                  , outputMultiTitle  :: String     -- ^ The title of the graph
                  , outputMultiLines  :: [ Line2D ] -- ^ The lines to draw
                  , outputMultiXaxis  :: String     -- ^ Label for the xaxis
                  , outputMultiYaxis  :: String     -- ^ Label for the yaxis
                  , outputMultiKey    :: String     -- ^ Where to place the key
                  }

type Line2D = (String, FilePath)

makeMultiPlotPdf :: CliOptions      -- ^ The command-line options
                 -> OutputMultiPlot -- ^ The multi-plot to draw
                 -> IO FilePath     -- ^ The returned pdf graph
makeMultiPlotPdf options outputMulti =
  do writeFile gnufile gnucontents
     system gnucommand
     system epstopdfcommand
     return pdfFile
  where
  file            = outputMultiBase outputMulti
  gnufile         = File.addExtension file ".gnuplot"
  epsfile         = File.addExtension file ".eps"
  pdfFile         = File.addExtension file ".pdf"

  gnucommand      = unwords [ "gnuplot", gnufile ]
  epstopdfcommand = unwords [ "eps2pdf -f", epsfile ]
     
  gnucontents     = unlines [ "set terminal postscript eps colour dash 20"
                            , "set output \"" ++ epsfile ++ "\""
                            , ""
                            , titleCommand
                            , quoteCommand "set xlabel" xaxis
                            , quoteCommand "set ylabel" yaxis
                            -- , "set cbrange [0:1]"
                            , "set key " ++ (outputMultiKey outputMulti)
                            , ""
                            , "plot " ++ plotLines

                            ]

  titleCommand    = makeGnuTitleCommand options $ outputMultiTitle outputMulti
  xaxis           = outputMultiXaxis outputMulti
  yaxis           = outputMultiYaxis outputMulti
  
  plotLines       = List.intercalate ", " 
                    (map makePlot $ outputMultiLines outputMulti)
  
  makePlot :: Line2D -> String
  makePlot (s, filename) =
    unwords [ surroundInQuotes filename
            , "title"
            , "'" ++ s ++ "'"
            , "with linespoints"
            ]



makeTimeGroupPdf :: CliOptions -> TimeGraphGroup -> IO TimeGraphGroup
makeTimeGroupPdf options timeGraphs =
  do writeFile gnufile gnucontents
     system gnucommand
     system epstopdfcommand
     return $ timeGraphs { timeGroupName = newName }
  where
  origName        = timeGroupName timeGraphs
  newName         = File.addExtension origName "pdf"
  gnufile         = File.addExtension origName "gnuplot"
  epsfile         = File.addExtension origName "eps"
  epstopdfcommand = unwords [ "eps2pdf -f", epsfile ]
  gnucommand      = unwords [ "gnuplot", gnufile ]

  gnucontents     = unlines [ "set terminal postscript eps colour dash 20"
                            , quoteCommand "set output" epsfile
                            , ""
                            , titleCommand
                            , quoteCommand "set xlabel" xaxis
                            , quoteCommand "set ylabel" yaxis
                            , "set yrange [0:1]"
                            , "set xrange [0:" ++ (show numInstances) ++ "]"
                            , "set key outside right bottom"
                            , ""
                            , plotCommand
                            ]

  -- If there is only one time graph to plot then we give the plot no
  -- title and just put the value in the actual title to the whole graph.
  graphs          = timeGroupGraphs timeGraphs
  isSingle        = 1 == length graphs
  headGraph       = head graphs
  titleCommand    = makeGnuTitleCommand options title
  title
    | isSingle    = commonTitle ++ " time = " ++ 
                    (show $ timegraphTime headGraph)
    | otherwise   = commonTitle
  commonTitle     = "A probability at time graph"
  xaxis           = "Instance Number"
  yaxis           = "Probability"

  plotCommand
    | isSingle  = unwords [ "plot"
                          , surroundInQuotes $ timegraphName headGraph
                          , "with lines notitle"
                          ]
    | otherwise = "plot " ++ (List.intercalate ", " plots)

  plots           = map makePlot graphs
  makePlot :: TimeGraph -> String
  makePlot tg = unwords [ surroundInQuotes $ timegraphName tg
                        , "with lines title"
                        , "'time = " ++ (show $ timegraphTime tg) ++ "'"
                        ]

  numInstances
    | null graphs = 0
    | otherwise   = length $ timegraphData (head graphs)


-- Santitises a string for use in latex
-- currently this just involves escaping any underscore characters.
escapeToLatex :: String -> String
escapeToLatex ""            = ""
escapeToLatex ('_' : rest)  = '\\' : '_' : (escapeToLatex rest)
escapeToLatex (h : rest)    = h : (escapeToLatex rest)

sensitivityGraphsTexIntroduction :: String
sensitivityGraphsTexIntroduction =
  unlines
    [ "A sensitivity graph is depicts how varying a single rate"
    , "alters the probabilities of completion of the passage"
    , "in which we are interested."
    , "Each sensitivity graph belongs to a group, each group"
    , "represents one selection of the process instances."
    , "A group of sensitivity graphs therefore relate to one PEPA"
    , "model but the pepa model in question will have different rates"
    , "for some of the graphs in that group."
    ]

candleStickGraphsTexIntroduction :: String
candleStickGraphsTexIntroduction =
  unlines
    [ "A candle stick graph is ..."
    ]

timeAtGraphsTexIntroduction :: String
timeAtGraphsTexIntroduction =
  unlines
    [ "A time-at graph is ..."
    ]

{-
  Probably would be possible to make this more generic
  by first producing a list of ten indexes into the
  list and then taking from the list those incidices.
-}
chooseTenSpacedEqually :: [ a ] -> [ a ]
chooseTenSpacedEqually l
  | size <= 10 = l
  | otherwise  = take 10 $ takeSpaced 0 0 l
  where
  size = fromIntegral $ length l
  gap = size / 10
  
  takeSpaced :: Double -> Double -> [ a ] -> [ a ]
  takeSpaced _current _next []        = []
  takeSpaced current  next (h : rest)
    | next <= current = h : (takeSpaced (current + 1) (next + gap) rest)
    | otherwise       = takeSpaced (current + 1) next rest

{-
  We select every nth item from a list.
-}
everyNth :: Int -> [ a ] -> [ a ]
everyNth n
  | n < 1     = error "everyNth with n less than 1"
  | otherwise = everyNthWith 1
  where
  everyNthWith :: Int -> [ a ] -> [ a ]
  everyNthWith _ []         = []
  everyNthWith x (h : rest)
    | x >= n                = h : (everyNthWith 1 rest)
    | otherwise             = everyNthWith (x + 1) rest
  


{-
  A utility function for creating a gnuplot title command based on
  the given title, but if the options specify no title then of course
  we omit it.
-}
makeGnuTitleCommand :: CliOptions -> String -> String
makeGnuTitleCommand options title
  | any isNoGraphTitles options = "# no title command"
  | otherwise                   = quoteCommand "set title"  title

-- For checking presence of the command-line options
isNoGraphTitles :: CliOpt SmcOptions -> Bool
isNoGraphTitles (CliNonStandard NoGraphTitles) = True
isNoGraphTitles _                              = False

isIntersperseOptions :: CliOpt SmcOptions -> Bool
isIntersperseOptions (CliNonStandard IntersperseCdfsPdfs) = True
isIntersperseOptions _                                    = False


---
