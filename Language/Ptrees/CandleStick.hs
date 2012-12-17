module Language.Ptrees.CandleStick
  ( makeAllCandle
  , makeCandlePdf
  , PassageDb
  , PassageDbEntry
  )
where

{- Standard Library Modules Imported -}
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import System.Cmd
  ( system )
{- External Library Modules Imported -}
{- Local Modules Imported -}
import qualified Language.Ptrees.Syntax as Ptrees
import Language.Ptrees.DataBase
  ( DataBase        ( .. )
  , DataBaseEntry   ( .. )
  )
import qualified Language.Pepa.Compile.Uniformise as Uniformise
{- End of Module Imports -}

-- type Time          = Double
-- type Name          = String
-- type Probability   = Double
-- type Expr          = Int

type PassageDb      = DataBase Ptrees.DataBaseTag Uniformise.PassageResult
type PassageDbEntry = DataBaseEntry Ptrees.DataBaseTag Uniformise.PassageResult

makeAllCandle :: String -- ^ The base name for the candle stick graphs
              -> PassageDb  -- ^ The database from which to extract results
              -> [ CandleStickGraph ]
makeAllCandle prefix database =
  map makeSingleCandle percents
  where
  -- times are taken from the database but they should also be
  -- somehow configurable via the command-line
  times = [ 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0
          , 7.0, 8.0, 9.0, 10.0 ]


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


makeCandlePdf :: CandleStickGraph -> IO CandleStickGraph
makeCandlePdf candle =
  do writeCandle candle
     writeFile gnufile gnucontents
     system gnucommand
     system epstopdfcommand
     return $ candle { candleName = pdfFile }
  where
  file            = candleName candle
  gnufile         = switchSuffix ".candle" ".gnuplot" file
  epsFile         = switchSuffix ".candle" ".eps"     file
  pdfFile         = switchSuffix ".candle" ".pdf"     file
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

  titleCommand    = unwords [ "set title"
                            , "'top ="
                            , show $ candleHigh candle
                            , "bottom ="
                            , (show $ candleLow candle) ++ "'"
                            ]
  -- title           = "A candlestick graph"
  xaxis           = "Time"
  yaxis           = "Probability"


  xrange          = "[0:" ++ show xmax ++ "]"
  xmax :: Double
  xmax            = 1 + 10.0 -- (maximum $ getCandleTimes candle)

  writeCandle :: CandleStickGraph -> IO ()
  writeCandle cgraph =
    writeFile (candleName cgraph) $ printCandleStick cgraph

{-|
  Create a candle stick graph from the given data base and parameters.
  A candle stick graph uses all the entries within a database.
-}
createCandleStick :: String                  -- ^ A prefix for the name
                  -> [ Double ]              -- ^ The time values
                  -> Int                     -- ^ The low percentage
                  -> Int                     -- ^ The high percentage
                  -> PassageDb               -- ^ The data base
                  -> CandleStickGraph
createCandleStick prefix times lowP highP database =
  CandleStickGraph { candleName  = name
                   , candleLow   = lowP
                   , candleHigh  = highP
                   , candleLines = cLines
                   }
  where
  -- the name of the file where the data will be written to in order
  -- for gnuplot to read it.
  name            = concat [ prefix
                           , "--"
                           , show lowP
                           , "--"
                           , show highP
                           , ".candle"
                           ]
  -- The list of lines, these lines are the lines
  -- in the candle stick graph file, and will represent a vertical
  -- line with a box on it on the graph.
  cLines          = map gatherOneLine times
  entries :: [ PassageDbEntry ]
  entries         = dbEntries database

  -- Okay this gathers up all the probabilities
  -- at the given time. Currently if a set of results
  -- doesn't contain the given time then we error.
  -- It's possible that instead of error-ing we wish
  -- to just discount that entry. To do that we could
  -- just use 'mapMaybe'
  gatherOneLine :: Double -> CandleStickLine
  gatherOneLine time =
    CandleStickLine { candleLineTime    = time
                    , candleLineBottom  = bottom
                    , candleLineOpening = opening
                    , candleLineMedian  = median
                    , candleLineClosing = closing
                    , candleLineTop     = top
                    }
    where
    probabilities = map (getCdfProbabilityAtTime . dbeResult) entries
    bottom        = head sortedProbs
    top           = last sortedProbs
    getCdfProbabilityAtTime :: Uniformise.PassageResult -> Double
    getCdfProbabilityAtTime =
      Maybe.fromMaybe (error "No cdf at that time") .
      lookup time .
      Uniformise.cdfResult
    -- getCdfProbabilityAtTime (LazyResult ptree)

    -- This arithmetic is not particularly accurate!!
    opening       = sortedProbs !! (div (size * lowP) 100)
    closing       = sortedProbs !! (div (size * highP) 100)
    median        = sortedProbs !! (div (size * 50) 100)

    size          = length sortedProbs
    sortedProbs   = List.sort probabilities

{-|
  A candle stick graph is simply a set of candle stick lines
  however we retain the information used to create the lines
  in particular the high and low percentage values which are
  used to calculate the opening and closing of the boxes in the
  middle of each candlestick line. We retain such information
  in order to print it out and actually say what the graph is
  showing.
-}
data CandleStickGraph = 
  CandleStickGraph { candleName   :: String
                   , candleLow    :: Int
                   , candleHigh   :: Int
                   , candleLines  :: [ CandleStickLine ]
                   }

{-|
  A line in a candle stick graph, more or less speaks for itself really.
-}
data CandleStickLine = 
  CandleStickLine { candleLineTime    :: Double
                  , candleLineBottom  :: Double
                  , candleLineOpening :: Double
                  , candleLineMedian  :: Double
                  , candleLineClosing :: Double
                  , candleLineTop     :: Double
                  }


{-|
  Printing a candle stick graph in a format suitable for gnuplot
  is pretty trivial.
-}
printCandleStick :: CandleStickGraph -> String
printCandleStick candleStick =
  unlines $ map printCandleLine (candleLines candleStick)
  where
  printCandleLine :: CandleStickLine -> String
  printCandleLine line =
    unwords [ show $ candleLineTime    line
            , show $ candleLineBottom  line
            , show $ candleLineOpening line
            , show $ candleLineMedian  line
            , show $ candleLineClosing line
            , show $ candleLineTop     line
            ]

{-
  Switches a suffix, this allows us to for example easily get the mod file
  from the pepa file, that is
  [-switchSuffix ".pepa" ".mod" "file.pepa" -]
  will evaluate to
  [-"file.mod"-]
-}
switchSuffix :: String -> String -> FilePath -> FilePath
switchSuffix currentSuffix desiredSuffix fileName =
    (removeSuffix currentSuffix fileName) ++ desiredSuffix

-- Produces a gnuplot command where the command is the first string
-- and the argument is the second string. Where the argument is
-- to be surrounded in quotes.
quoteCommand :: String -> String -> String
quoteCommand command arg =
  command ++ " " ++ (surroundInQuotes arg)

surroundInQuotes :: String -> String
surroundInQuotes s = "\"" ++ s ++ "\""

{-
  Removes the given suffix if it is the suffix of the file path given,
  otherwise leaves the file path untouched.
-}
removeSuffix :: String -> FilePath -> FilePath
removeSuffix ext fileName
  | ext == fileExt = fileBase
  | otherwise      = fileName
  where 
  (fileBase, fileExt) = splitFileExt fileName

{-
  \subsection{File Extensions}
  A couple of functions to operate over filename extensions, I sort of
  expected these to be in the standard libraries but I cannot find them
  if anyone knows them, please take these out and replace uses of them,
  but please leave a comment here stating where you found them.

  Update: There has been a discussion on the Haskell-Cafe mailing list
  and an announcement of a release of a library module
  [-System.FilePath-]. I think it is worth waiting until that is in
  a released version of ghc before switching over to it, but we
  then should do that.

  Alternatively just make these synonyms ie.
  [-removeSuffix = Some.Library.function-]
  I think it can actually be found in
  [-Distribution.Compat.FilePath-] but I don't seem to be allowed to
  import that module.

  The first of these [-splitSuffix-] returns the base name and the extension
  the dot is included as part of the extension.
-}
splitFileExt :: FilePath -> (String, String)
splitFileExt []              = ([], [])
splitFileExt ('.' : rest)
    | restSuf == [] = ([], '.' : restPre)
    | otherwise     = (('.' : restPre), restSuf)
    where (restPre, restSuf) = splitFileExt rest
splitFileExt (first : rest)  = 
    ((first : restPre) , restSuf)
    where (restPre, restSuf) = splitFileExt rest
