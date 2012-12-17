{-|
  This module implements a very simple data base module
  for the collation, selection and display of results
  from an srmc model.
-}
module Smc.DataBase
  ( -- * Types representing databases and database components
    SerialDataBase    ( .. )
  , DataBase          ( .. )
  , DataBaseGroup     ( .. )
  , DataBaseEntry     ( .. ) 
  , rateValueOfEntry
  , RateValue
  , RateValueArray
  , RateFilter
  , Time
  , Probability 
  , PassageResults

  -- * Types of graphs and associated operations.
  , SensitivityGraph   ( .. )
  , SensitivityGroup   ( .. )
  , CandleStickGraph   ( .. )
  , CandleStickLine    ( .. )
  , getCandleTimes
  , TwoDGraph
  , ThreeDGraph
  , TimeGraph          ( .. )
  -- * Operations on whole databases.
  , databaseEntries
  , getDataBaseTimes
  , collateDataBase

  -- Operations to make and print graphs
  , printThreeDGraph
  , groupSensitivityGraphs
  , createCandleStick 
  , printCandleStick
  , getTimeGraphData
  )
where

{- Standard Library Modules Imported -}
import Control.Arrow
  ( second )
import Control.Monad
  ( liftM )
import Data.List
  ( sortBy
  , sort
  )
import System.FilePath
  ( combine )
{- External Library Modules Imported -}
{- Local Modules Imported -}
import Language.Pepa.Syntax
  ( ProcessDef )
{- End of Imports -}

{-|
  For smc graph it is useful to be able to serialise both a collated
  and a non-collated form the database. The non-collated form such that
  each entry has the name of the file storing the passage-time results
  for that model instance. The collated form has read in all such
  result files and has the passage-time results stored explicitly.
  This non-collated form the 'smc' compiler can output when the results
  files do not yet exist since the appropriate runs of ipc have not yet
  been done. However the collated form is nicer to work with since every
  function need not be done within the IO monad.
-}
data SerialDataBase =
    NonCollated (DataBase FilePath)
  | Collated    (DataBase PassageResults)
  deriving (Show, Read)

{-|
  The type of a results database.
  We generalise over the type of a set of results since we can use
  this database type for both the pre-collated and the post-collated
  results. The pre-collated results this will be what smc originally
  outputs and the results type will be a file telling you where you
  can access those results.

  The post-collated results will be an actual data type and will be
  the result of reading the files and interepreting the contents.
-}
data DataBase a = 
  DataBase { -- | A string representation of the original srmc model
             dbStringModel :: String 
           , dbGroups      :: [ DataBaseGroup a ]
           }
           deriving (Show, Read)

{-|
  A data-base group is intended to store all of the data-base entries
  associated with a single 'pepa' model. That is all of the entries
  within a data-base group all have the same process instantiations.
  Of course they may have different rate parameter instantiations.
-}
data DataBaseGroup a =
  DataBaseGroup { dbgRateArrays :: [ RateValueArray ]
                , dbgEntries    :: [ DataBaseEntry a ]
                -- These are the process instantiations, probably the incorrect
                -- type for this but it will do for now since we only wish to be
                -- able to print it out in the document. However later we will
                -- need to be able to filter on it so we will need a better type.
                , dbgProcesses  :: [ ProcessDef ]
                }
  deriving (Show, Read)

{-|
  A data base entry stores the results from a single run of a single pepa model.
  We store in this the rate values that were used during this run of the given
  pepa model.
  The rates are stored as a filter, since a rate filter defines the values of
  the rate parameters. Of course this is the only instance
  *for this particular model* which will satisfy this filter.
  If another model which has different process instantiations but uses the 
  same variable rates (and perhaps more) then it would also satisfy this rate
  filter.
-}
data DataBaseEntry a =
  DataBaseEntry { dbPassageCdf    :: a
                , dbPassagePdf    :: a
                , dbPassageCdfCsv :: FilePath
                , dbPassagePdfCsv :: FilePath
                , dbRates         :: RateFilter
                -- process instantiations
                }
  deriving (Show, Read)


{-|
  Returns the value of a rate within a single database entry
-}
rateValueOfEntry :: String -> DataBaseEntry a -> Maybe Double
rateValueOfEntry r = lookup r . dbRates 
  

-- | A rate value maps/sets a given name to a given value
type RateValue      = (String, Double)
-- | A rate value array maps a given rate name to a list of possible values
type RateValueArray = (String, [ Double ] )
-- | A rate filter is a list of rate values mapping a set of rate names
-- to single values. This allows us to pick out all the entries within a group
-- of entries which have their rates set to some combination.
-- For eample if one group of results varies the three rates r1, r2 and r3 and
-- we wish to see the effect of varying rate r1, then we may pick out all the
-- results in which r2=1.0 and r3=1.0.
type RateFilter = [ RateValue ]

-- | A time within a results file
type Time        = Double
-- | A probability within a results file.
type Probability = Double

-- | The type of a set of passage-time results. These simply map times to
-- probabilities.
type PassageResults = [ (Time, Probability) ]

{-|
  All the times involved in a single set of passage-time results.
-}
passageResultsTimes :: PassageResults -> [ Time ]
passageResultsTimes = map fst



{-|
  Sensitivity Graphs, must retain information about which rates (and processes)
  where used and varied etc.
  A sensitivity graph is obtained by applying a rate filter which sets all
  but one of the variables rates associated with a single pepa model.
  Because all but one of the variable rates remain constant the graph shows the
  effect that the single varied rate has on the model (with respect to the given
  rate filter, obviously if a different rate filter which sets the same rates
  but to different values is applied this may change how the one variable rate
  affects the model).
-}
data SensitivityGraph =
  SensitivityGraph { sgName       :: String
                   , sgDataCdf    :: ThreeDGraph
                   , sgDataPdf    :: ThreeDGraph
                   , sgVariedRate :: String
                   , sgRateValues :: RateFilter
                   , sgEntries    :: [ (Double, DataBaseEntry PassageResults) ]
                   }


{-|
  A sensitivity graph group is a group of sensitivity graphs all which
  relate to the same model (or process instantiations).

  We parameterise the type of the graphs because once we have translated
  a graph into pdf files we wish to store 
  (as well as the graph information itself) data about the pdf files.
-}
data SensitivityGroup a =
  SensitivityGroup { sgroupGraphs    :: [ a ]
                   , sgroupProcesses :: [ ProcessDef ]
                   }

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


{-| Returns all the times of a candle stick graph -}
getCandleTimes :: CandleStickGraph -> [ Time ]
getCandleTimes = (map candleLineTime) . candleLines


{-|
  A two dimensional graph consists of mapping doubles to doubles.
-}
type TwoDGraph   = [ (Double, Double) ]
{-|
  A three-dimensional graph is essentially a set of two-dimensional
  graphs where each two-dimensional graph is given a value.
-}
type ThreeDGraph = [ (Double, [ (Double, Double) ] ) ]



{-|
  A time graph shows process number against the probability at a given
  time. Often we wish to combine several of these into one plot.
-}
data TimeGraph = TimeGraph { timegraphName :: String
                           , timegraphTime :: Time
                           , timegraphData :: [ (InstanceNumber, Double) ]
                           }
-- | An instance number is really an 'EntryNumber'. Instance used here means
--   a particular pepa model where by we distinguish pepa models not only
--   by the process instantiations but also the rate parameter values.
type InstanceNumber = Int


{-|
  Return all the entries within a data base, essentially then looks past
  all the grouping and just returns the entries as a list.
  This is useful for things like making up the instance number vs probability
  graphs and the candlestick graphs.
-}
databaseEntries :: DataBase a -> [ DataBaseEntry a ]
databaseEntries = 
  concatMap dbgEntries . dbGroups


{-|
  This is a little bit of a kludge, it assumes that all entries in the
  data base define the same times in the passage-time results.
  It also of course fails if there are no entries in the database.
-}
getDataBaseTimes :: DataBase PassageResults -> [ Time ]
getDataBaseTimes database =
  passageResultsTimes $ dbPassageCdf oneEntry
  where
  oneEntry = head $ databaseEntries database



{-|
  Collating a data-base consists of turning a database in which
  all the results are references to files into one in which
  the results have been read in from file and interpreted.

  NOTE: here we assume that the .db (or .col.db) file is situated
  in the same directory as the .PT_RESULTS files.
-}
collateDataBase :: FilePath -> DataBase FilePath -> IO (DataBase PassageResults)
collateDataBase dir db =
  do newGroups <- mapM collateEntrySet $ dbGroups db
     return DataBase { dbStringModel = dbStringModel db
                     , dbGroups      = newGroups
                     }
  where
  -- collating on entry set is simple, we just collate each entry in the set
  collateEntrySet :: DataBaseGroup FilePath
                  -> IO ( DataBaseGroup PassageResults )
  collateEntrySet g = 
    do entries <- mapM collateEntry $ dbgEntries g
       return $ g { dbgEntries = entries }

  -- Collating a single entry is pretty simple we just read in the
  -- results file and replace the 'dbPassage' file of the entry
  -- with the interpreted contents of the results file.
  collateEntry :: DataBaseEntry FilePath -> IO (DataBaseEntry PassageResults)
  collateEntry entry =
    do passagePdf <- readResultsFile (combine dir $ dbPassagePdf entry)
       passageCdf <- readResultsFile (combine dir $ dbPassageCdf entry)
       return $ entry { dbPassagePdf = passagePdf
                      , dbPassageCdf = passageCdf
                      }

  -- Read in a results file, essentially just read in the contents of the
  -- the file and then interpret that contents as a set of passage-time
  -- results.
  readResultsFile :: FilePath -> IO PassageResults
  readResultsFile =
    liftM interpretContents . readFile
  
  -- Interpreting the contents of a results file is simple
  -- each mapping is on a line of its own and the time is separated
  -- from the probability by a comma
  interpretContents :: String -> PassageResults
  interpretContents =
    (map interpretLine) . lines
    where
    interpretLine :: String -> (Double, Double)
    interpretLine inputline =
      (read time, read prob)
      where
      (time, commaProb) = span (/= ',') inputline
      prob              = dropWhile (== ',') commaProb


{-
  Returns for the given entry the probability at the given time.
  Is unforgiving and will error should we attempt to lookup a time
  which is not defined.
-}
getCdfProbabilityAtTime :: Time -> DataBaseEntry PassageResults -> Probability
getCdfProbabilityAtTime time entry =
  case lookup time $ dbPassageCdf entry of
    Just t  -> t
    Nothing -> error $ "entry doesn't have time : " ++ (show time)


{-|
  Prints a three dimensional graph in a format suitable for gnuplot.
-}
printThreeDGraph :: ThreeDGraph -> String
printThreeDGraph results = 
  -- This puts a new line character after the last line
  -- of the set, which already has a newline at the end of
  -- thus creating the blank-line gnuplot requires to separate
  -- the sets.
  unlines sLines
  where
  -- sLines represents the strings representing one set.
  -- of coures each 'line' is a string which will contain
  -- new-line characters so 'line' is a bit of misnomer.
  sLines :: [ String ]
  sLines = map printSet rLines

  -- So each set in a threeD graph has a single value
  -- which all the members of the set share, we represent
  -- this in a threeD graph structure appropriately, but
  -- for printing it out in the format that gnuplot likes
  -- we need to distribute the 'common-set-value' amoung
  -- all the members of that set.
  rLines :: [ [ (Double, Double, Double) ] ]
  rLines = map distributeRate results

  distributeRate :: (Double, [ (Double, Double) ] ) 
                 -> [ (Double, Double, Double) ]
  distributeRate (d, pairs) = map (mkTriple d) pairs

  mkTriple :: Double -> (Double, Double) -> (Double, Double, Double)
  mkTriple a (b, c) = (a, b, c)

  -- printing a set is simple we print each line in the set
  -- and separate them with newlines.
  printSet :: [(Double, Double, Double)] -> String
  printSet = unlines . (map printLine)

  -- printing a line is simple as pie
  printLine :: (Double, Double, Double) -> String
  printLine (a, b, c) = unwords $ map show [ a, b, c ]


{-|
  All the sensitivity graphs from one group of data base entries.
  Recall that each data base group corresponds to one combination
  of process instantiations.
  The point here is that there is a potentially large number of
  sensitivity graphs. A sensitivity graph shows how varying one
  rate affects the results for one particular model.
  For each variable rate we need a sensitivity graph for each of
  the combinations of the other variable rates. Suppose we have
  three rates, r1, r2 and r3 all of which can be 1.0 or 2.0.
  For r1 we will have FOUR sensitivity graphs and similarly for
  r2 and r3. The four sensitivity graphs for r1 are when:
  @
  r2 = 1.0 and r3 = 1.0
  r2 = 1.0 and r3 = 2.0
  r2 = 2.0 and r3 = 1.0
  r2 = 2.0 and r3 = 2.0.
  @
  We must do all of these since it could be that varying r1 has a
  large effect on the model when say r2 and r3 at at 2.0, but almost
  no effect when r2 and r3 are both 1.0.
-}
groupSensitivityGraphs :: DataBaseGroup PassageResults 
                       -> SensitivityGroup SensitivityGraph
groupSensitivityGraphs dbgroup =
  -- For now we just make one sensitivity graph per rate
  SensitivityGroup { sgroupGraphs    = graphs
                   , sgroupProcesses = dbgProcesses dbgroup
                   }
  where
  -- For each rate, we wish to make a set of filters which is the
  -- combinations of the other rate values. For each filter we make
  -- a sensitivity graph.
  graphs        = concatMap senGraphsForOneRate allRateArrays
  allRateArrays = dbgRateArrays dbgroup
  
  -- The entries in the data base group, so we are only dealing with
  -- entries involving a single pepa model (but obviously with different
  -- rate parameter settings).
  entries       = dbgEntries dbgroup

  senGraphsForOneRate :: RateValueArray -> [ SensitivityGraph ]
  senGraphsForOneRate (rName, _) =
    map mkSensitivity $ permuteFilters otherRates
    where
    otherRates = filter notR allRateArrays
    notR :: RateValueArray -> Bool
    notR = not . (rName ==) . fst

    permuteFilters :: [ RateValueArray ] -> [ RateFilter ]
    permuteFilters []                   = [[]]
    permuteFilters ((r, values) : rest) =
      -- So we should have m * n filters where m is the number
      -- of values in the current (or head) rate value array
      -- and n is the number of filters got by permuting the
      -- rest of the list.
      concatMap addRest current
      where
      -- If there were only one rate value array then the filters
      -- would be that rate mapped to each of the different values
      -- in that array.
      current     = zip (repeat r) values
      -- If it's not the only rate value array however then we must
      -- permute all the other rate value arrays. This will give us
      -- a list of filters which do *not* mention the current rate
      -- name. So for each value that the current rate name can take
      -- we make a list of filters which consists of the filters from
      -- the rest of the rate value arrays plus one possible setting
      -- for the current rate array.
      -- This means we essentially copy the list of filters got by
      -- permuting the rest n times, where n is the number of values
      -- the current one can take. Having copied it n times we add
      -- a single mapping of the current rate.
      restFilters = permuteFilters rest
      addRest :: RateValue -> [ RateFilter ]
      addRest currentValue = map (currentValue :) restFilters
      
    -- Once we have sets of rate filters we create a sensitivity graph
    -- for each rate filter. This function takes a single rate filter
    -- and produces the sensitivity graph based on it.
    mkSensitivity :: RateFilter -> SensitivityGraph
    mkSensitivity rFilter = 
      SensitivityGraph { sgName       = graphName
                       , sgDataCdf    = threeDGraphCdf
                       , sgDataPdf    = threeDGraphPdf
                       , sgVariedRate = rName
                       , sgRateValues = rFilter
                       , sgEntries    = orderedGroups
                       }
      where
      -- The name of the graph is essentially the name of the rate
      -- this will be qualified later. However the problem with this
      -- is that the rate name will likely contain '::' because it is
      -- within a namespace. Hence we convert those to underscores.
      -- It *should* be that a 'RateFilter' has a 'QualifiedName' and not
      -- a string, but we'll live with it this way for now.
      graphName       = map convertColon rName
      convertColon :: Char -> Char
      convertColon ':' = '_'
      convertColon c   = c

      -- The actual sensitivity graph information
      threeDGraphCdf  = map getCdfGroup orderedGroups
      threeDGraphPdf  = map getPdfGroup orderedGroups
      orderedGroups   = sortBy orderGroup $ map mkGroup filteredEntries
      filteredEntries = filter conformsToFilter entries

      -- Determines whether a given entry conforms to the current
      -- rate filter.
      conformsToFilter :: DataBaseEntry PassageResults -> Bool
      conformsToFilter entry =
        all filterOkay rFilter
        where
        filterOkay :: RateValue -> Bool
        filterOkay (r2, value) =
          case lookup r2 $ dbRates entry of
            Just a  -> a == value
            Nothing -> error "filtered rate value not present in entry"

      -- Maps each entry which we know applies to the filter the value
      -- that the varied rate
      -- (that is the rate we are making the sensitivity graph for)
      -- takes on for that entry, we can then sort these entries
      mkGroup :: DataBaseEntry PassageResults 
              -> (Double, DataBaseEntry PassageResults )
      mkGroup entry =
        case lookup rName $ dbRates entry of
          Just a  -> (a, entry)
          Nothing -> error "value not present but should be"

      -- From the entry we obtain the pdf of the passage-time results
      -- this will make up one line in the threeD graph.
      getPdfGroup :: (Double,  DataBaseEntry PassageResults) 
                  -> (Double, TwoDGraph)
      getPdfGroup = second dbPassagePdf 
      
      -- And we do the same for the cdf so we now have a sensitivity
      -- graph for both the cdf and the pdf.
      getCdfGroup :: (Double,  DataBaseEntry PassageResults) 
                  -> (Double, TwoDGraph)
      getCdfGroup = second dbPassageCdf 


      -- Ordering a group consists of simply ordering between the head
      -- value of the group.
      orderGroup :: (Double, DataBaseEntry PassageResults)
                 -> (Double, DataBaseEntry PassageResults)
                 -> Ordering
      orderGroup (a,_) (b,_) = compare a b




{-|
  Create a candle stick graph from the given data base and parameters.
  A candle stick graph uses all the entries within a database.
-}
createCandleStick :: String                  -- ^ A prefix for the name
                  -> [ Double ]              -- ^ The time values
                  -> Int                     -- ^ The low percentage
                  -> Int                     -- ^ The high percentage
                  -> DataBase PassageResults -- ^ The data base
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
  entries         = databaseEntries database

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
    probabilities = map (getCdfProbabilityAtTime time) entries
    bottom        = head sortedProbs
    top           = last sortedProbs

    -- This arithmetic is not particularly accurate!!
    opening       = sortedProbs !! (div (size * lowP) 100)
    closing       = sortedProbs !! (div (size * highP) 100)
    median        = sortedProbs !! (div (size * 50) 100)

    size          = length sortedProbs
    sortedProbs   = sort probabilities


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


{-|
  Obtaining a time graph from a data base. This is similar to obtaining
  a candle stick graph. We use every entry in the database.
  One point to note is that we simply number the data base results
  more or less arbitrarily.
-}
getTimeGraphData :: String -> DataBase PassageResults -> Time -> TimeGraph
getTimeGraphData prefix database time =
  TimeGraph { timegraphName = name
            , timegraphTime = time
            , timegraphData = numberedEntries
            }
  where
  name            = concat [ prefix, "--at--time--", show time, ".dat" ]
  numberedEntries = zip [0..] probabilities
  probabilities   = map (getCdfProbabilityAtTime time) entries
  entries         = databaseEntries database

      

