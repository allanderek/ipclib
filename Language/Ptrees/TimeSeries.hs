module Language.Ptrees.TimeSeries
  ( makeAllTimeSeriesPdfs
  , createTransientTimeSeries
  )
  where

{- Standard Library Modules Imported -}
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import System.Exit
  ( ExitCode  ( .. ) )
{- External Library Modules Imported -}
{- Local Modules Imported -}
import qualified Language.Pepa.QualifiedName as Qualified
import qualified Language.Pepa.Syntax as Pepa
import qualified Language.Pepa.Compile.GenMatrix as GenMatrix
import qualified Language.Pepa.Compile.Uniformise as Uniformise
import Language.Pepa.Compile.MarkovChain
  ( Probability )
import qualified Ipc.DrawGraph as DrawGraph
import Ipc.DrawGraph
  ( SimpleLine   ( .. ) )
import qualified Language.Ptrees.Syntax as Ptrees
import Language.Ptrees.Syntax
  ( PlotOptions  ( .. ) )
import qualified Language.Ptrees.DataBase as DataBase
import Language.Ptrees.DataBase
  ( DataBase        ( .. )  )
{- End of Module Imports -}

type TimeSeriesDataBase = DataBase Ptrees.DataBaseTag Uniformise.TransientResult
-- type TSDataBaseEntry    = DataBaseEntry Ptrees.DataBaseTag 
--                                        Uniformise.TransientResult

makeAllTimeSeriesPdfs :: String -> TimeSeriesDataBase ->IO ExitCode
makeAllTimeSeriesPdfs prefix database =
  do mapM plotGraph $ zip [ 0.. ] graphs
     return ExitSuccess   
  where
  plotGraph :: (Int, DrawGraph.Graph) -> IO ()
  plotGraph (i, graph) = 
    DrawGraph.plotSimpleGraph DrawGraph.PS graph filename
    where
    filename = concat [ prefix, "-", show i, ".eps" ]

  graphs   = DataBase.retrieveResults database retrieve

  retrieve :: Uniformise.TransientResult -> DrawGraph.Graph
  retrieve tr = 
    DrawGraph.makeSimpleGraph Nothing options sLines
    where
    sLines   = createTransientTimeSeries Ptrees.defaultPlotOptions tr
    options  = [] -- Ptrees.plotOptionsToGraphOptions 

createTransientTimeSeries :: PlotOptions 
                          -> Uniformise.TransientResult 
                          -> [ SimpleLine ]
createTransientTimeSeries plotOptions transientResult =
  map createLine plottedNames
  where
  steadies     = Uniformise.transientDistributions transientResult
  -- allNames are all the process names mentioned in any one of
  -- the time probability distribution / populations
  allNames     = foldl List.union [] $ 
                 map (Map.keys . GenMatrix.srPopulations . snd) steadies
  -- The plotted names are those names we wish to plot in the time series
  -- this will be the same as 'allNames' unless a plot options specifies
  -- the names to plot. Note also that this means we can specify the order
  -- of the names to plot using the plot options (which shouldn't matter
  -- but for now we cannot change the colours hence this is one way to
  -- make sure they are the same over multiple plots)
  plottedNames = maybe allNames getRealNames $ 
                       plotOptsLineNames plotOptions
  -- Problem is that the names given in the plot options are not qualified
  -- names so we have to turn them into qualified names by looking them up
  -- in 'allNames' for one that has the same original name.
  getRealNames = Maybe.mapMaybe getRealName
  getRealName t = List.find (\a -> t == Qualified.showOrig a) allNames

  -- If the plot options gives a list of names to plot then
  -- we of course must honour that and we return 'Nothing' for
  -- any that aren't in the set of line names to be plotted.
  -- If there are no line name options then we just plot everything
  createLine :: Pepa.ParsedComponentId -> SimpleLine
  createLine name =
    line
    where
    origName = Qualified.showOrig name
    line     = SimpleLine { simpleLineTitle  = Just origName
                          , simpleLinePoints = map getTimePoint steadies
                          , simpleLineWidth  = lineWidth
                          }
    -- for each time/steadyreport we must get the probability
    -- of being in that state. Note that if it isn't mentioned
    -- then the probability is assumed to be zero
    getTimePoint :: (Uniformise.Time, GenMatrix.SteadyReport) 
                 -> (Uniformise.Time, Probability)
    getTimePoint (time, steady) =
      (time, Maybe.fromMaybe 0.0 mProbability)
      where
      mProbability = Map.lookup name $ GenMatrix.srPopulations steady
  lineWidth = 2.0