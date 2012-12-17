{-|
  A module for drawing graphs of the results.
-}
module Gipc.DrawGraph
  ( renderSimpleLineGraph )
where

{- Standard Library Modules Imported -}
import Control.Arrow
  ( second )
import qualified Data.Maybe as Maybe
{- External Library Modules Imported -}
import qualified Graphics.Rendering.Chart as Chart
import qualified Graphics.Rendering.Chart.Types as ChartTypes
import qualified Data.Colour.Names as ColourNames
import qualified Data.Colour.RGBSpace as RGBSpace
{- Local Modules Imported -}
import Ipc.DrawGraph
  ( Graph              ( .. )
  , GraphData          ( .. )
  , SimpleLine         ( .. )
  )
{- End of Module Imports -}

{-| From a simple graph produce a renderable which may be placed
    within a drawing surface for a gtk application.
-}
renderSimpleLineGraph :: Graph -> Chart.Renderable ()
renderSimpleLineGraph = Chart.toRenderable . layoutSimpleLineGraph

layoutSimpleLineGraph :: Graph -> Chart.Layout1 Double Double
layoutSimpleLineGraph graph =
  Chart.defaultLayout1 { Chart.layout1_title_           = title
                       -- , Chart.layout1_bottom_axis     = xaxes
                       -- , Chart.layout1_left_axis       = yaxes
                       -- , Chart.layout1_right_axis      = yaxes
                       , Chart.layout1_plots_           = plotLines
                       }
  where
  title        = Maybe.fromMaybe "" $ graphTitle graph
  slines       = 
    case graphData graph of
       LineGraph l    -> l
       SGraph       _ -> error "Surface graphs not yet supported"
  -- gridlessAxis = Chart.defaultAxis { Chart.axis_grid = [] }
  -- yaxis        = Chart.autoScaledAxis gridlessAxis
  -- yaxes        = Chart.linkedAxes yaxis

  -- xaxes        = Chart.linkedAxes xaxis
  -- xaxis        = Chart.autoScaledAxis gridlessAxis

  plotLines    = map (Left . plotSimpleLine) $ zip colours slines
  colours      = loopRepeat [ blue, green, red, yellow, magenta, cyan, black ]
  blue         = ColourNames.blue
  green        = ColourNames.green 
  red          = ColourNames.red
  yellow       = ColourNames.yellow
  magenta      = ColourNames.magenta
  cyan         = ColourNames.cyan
  black        = ColourNames.black

  loopRepeat :: [a] -> [a]
  loopRepeat l = l ++ (loopRepeat l)



-- plotSimpleLine :: (RGBSpace.Colour a, SimpleLine) -> Chart.Plot Double Double
plotSimpleLine (colour, sline) =
  Chart.toPlot plotLine
  where
  plotLine  = Chart.defaultPlotLines { Chart.plot_lines_style_  = lineStyle
                                     , Chart.plot_lines_values_ = points
                                     , Chart.plot_lines_title_  = title
                                     }
  title     = Maybe.fromMaybe "" $ simpleLineTitle sline
  points    = [ simpleLinePoints sline ]
              -- [ [ Chart.Point x y | (x,y) <- simpleLinePoints sline ] ]
  lineStyle = Chart.solidLine (simpleLineWidth sline) colour

