{-|
  A module for drawing graphs of the results.
-}
module Ipc.DrawGraph
  ( Graph              ( .. )
  , GraphOption        ( .. )
  , GraphData          ( .. )
  , SurfaceGraph       ( .. )
  , Surface            ( .. )
  , SimpleLine         ( .. )
  , GraphOutput        ( .. )
  , makeSimpleGraph
  , plotSimpleGraph
  , plotSimpleGraphPNG
  )
where

{- Standard Library Modules Imported -}
import Control.Monad
  ( liftM )
import Data.List
  ( intercalate )
import qualified Data.Maybe as Maybe
import System.Cmd
  ( system )
import qualified System.FilePath as File
{- External Library Modules Imported -}
import qualified Graphics.Gnuplot.Simple as Gnuplot
import qualified Graphics.Gnuplot.Terminal.PostScript as Postscript
{- Local Modules Imported -}
{- End of Module Imports -}

data Graph = 
    Graph { graphTitle   :: Title        -- ^ The title of the graph
          , graphData    :: GraphData    -- ^ The data of the graph
          , graphOptions :: GraphOptions -- ^ Options for the graph
          }
          deriving (Show, Read)

type GraphOptions = [ GraphOption ]
data GraphOption = Xlogscale
                 | Ylogscale
                 | Zlogscale
                 | Custom String [ String ]
                 deriving (Show, Read, Eq)                

data GraphData = 
     LineGraph [ SimpleLine ]
   | SGraph  SurfaceGraph
   deriving (Show, Read)

data SurfaceGraph =
     SurfaceGraph { sgraphSurfaces :: [ Surface ]
                  , sgraphView     :: Maybe (Int, Int, Int, Int)
                  }               
   deriving (Show, Read)

{-|
  Output format of the graph to draw. In one case no graph is actually draw, CSV
  this outputs the graph information as a comma-seperated-value file.
-}
data GraphOutput = Window | PNG | PS | PDF | SVG | CSV
                   deriving (Show, Eq, Read)

type Title = Maybe String

data SimpleLine =
  SimpleLine { simpleLinePoints :: [ (Double, Double) ]
             , simpleLineTitle  :: Title
             , simpleLineWidth  :: Double
             }
             deriving (Show, Read)

data Surface =
     Surface { surfacePoints :: [ [ (Double, Double, Double) ] ]
             , surfaceTitle  :: Title  
             } 
             deriving (Show, Read)
   

makeSimpleGraph :: Title -> GraphOptions -> [ SimpleLine ] -> Graph
makeSimpleGraph title options glines =
  Graph { graphTitle   = title
        , graphData    = LineGraph glines
        , graphOptions = options
        }

csvFromGraph :: Graph -> String
csvFromGraph graph =
  case graphData graph of
    LineGraph glines -> csvFromSimpleLines glines
    SGraph sgraph    -> csvFromSurfaces $ sgraphSurfaces sgraph

csvFromSimpleLines :: [ SimpleLine ] -> String
csvFromSimpleLines sLines =
 unlines $ headLine : (bodyLines $ map simpleLinePoints sLines)
 where
 -- This is a little simplistic and assumes that they have all been added
 -- in the correct order and all with the same values.
 headLine  = intercalate ", " ("# number" :  map getTitle sLines)
 getTitle  = Maybe.fromMaybe "''" . simpleLineTitle 

 bodyLines :: [ [ (Double, Double) ] ] -> [ String ]
 bodyLines [] = []
 bodyLines l
   | any null l = []
   | otherwise  = 
     thisLine : rest
     where
     thisLine :: String
     thisLine = intercalate ", " ( (show . fst . head . head $ l)  :
                                   (map (show . snd . head) l))
     rest     = bodyLines $ map tail l

csvFromSurfaces :: [ Surface ] -> String
csvFromSurfaces surfaces =
  drawGroups groupData
  where
  groupData :: [ [ [ (Double, Double, Double) ] ] ]
  groupData = map surfacePoints surfaces

  drawGroups :: [ [ [ (Double, Double, Double) ] ] ] -> String
  drawGroups groups
    | any null groups = ""
    | otherwise       = 
      concat [ drawLines headGroups
             , "\n\n"
             , drawGroups $ map tail groups
             ]
    where
    -- these are the head groups of each surface, NOT the first
    -- surface.
    headGroups = map head groups
    drawLines :: [ [ (Double, Double, Double) ] ] -> String
    drawLines []              = ""
    drawLines tripleGroups
      | any null tripleGroups = ""
      | otherwise             = 
        concat [ thisLine
               , "\n"
               , drawLines $ map tail tripleGroups
               ]
        where
        thisLine         = intercalate ", " allValues
        allValues        = (show headValue) : otherValues
        headLines        = map head tripleGroups
        -- We assume that everything here has the same first
        -- value so we just take the first one.
        (headValue,_,_)  = head headLines
        otherValues      = concatMap take2and3 headLines
        take2and3 :: (Double, Double, Double) -> [ String ]
        take2and3 (_, b, c) = [show b, show c]

        

{-| Plot a graph as a png file -}
plotSimpleGraphPNG :: Graph -> FilePath -> IO ()
plotSimpleGraphPNG = plotSimpleGraph PNG

{- Plot a simple two-dimensional graph based on a list of simple lines -}
plotSimpleGraph :: GraphOutput     -- ^ The kind of output
                -> Graph           -- ^ The graph to plot
                -> FilePath        -- ^ The output filename, of course this
                                   --   is not used for Window
                -> IO ()
plotSimpleGraph CSV graph filepath     =
  writeFile filepath $ csvFromGraph graph
plotSimpleGraph PS  graph filepath     =
  plotSimpleGraphEps graph filepath
plotSimpleGraph PDF graph filepath     =
  do plotSimpleGraphEps graph epsFile
     system $ unwords [ "eps2pdf", "-f", epsFile ]
     return ()
  where
  -- We are just assuming that the file extension given is "pdf"
  epsFile = File.addExtension (File.dropExtension filepath) "eps"
plotSimpleGraph PNG graph filepath     =
  getPlottingFunction graph attributes 
  where
  attributes = (Gnuplot.PNG filepath) : (graphAttributes graph)
plotSimpleGraph SVG graph filepath     =
  do putStrLn "Scalable Vector Graphics output not yet supported:"
     putStrLn "outputting png instead"
     plotSimpleGraph PNG graph filepath
plotSimpleGraph Window graph filepath  =
  do putStrLn "Window graph display not yet supported:"
     putStrLn "outputting png instead"
     plotSimpleGraph PNG graph filepath


plotSimpleGraphEps :: Graph -> FilePath -> IO ()
plotSimpleGraphEps graph filepath =
  getPlottingFunction graph attributes
  where
  attributes = (Gnuplot.terminal term) : (graphAttributes graph)
  term       = Postscript.eps . Postscript.color . Postscript.cons $ filepath
-- plotSimpleGraph Window graph _filepath =
-- plotSimpleGraph SVG  graph filepath     =

getPlottingFunction :: Graph -> [ Gnuplot.Attribute ] -> IO ()
getPlottingFunction graph attributes =
  case graphData graph of
    LineGraph glines   ->
      Gnuplot.plotPathsStyle attributes $ map makeGnuLine glines
    SGraph sgraph      ->plotSurfaces (sgraphSurfaces sgraph)
                                      (sgraphView sgraph)
  where
  plotSurfaces :: [ Surface ] -> Maybe (Int, Int, Int, Int) -> IO ()
  plotSurfaces [ gsurf ] mView =
    Gnuplot.plotMesh3d allAttributes [] $ makeGnuSurface gsurf
    where
    allAttributes = attributes ++ (viewAttributes mView)
    viewAttributes Nothing          = []
    viewAttributes (Just (a,b,c,d)) = 
      [ Gnuplot.Custom "view" [ viewArgs ] ]
      where
      viewArgs = intercalate "," [ show a
                                 , show b
                                 , show c
                                 , show d
                                 ]
  plotSurfaces _ _mView        = 
    error "Unimplemented multiple surface graphs"
                             

graphAttributes :: Graph -> [ Gnuplot.Attribute ]
graphAttributes graph
  | Just t <- graphTitle graph = (Gnuplot.Title t) : attributes
  | otherwise                  = attributes
  where
  options    = graphOptions graph
  attributes = map attributeOfOpt options

  attributeOfOpt :: GraphOption -> Gnuplot.Attribute
  attributeOfOpt (Xlogscale)  = Gnuplot.Custom "logscale" [ "x" ]
  attributeOfOpt (Ylogscale)  = Gnuplot.Custom "logscale" [ "y" ]
  attributeOfOpt (Zlogscale)  = Gnuplot.Custom "logscale" [ "z" ]
  attributeOfOpt (Custom s l) = Gnuplot.Custom s l



makeGnuLine :: SimpleLine -> (Gnuplot.PlotStyle, [ (Double, Double) ])
makeGnuLine sline =
  (plotStyle, simpleLinePoints sline)
  where
  -- gnuplot 0.2
  -- (Gnuplot.Lines, lineSpec)
  plotStyle = Gnuplot.PlotStyle { Gnuplot.plotType  = Gnuplot.Lines
                                , Gnuplot.lineSpec  = lineSpec
                                }
  --  title     = simpleLineTitle sline
  lineSpec  = Gnuplot.CustomStyle lineAttrs

  lineAttrs = Maybe.catMaybes 
               [ Just . Gnuplot.LineWidth $ simpleLineWidth sline
               ,  liftM Gnuplot.LineTitle $ simpleLineTitle sline
               ]

{-
   Clearly I'd prefer if this returned some style as well, but the
   current gnuplot library doesn't allows, that is there is no
   equivalent Gnuplot.plot3dStyle taking in: (PlotStyle, [[D,D,D]])   
-}
makeGnuSurface :: Surface -> [ [ (Double, Double, Double) ] ]
makeGnuSurface = surfacePoints

