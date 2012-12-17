module Language.Ptrees.Against
  ( AgainstDataBase
  , makeAgainstPdf
  , makeAgainstSurfacePdf
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
import qualified Language.Ptrees.Syntax as Ptrees
import Language.Ptrees.Syntax
  ( PlotOptions     ( .. ) )
import qualified Language.Ptrees.DataBase as DataBase
import Language.Ptrees.DataBase
  ( DataBase ) 
import qualified Ipc.DrawGraph as DrawGraph
import Ipc.DrawGraph
  ( SimpleLine   ( .. )
  , Graph        ( .. )
  , GraphData    ( .. )
  , SurfaceGraph ( .. )
  , Surface      ( .. )
  )
{- End of Module Imports -}


type AgainstDataBase = DataBase Ptrees.DataBaseTag Double

makeAgainstPdf :: PlotOptions -> String -> String 
               -> AgainstDataBase -> IO ExitCode
makeAgainstPdf plotOpts prefix againstTag database =
  do DrawGraph.plotSimpleGraph DrawGraph.PDF graph filename
     return ExitSuccess   
  where
  graph    = DrawGraph.makeSimpleGraph Nothing options graphLines
 
  options  = Ptrees.plotOptionsToGraphOptions plotOpts

  filename = concat [ prefix, "-", againstTag, ".pdf" ]
  graphLines = [ SimpleLine { simpleLineTitle  = Nothing
                            , simpleLinePoints = mapping
                            , simpleLineWidth  = 2.0
                            }
               ]                          

  mapping = Maybe.catMaybes $ DataBase.retrieveWithKey database getValues
  getValues :: Ptrees.DataBaseTag -> Double -> Maybe (Double, Double)
  getValues tag v =
    do s <- Map.lookup againstTag tag 
       return (read s, v)




makeAgainstSurfacePdf :: PlotOptions     -- ^ The plotting options
                      -> String          -- ^ The prefix of the file
                      -> String          -- ^ The first variable rate
                      -> String          -- ^ The second variable rate
                      -> AgainstDataBase -- ^ The data base
                      -> IO ExitCode
makeAgainstSurfacePdf plotOpts prefix firstTag secondTag database =
  do DrawGraph.plotSimpleGraph DrawGraph.PDF graph filename
     return ExitSuccess   
  where
  
  graph    = Graph { graphTitle   = Nothing
                   , graphData    = SGraph sgraph
                   , graphOptions = Ptrees.plotOptionsToGraphOptions plotOpts
                   }
  sgraph   = SurfaceGraph { sgraphSurfaces = graphSurfaces
                          , sgraphView     = plotOptsView plotOpts
                          }
  filename = concat [ prefix, "-", firstTag, "-", secondTag, ".pdf" ]
  graphSurfaces = [ Surface { surfacePoints = groups
                            , surfaceTitle  = Nothing
                            }
                  ]

  -- We must sort first since grouping only groups locally
  groups  = List.groupBy equalFirst sorted
  equalFirst (a,_, _) (b, _, _) = a == b
  sorted  = List.sortBy firstThenSecond mapping
 
  firstThenSecond (a, b, _) (c, d, _)
    | first == EQ = compare b d
    | otherwise   = first
    where first = compare a c
 
  mapping = Maybe.catMaybes $ DataBase.retrieveWithKey database getValues
  getValues :: Ptrees.DataBaseTag -> Double -> Maybe (Double, Double, Double)
  getValues tag v =
    do s <- Map.lookup firstTag tag 
       t <- Map.lookup secondTag tag
       return (read s, read t, v)