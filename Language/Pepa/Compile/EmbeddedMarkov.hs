module Language.Pepa.Compile.EmbeddedMarkov
  ( getEmbedded 
  , solveForEmbeddedSteadyState
  )
where

{- Standard Library Modules Imported -}
import qualified Data.Map as Map
{- External Library Modules Imported -}
{- Local Modules Imported -}
import qualified Language.Pepa.Compile.GenMatrix as GenMatrix
import Language.Pepa.Compile.GenMatrix
  ( GeneratorMatrix    ( .. )
  , SteadyState        ( .. )
  )
import qualified Data.Matrix as Matrix
import qualified Language.Pepa.Compile.MarkovChain as Markov
import Language.Pepa.Compile.MarkovChain
  ( MarkovChain
  , Probability 
  )
import qualified Language.Pepa.Compile.States as States
import Language.Pepa.Compile.States
  ( StateSpace         ( .. )
  , State
  )
import qualified Language.Pepa.MainControl as MainControl
import Language.Pepa.MainControl
  ( MainControl )
{- End of Module Imports -}

{-|
   The data type holding an embedded markov chain. Note that the actual
   matrix represented here will be  the EMC - I, or the embedded markov chain
   minus the identity matrix.
-} 
data EmbeddedMarkov =
  Embedded { embedStates     :: StateSpace
           , embedMatrix     :: Matrix.Matrix Markov.Probability
           , embedTransposed :: Bool
           }


{-|
   Compute the embedded markov chain from the generator matrix
   of the ctmc. Note that here we actually generate the
   embedded markov chain minus the identity matrix.
   This is merely because we will end up computing the EMC - I
   in all cases anyway and it's convenient to do so here.
   If we change our mind about this it shouldn't be too hard
   to create EMC - I. Alternatively whenever we need the actual
   emc we could just say @(getEmbedded genMatrix) + I@
   (obviously not actual code).
-}
getEmbedded :: GeneratorMatrix -> EmbeddedMarkov
getEmbedded gMatrix
  | gmTransposed gMatrix = transposedEmbed
  | otherwise            = nonTransposedEmbed
  where
  nonTransposedEmbed =  
    Embedded { embedStates     = gmStates gMatrix
             , embedMatrix     = findEmbedMatrix $ gmMatrix gMatrix
             , embedTransposed = False
             }
    where 
    -- We can use 'Matrix.mapMatrixRows' rather than 
    -- 'Matrix.mapMatrixRowsFull' because we are not changing the
    -- dimensions of the matrix and can assume all rows are represented
    -- because all of the diaganols must be represented.
    findEmbedMatrix :: MarkovChain -> MarkovChain
    findEmbedMatrix = Matrix.mapMatrixRowsWithI editRow

    editRow :: Int -> Markov.Row -> Markov.Row
    editRow i row = 
      -- Basically instead of computing the embedded markov chain we
      -- are computing the emc - I because we compute that in all
      -- cases anyway and it's convenient to do so here,
      -- see main function comment.
      -- We do this by just mapping the whole vector and then overriding
      -- the ii position.
      Matrix.vectorInsert i (-1.0) $ Matrix.mapVector normaliseCell row
      where
      totalOut  = (Matrix.vectorSum row) - aii
      aii       = Matrix.safeVectorIndex row i
      normaliseCell x = x / totalOut

  transposedEmbed = undefined

solveForEmbeddedSteadyState :: GeneratorMatrix -> MainControl SteadyState
solveForEmbeddedSteadyState = solveEmbedded . getEmbedded

solveEmbedded :: EmbeddedMarkov -> MainControl SteadyState
solveEmbedded embed =
  MainControl.valueResult steady "emc-solution" logInfo
  where
  steady    = SteadyState { steadyProbabilities = probabilities
                          , steadyProbsList     = steadyList
                          , steadyStateSpace    = states
                          }

  -- Solve the embedded markov chain using the 'Markov' module to
  -- do the actual grunt work of solving.
  emc           = embedMatrix embed
  solution      = Markov.solveForSteadyState (embedTransposed embed) emc
  states        = embedStates embed

  -- Well this is pretty questionable in fact I think simply wrong
  -- it must translate the indicies of the solution vector into
  -- state ids. So if we just say that this is from Ints which are
  -- yet to be converted to state ids then it is fine.
  probabilities = Map.mapKeys (States.indexStateId 1) $ 
                  Matrix.getMap solution

  steadyList    = map getState $ Map.elems tangIdMap
  tangIdMap     = spaceTangIds states

  getState :: State -> (Probability, State)
  getState state =
    ( Matrix.safeVectorIndex solution $ 
      States.stateIdIndex 1 $ States.stateNumber state
    , state
    )

  -- For the log info just show the embedded markov chain.
  -- We could also show the steady-state probabilities of the
  -- embedded markov chain.
  logInfo       = unlines [ "The embedded markov chain"
                          , show emc
                          -- , "The solution to the emc"
                          -- , show probabilities
                          , "The emc steady-state"
                          , GenMatrix.showSteadyResults steady
                          ]

