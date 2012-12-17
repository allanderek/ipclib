module Language.Pepa.Compile.MarkovChain
  ( MarkovChain
  , Row
  , Probability
  , ProbVector
  , indexProbVector
  , solveForSteadyState
  )
where

{- Standard Library Modules Imported -}
{- External Library Modules Imported -}
import qualified Data.Matrix as Matrix
import qualified Data.Matrix.Solve as Solve
{- Local Modules Imported -}
{- End of Module Imports -}


{-|
  The type of the representation of a Markov chain.
  Note that we do not specify whether this is a 
  continuous-time Markov Chain or a 
  discrete-time Markov Chain.
-}
type MarkovChain = Matrix.Matrix Probability

{-| A row within a Markov chain is just a vector of probabilities -}
type Row = Matrix.Vector Probability

{-| The type of a probability within a Markov Chain -}
type Probability = Double

{-| The type of a probability vector which is the solution to a Markov Chain -}
type ProbVector  = Matrix.Vector Probability

{-| indexing a probability vector. -}
indexProbVector :: ProbVector -> Int -> Probability
indexProbVector = Matrix.safeVectorIndex

{-|
  Solving the Markov chain for steady state. This function takes
  care of transposing the Markov chain, replacing the final row
  with a row full of ones (to ensure that the solution is
  a probability vector). 

  The first argument determines whether or not the input Markov
  chain's generator matrix has already been transposed. Since the
  transpose 

  TODO: Very good question here, why can't we just add the row
  of ones to the top, this would simplify the creation of
  both the qtn and the solVector. In other words why do we replace
  the LAST row and not the FIRST?
  UPDATE: Okay I've tried this and it seems to work, the old code I've left
  commented below because I'm obviously still not one hundred percent
  convinced by this. One thing I really need is a good test suite.
  I've now removed the commented code because I'm more convinced that it
  does indeed work. Obviously it can be retrieved via the darcs repository.

  TODO: There should be an argument to determine which kind of matrix
  solver to use.
-}
solveForSteadyState :: Bool               -- ^ Whether the input Markov chain
                                          --   generator matrix has already
                                          --   been transposed.
                    -> MarkovChain        -- ^ Markov chain to be solved
                    -> ProbVector         -- ^ Resulting probability vector
solveForSteadyState transposed markov =
  solution
  where
  -- First we transpose the matrix Q to obtain Q^{T}
  qt
    | transposed = markov
    | otherwise  = Matrix.transposeMatrix markov
  -- Then we make it into a probability matrix
  -- by replacing the final row with a row of
  -- ones, this ensures that the solution is a probability
  -- vector (ie the values sum to one).
  -- In fact we may replace the FIRST row, when using lists to build
  -- up this matrix this was definitely faster since replacing the
  -- first element of a list is faster than replacing the second element.
  -- However when using maps or arrays it may be worthwhile NOT replacing
  -- the first row, since the top row likely contains a few zeros which
  -- when added to other rows during echelon reduction won't change zeros
  -- in those rows, allowing the matrix to still be represented sparsely.
  -- However it needs some benchmarking to see which would be better,
  -- the key point is that the row we replace must be the same as the index
  -- of the one in the solution vector.
  indexOfOneVec = 1
  qtn           = Matrix.replaceRow indexOfOneVec oneVector qt
  -- oneVector : (Matrix.dropRows 1 qt)

  -- The one vector is just a vector full of ones of the correct length
  -- this seems a trifle risky. Okay if we do it with 'fromListWithSize'
  -- it's not so risky but also not as efficient. The problem with the
  -- other way was attempting to add two vectors with different zero elements
  -- together, that should be possible, but at the moment unimplemented in
  -- Data.Matrix
  oneVector     = Matrix.fromListWithSize 0.0 matrixSize $ 
                                          replicate matrixSize 1.0
  -- Matrix.empty 1.0 matrixSize
  -- 

  -- The solution vector is all zeros apart from the first column which
  -- is one. The zeros represent that the flow into each state must equal
  -- the flow out of that state. The one represents that the probability
  -- of being in each state must sum to one. We must be in exactly one of
  -- of the states in question. The one is in the first column because we
  -- replaced with first row with the oneVector.
  solVector     = Matrix.vectorInsert indexOfOneVec 1.0 zeroVector
  zeroVector    = Matrix.empty 0.0 matrixSize
  -- Building up the solVector with a list, inefficient!
  -- Matrix.fromListWithSize solutionList matrixSize 
  --- solutionList  = 1.0 : (replicate (matrixSize - 1) 0.0)


  -- The size of the matrix is just the number of row
  -- (which should equal the number of columns). We might check for this?
  matrixSize    = Matrix.numberOfRows markov

  -- So then the solution is simply the result of solving the system of
  -- linear equations which we have made for ourselves.
  solution      = Solve.guassianElimination qtn solVector
