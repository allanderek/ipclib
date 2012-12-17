{-|
  This module contains the logic to uniformise a matrix
  and obtain passage-time results from a uniformised matrix.
-}
module Language.Pepa.Compile.Uniformise
  ( UniformMatrix
  , PassageResult
  , PassageEndResult
  , TransientResult             ( .. )
  , Time
  , pdfResult
  , cdfResult
  , uniformiseGenMatrix
  , getPassageTimes
  , computeTransientResults
  , getPassageEndTimes
  , pprintPassageResult
  , hprintPassageResult 
  , pprintPassageEndResult
  , hprintPassageEndResult
  , pprintTransientResult
  , hprintTransientResult
  , formatCsvPassageResultCdf 
  , formatCsvPassageResultPdf 

  )
where

{- Standard Library Modules Imported -}
import Control.Arrow
  ( second )
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Set
  ( Set )
import qualified Data.Set as Set
import qualified Text.PrettyPrint.HughesPJ as Pretty
import Text.PrettyPrint.HughesPJ
  ( Doc )
{- External Library Modules Imported -}
{- Local Modules Imported -}
import qualified Language.Pepa.Compile.States as States
import Language.Pepa.Compile.States
  ( StateId )
import qualified Language.Pepa.Compile.GenMatrix as GenMatrix
import Language.Pepa.Compile.GenMatrix
  ( GeneratorMatrix   ( .. )
  , SteadyState       ( .. )
  , SteadyReport
  , ProbabilityDistribution
  )
import qualified Data.Matrix as Matrix
import qualified Language.Pepa.Compile.MarkovChain as Markov
import Language.Pepa.Compile.MarkovChain
  ( MarkovChain
  , Probability
  , ProbVector
  )
import qualified Language.Pepa.MainControl as MainControl
import Language.Pepa.MainControl
  ( MainControl
  , valueResult
  )
import qualified Language.Pepa.Utils as Utils
{- End of Module Imports -}

-- | The type of a uniformised matrix
data UniformMatrix = 
  UniformMatrix { -- | The actual uniformised matrix
                  uniformMatrix :: MarkovChain
                  -- | The q value that was used to uniformise
                  -- the matrix.
                , uniformQValue :: Double
                }
               
-- The type of a probability vector which will be used to represent
-- a hop (or in other parlance a pi(n)).
type PiHop = Markov.ProbVector


-- Sometimes we wish to make sure for example that a probability is not
-- greater than one. However because of slight rounding errors and
-- representation errors we can be slightly less strict about this.
-- Hence we have a general error threshold which means we should be within
-- this threshold of being an error. So if we wish to do a sanity check
-- on a probability value then rather than:
-- @probValue > 1.0 = error ... @
-- we write
-- @probValue > (1.0 + errorThreshold) = error ... @
errorThreshold :: Double
errorThreshold = 0.0000000000001 

{-|
  Uniformising a generator matrix. If the generator matrix is Q, then
  we wish to compute:
    P = Q/q + I
  where q is some value such that it is larger than the magnitude of
  any of the values of Q along the diagonal.
  TODO: say more about why we might wish to do this.
-}
uniformiseGenMatrix :: GeneratorMatrix
                       -- ^ The generator matrix to uniformise
                    -> MainControl UniformMatrix
                       -- ^ The uniforimised matrix
uniformiseGenMatrix gMatrix =
  valueResult uniMatrix "uniformised-matrix" matrixLog 
  where
  uniMatrix = UniformMatrix { uniformMatrix = uMatrix
                            , uniformQValue = qValue
                            }

  uMatrix   = Matrix.addIdentity 1.0 $ Matrix.scalorDivide qValue genMatrix
  genMatrix = gmMatrix gMatrix

  -- Set the qValue higher than anything in the geneator matrix
  -- What we should really do is, when building the generator matrix
  -- from the state space we keep track of the largest rate value in
  -- the matrix and pass that value in to this function.
  -- We should even have this function taking in a 'Maybe Double' which
  -- is the largest value in the matrix such that we can use
  -- 'Matrix.maxAbsoluteValueInMatrix' if 'Nothing' is passed in.
  -- This just allows the function to be a bit more generic.
  -- At some  point we might like to release a "Markov Chain"
  -- library has a separate library to ipclib.
  qValue    = 1.1 * maxValue
  maxValue  = Matrix.maxAbsoluteValueInMatrix genMatrix

  -- Now the logging information.
  matrixLog = unlines [ "The uniformised matrix:"
                      , "q = " ++ (show qValue)
                      , show uMatrix
                      -- , unlines $ map (show . sum) uMatrix
                      ]


-- | The passage-result consists of both the pdf and the cdf of the
-- the passage in question.
data PassageResult = 
  PassageResult { passagePdf :: [ (Time, Probability) ]
                , passageCdf :: [ (Time, Probability) ]
                }
                deriving (Show, Read)

type Time        = Double



-- | For a passage-end query we essentially have a similar thing to a set
-- of passage-time results one for each kind of passage-completion.
type PassageEndResult = [ (String, PassageResult) ]




{-| 
  Returns the mapping from times to probabilities for the cdf
-}
cdfResult :: PassageResult -> [ ( Time, Probability ) ]
cdfResult = passageCdf


{-| 
  Returns the mapping from times to probabilities for the pdf
-}
pdfResult :: PassageResult -> [ ( Time, Probability ) ]
pdfResult = passagePdf


{-|
  Pretty print a passage result, again this will clearly change as the
  type above changes.
  We print both of the results out, it may seem wasteful but this is
  generally only used for --stdout which means the user is probably
  solving a pretty small model.
-}
pprintPassageResult :: PassageResult -> Doc
pprintPassageResult ptRes =
  Pretty.vcat [ pdfDoc, cdfDoc ]
  where
  pdfDoc   = Pretty.vcat (pdfHead : pdfLines)
  pdfHead  = Pretty.text "The probability density function results"
  pdfLines = map pprintLine $ passagePdf ptRes

  cdfDoc   = Pretty.vcat (cdfHead : cdfLines)
  cdfHead  = Pretty.text "The cummulative distribution function results"
  cdfLines = map pprintLine $ passageCdf ptRes

  pprintLine :: (Time, Probability) -> Doc
  pprintLine (time, prob) = 
    Pretty.hcat [ Utils.pprintDouble time
                , Pretty.comma
                , Utils.pprintDouble prob
                ]                  

{-|
  Print a passage result as a string using pprintPassageResult
-}
hprintPassageResult :: PassageResult -> String
hprintPassageResult = Pretty.render . pprintPassageResult


{-| Pretty print a passage end result -}
pprintPassageEndResult :: PassageEndResult -> Doc
pprintPassageEndResult per = 
  Pretty.vcat $ map pprintSingle per
  where
  pprintSingle :: (String, PassageResult) -> Doc
  pprintSingle (name, pt) = 
    Pretty.hcat [ Pretty.text name
                , pprintPassageResult pt
                ]

{-| Pretty print a passage-end result to a string -}
hprintPassageEndResult :: PassageEndResult -> String
hprintPassageEndResult = Pretty.render . pprintPassageEndResult


{-|
  Format the cummulative distribution function of a passage-time
  calculation into a string of the contents of a comma separated file.
-}
formatCsvPassageResultCdf :: PassageResult -> String
formatCsvPassageResultCdf =
  unlines . (map displayLine) . passageCdf
  where
  displayLine :: (Time, Probability) -> String
  displayLine (time, prob) = concat [ show time, ", ", show prob ]


{-|
  Format the probability density funcion of a passage-time
  calculation into a string of the contents of a comma separated file.
-}

formatCsvPassageResultPdf :: PassageResult -> String
formatCsvPassageResultPdf =
  unlines . (map displayLine) . passagePdf
  where
  displayLine :: (Time, Probability) -> String
  displayLine (time, prob) = concat [ show time, ", ", show prob ]


{-|
    The result we get from a transient calculation, this is basically
    a mixture of a steady-state result and a passage-time result.
    We basically calculate a 'steady-state' for each time, of course
    it's not really a *steady* state, but it contains the same information
    as a steady state, basically a probability distribution mapping states
    to their probabilities. There is no reason why we cannot turn this
    into a steady-report, even reporting the throughput, this will not
    quite be a throughput but a rate at that time of a given action.

    It's possible that we would be better saying (Time, SteadyState)
    here and then when printing out the transient result we compute
    the steady-result and use that. This will work if we ever want
    to do other things with transient results for which we do not need
    the steady-report.
-}
data TransientResult =
  TransientResult { transientDistributions :: [ ( Time, SteadyReport ) ]
                  }
                  deriving (Show, Read)

{-|
  Pretty printing a transient result
-}
pprintTransientResult :: TransientResult -> Doc
pprintTransientResult transient =
  Pretty.vcat $ map pprintSingleTime $ transientDistributions transient
  where
  pprintSingleTime :: (Time, SteadyReport) -> Doc
  pprintSingleTime (time, report) =
    Pretty.hcat [ Pretty.text "time:"
                , Pretty.vcat [ Utils.pprintDouble time
                              , GenMatrix.pprintSteadyStateReport False report
                              ]
                ]

{-| Pretty printing a transient result to a string -}
hprintTransientResult :: TransientResult -> String
hprintTransientResult = Pretty.render . pprintTransientResult


addAbsorbingState :: Set StateId   -- ^ The set of target states
                  -> UniformMatrix -- ^ The input uniformised matrix
                  -> (UniformMatrix, Int)  -- ^ The output uniformised matrix
                                           -- and the absorbing state's index
addAbsorbingState targets uniMatrix =
  (uniMatrix { uniformMatrix = newMatrix }, absorbingIndex)
  where
  currentSize    = Matrix.numberOfRows uMatrix
  -- The index at which we will add the absorbing row
  -- is the current number of rows plus one.
  absorbingIndex = 1 + currentSize
  -- The absorbing state is also the same as the new target states
  -- in that all of them with probability one go to the absorbing state.
  absorbing      = Matrix.vectorInsert absorbingIndex 1.0 $
                   Matrix.empty 0.0 absorbingIndex
  -- We can place the new absorbing state just at the end of the matrix
  -- absorbingState    = length uMatrix
  -- To get the new states we map the old states to the new states and
  -- add the new absorbing state.
  withAbsorbing = Matrix.appendRow absorbing uMatrix
  uMatrix       = uniformMatrix uniMatrix

  -- Finally then we must update each of the states in the target set.
  -- Each target state is replaced by a state which transitions to the
  -- absorbing state with probability one. This is just the same as
  -- the absorbing state itself.
  newMatrix     = Set.fold updateTargetState withAbsorbing targets
  updateTargetState :: StateId -> MarkovChain -> MarkovChain
  updateTargetState i =
    Matrix.insertRow (States.stateIdIndex 1 i) absorbing

{-|
  Return the passage time results from a list of source states to
  a list of target states. For now the result is just a list of matrices
  demonstrating the n/m hops of the uniform matrix

  The computation of a cummulative distribution function involves the
  computation of the following function:

  sum from n = 1 to infinity  of
    ( 1 - ((e ^ (-qt)) * sum from k=0 to n-1 of
                           ((qt)^ k) / k! )
    *
    sum of k in target state of
      pi(n) k

  In otherwords the top part of the equation multiplied by the
  probability of completing the passage in exactly 'n' hops.
-}
getPassageTimes :: [ Time ]
                   -- ^ The times to calculate for
                -> SteadyState
                   -- ^ The steady state distribution
                -> UniformMatrix
                   -- ^ The initial uniform matrix
                -> Set StateId
                   -- ^ The source state
                -> Set StateId
                   -- ^ The target states
                -> MainControl PassageResult
                   -- ^ The resulting values at the given times
getPassageTimes = computePassageTimeResults


{-|
  Return the passage time results from a list of source states to
  a list of target states. For now the result is just a list of matrices
  demonstrating the n/m hops of the uniform matrix

  The computation of a cummulative distribution function involves the
  computation of the following function:

  sum from n = 1 to infinity  of
    ( 1 - ((e ^ (-qt)) * sum from k=0 to n-1 of
                           ((qt)^ k) / k! )
    *
    sum of k in target state of
      pi(n) k

  In otherwords the top part of the equation multiplied by the
  probability of completing the passage in exactly 'n' hops.
-}
computePassageTimeResults :: [ Time ]
                             -- ^ The times to calculate for
                          -> SteadyState
                             -- ^ The steady state distribution
                          -> UniformMatrix
                             -- ^ The initial uniform matrix
                          -> Set StateId
                             -- ^ The source state
                          -> Set StateId
                             -- ^ The target states
                          -> MainControl PassageResult
                             -- ^ The resulting values at the given times
computePassageTimeResults times steadystate uniMatrix sources targets
  | Set.null sources = MainControl.resultError "No source states provided"
  | Set.null targets = MainControl.resultError "No target states provided"
  | otherwise        = 
  valueResult ptResults "uniform-hops" logInformation
  where
  ptResults = PassageResult { passagePdf = map selectPdf values
                            , passageCdf = map selectCdf values
                            }

  -- Note that in contrast to transient analysis performed below
  -- the 'mHops' are calculated over the modified uniformised-matrix.
  -- This is one which has had an absorbing state added such that we
  -- calculate only the first passage and not subsequent passages.
  mHops        = calculateNHopsUniformMatrix probDist absorbMatrix sources
  probDist     = Just $ steadyProbabilities steadystate
  absorbMatrix = fst absorbResult
  absorbIndex  = snd absorbResult
  absorbResult = addAbsorbingState targets uniMatrix 
  -- Get the value of 'q' that was used to uniformise the matrix
  qValue = uniformQValue uniMatrix


  selectPdf :: (Time, (Double, Double, Double)) -> (Time, Double)
  selectPdf = second Utils.snd3
  selectCdf :: (Time, (Double, Double, Double)) -> (Time, Double)
  selectCdf = second Utils.fst3

  values    = map getValuesAtTime times

  getValuesAtTime :: Time -> (Time, (Double, Double, Double))
  getValuesAtTime time =
    (time, theseValues)
    where
    theseValues  = calculateValueAtTime validHop
                                        getIntermediate 
                                        combineProbs
                                        qValue
                                        mHops
                                        time

    -- We want to take some range of hops, it would be nice to
    -- calculate until n=infinity but since that's not possible
    -- we calculate until n=X for some X such that the probability
    -- of being in the absorbing state is very close to one.
    validHop :: PiHop -> Bool
    validHop hop = (1 - (lastValueInHop hop)) > 0.0001

    -- Not too keen about the way in which I'm getting the 'absorbing'
    -- state here, it should probably be passed in as an argument?
    lastValueInHop :: PiHop -> Probability
    lastValueInHop hop = Matrix.safeVectorIndex hop absorbIndex
    
    getIntermediate :: Probability -> Probability -> Probability 
                    -> PiHop -> (Double, Double, Double)
    getIntermediate erlangCdf erlangPdf erlangNewPdf piHop =
      ( erlangCdf    * targetsProbability
      , erlangPdf    * targetsProbability
      , erlangNewPdf * targetsProbability
      )
      where
      -- This is the probability that in this hop we are in one
      -- of the target states.
      targetsProbability = 
        Utils.errorIf condition errorMsg sumValue
        where 
        condition = sumValue > (1.0 + errorThreshold)
        errorMsg  = error $ concat [ "sum probs greater than one: "
                                   , show sumValue ]
        sumValue  = sumMapSet getTargetProb targets      
        -- Note: it is not (i+1) here, so no Look, ma!, why? because
        -- the probability distribution is achieved from 'Matrix.getMap'
        getTargetProb :: StateId -> Probability
        getTargetProb = stateIdIndexProbVector piHop


    combineProbs :: [ (Double,Double,Double) ] -> (Double, Double, Double)
    combineProbs =
      foldl addValues (0.0, 0.0, 0.0)
      where
      addValues :: (Double, Double, Double) -> (Double, Double, Double)
                -> (Double, Double, Double)
      addValues (cdf1, pdf1, npdf1) (cdf2, pdf2, npdf2) =
        (cdf1 + cdf2, pdf1 + pdf2, npdf1 + npdf2)
                  
  -- The log information 
  -- (maybe this would be better in calculateValueAtTime)
  logInformation = unlines  [ sourceStatesLog
                            , targetStatesLog
                            , mHopsString
                            ]

  sourceStatesLog = "The source states were: " ++
                    (Utils.mkCSlist $ map show $ Set.toList sources)

  targetStatesLog = "The target states were: " ++
                    (Utils.mkCSlist $ map show $ Set.toList targets)

  -- A log string of some of the hops, this might be useful for 
  -- debugging, or also just for educational use on uniformisation.
  mHopsString = unlines $ map showHop $ take 20 mHops

  -- Show, for the purposes of logging, a probability vector.
  showHop :: PiHop -> String
  showHop row1 = unlines [ "Length of row is : " ++ 
                           (show $ Matrix.dimension row1)
                         , "Sum of row is : " ++ 
                           (show $ sum $ Matrix.toList row1)
                         , show row1
                         ]

{-|
  Perform transient analysis. In the future we should generalise this
  to go from a set of source states, using the steady-state of the
  embedded markov-chain to weight them just as we do for passage-time
  analysis. Of course sometimes with passage-time analysis the model
  deadlocks in which case we need to make sure that if there is only
  one source state we do not need to look at the steady-state
  (since there won't be one).
-}
computeTransientResults :: States.StateSpace
                           -- ^ The original state space
                        -> [ Time ]
                           -- ^ The times to calculate for
                        -> UniformMatrix
                           -- ^ The uniformised matrix
                        -> StateId
                           -- ^ The source state.
                        -> MainControl TransientResult
computeTransientResults states times uniMatrix source =
  return TransientResult { transientDistributions = distributions }
  where
  distributions = map getDistributionAtTime times

  mHops         = calculateNHopsUniformMatrix Nothing uniMatrix sources
  sources       = Set.singleton source
  -- Get the value of 'q' that was used to uniformise the matrix
  qValue        = uniformQValue uniMatrix
  hopSize       = Matrix.numberOfRows $ uniformMatrix uniMatrix

  getDistributionAtTime :: Time -> (Time, SteadyReport)
  getDistributionAtTime time =
    (time, steadyReport)
    where
    steadyReport = GenMatrix.getSteadyStateReport steadystate

    steadystate  = calculateValueAtTime alwaysTrue 
                                        getIntermediate 
                                        combineProbs
                                        qValue
                                        mHops
                                        time
    alwaysTrue :: a -> Bool
    alwaysTrue = const True
    
    getIntermediate :: Probability -> Probability -> Probability 
                    -> PiHop -> Markov.ProbVector
    getIntermediate _erlangCdf _erlangPdf = 
      -- We ignore the erlangCdf and the erlangPdf and use the
      -- erlangNewPdf which we use to scale the piHop vector so
      -- in the end this is just a scaleVector operation.
      Matrix.scaleVector {- erlangNewPdf piHop -}


    -- Each probability vector we have computed represents the probability
    -- that we have completed exactly that number of hops in the given time
    -- multiplied by the probability vector of states after that many of
    -- hops. So we add up all of these to get a probabliity vector of states
    -- at the given time.
    combineProbs :: [ Markov.ProbVector ] -> GenMatrix.SteadyState
    combineProbs allProbVectors = 
       GenMatrix.createSteadyState states solution
       where
       solution :: Markov.ProbVector
       solution = foldr Matrix.vectorAdd emptyProbVector allProbVectors
       emptyProbVector = Matrix.empty 0.0 hopSize

{-
  Really need to comment this function.
-}
calculateValueAtTime :: forall a b .
                        (PiHop -> Bool)   -- ^ Function to tell whether all
                                          -- ^ subsequent hops may be ignored
                     -> (Probability ->
                         Probability ->
                         Probability ->
                         PiHop -> a)      -- ^ Function to turn a single hop
                                          --   into part of the result.
                                          --   The two probability arguments
                                          --   are the cdf and the pdf of the
                                          --   erlang, since we need to compute
                                          --   the former anyway to decide
                                          --   whether to continue calculating
                                          --   more hop values.
                     -> ([a] -> b)
                     -> Double            -- ^ The value that was used to 
                                          --   uniformise the generator matrix
                                          --   which was then used to obtain the
                                          --   hops.
                     -> [ PiHop ]         -- ^ The (infinite) list of hops
                     -> Time              -- ^ The time at which to calculate
                     -> b                 -- ^ The final answer, the value at
                                          --   the given time.
calculateValueAtTime isValidHop toIntermediate 
                     joinIntermediates qValue hops time =
  joinIntermediates intermediateValues
  where
  intermediateValues = convertHops 0 hops

  -- Once the probability of performing a given number of hops within
  -- the given time shrinks to zero we can stop computing hop/intermediate
  -- values since at this point there is no chance to get that far along
  -- within the given time. This is the value we compare the cdf of the
  -- erlang to.
  erlangThreshold  = 0.000001

  -- The Int argument 'n' is the number of the hop.
  convertHops :: Int -> [ PiHop ] -> [a]
  convertHops _ []                  = []
  convertHops n (hop : otherHops)
    | not $ isValidHop hop          = []
    | n == 0                        =
      -- Why is the poisson e^{-qt} ?
      -- It should be e^{-qt}((qt)^n  / n!) but since n = 0 then
      -- (qt)^n = 1, and n! = 1, so ((qt)^n / n!) = 1
      (toIntermediate 1.0 qValue e_minusqt hop) : remainingHops
    | erlangCdf < erlangThreshold   =
      -- The erlangCdf is below the given threshold then this means
      -- that there is essentially no chance of completing 'n' hops
      -- within the given time. So we can return the empty list.
      -- Instead though we return the value for this hop, the reason
      -- is that if time = 0.0, then erlangCdf == 0, and hence obviously
      -- less than the threshold, but we do want at least one hop value.
      [ thisHopValue ]
    | otherwise                     = 
      -- If we have a real hop then convert the hop to an intermediate
      -- value and recursively compute other intermediate values.
      thisHopValue : remainingHops
    where
    -- Convert this hop to an immediate value
    thisHopValue  = toIntermediate erlangCdf erlangPdf poisson hop
    remainingHops = convertHops (n+1) otherHops

    -- This is the probability of performing 'n' hops within time t
    erlangCdf
      -- If this is zero it doesn't matter what the qsum is so
      -- let's not bother computing it.
      | e_minusqt == 0            = 1
      -- Okay at least for now, if e_minusqt is within some threshold
      -- of zero then we don't bother computing either.
      | e_minusqt < zeroThreshold = 1
      | otherwise                 = 1 - (sum e_minusqtQsumTerms) 
      -- Try to compute it the fast way, if this fails because qSum
      -- overflows then we can distribute e_minusqt throughout
      -- the qSum terms.
      --  | isInfinite qSum = 1 - (sum e_minusqtQsumTerms)
      --  Otherwise we are just left to do the usual as it is written.
      --  | otherwise       = (1 - (e_minusqt * qSum))

    -- erlangPdf is the probability of performing 'n' hops in exactly
    -- the given time. We don't actually need this ourselves, but since
    -- it has some values in common with the computation of erlangCdf it's
    -- easiest for us to just compute this here and pass it on to the
    -- 'toIntermediate' function which may well ignore it.
    -- This is (q^n * t^{n-1} * e^{-qT} )
    --         --------------------------
    --            (n - 1)!
    -- We factorise this a bit to get
    -- q * e^{-qT} * (qt/1 * qt/2 * .... * qt/(n-1))

    erlangPdf       = outValue * (product $ take (n-1) qTOverKTerms)
    outValue
      -- Small shortcut in the case that the time is 0.0, here we know
      -- that -qt is zero and x^0 = 1 hence e^{-qt} = 1
      | time == 0 = qValue
      | otherwise = qValue * e_minusqt 

    -- e^{-qt}
    e_minusqt = exp (- qT)
    qT        = qValue * time

    -- The poisson is the probability that at time 't' we have
    -- completed exactly 'n' hops. This is equal to the the
    -- erlangCdf(n) - erlangeCdf(n+1), because that is
    -- the probability that we have completed (at least) n hops within time t
    -- minus the probability that we have completed more than n hops
    -- within time t
    -- This works out as e^{-qt} * ((qt)^n / n!)
    poisson       = makeQSumTerm n * e_minusqt -- erlangDifference

    -- A zero threshold, when perform a test, x < zeroThreshold we really
    -- mean x == 0.0, however we allow it to be quite close to zero because
    -- we know that the arithmetic performed is not perfect.
    zeroThreshold = 1.0e-300

    -- If k is large, then the the qSum may go to infinite
    -- we can avoid this by first multiplying all the qsum terms
    -- by the e_minusqt value. So in effect we will be calculating
    -- (e_minusqt * qSum) by doing 
    -- (sum k = 0 to (n-1) of e_minusqt * ((qt)^k) / k!
    e_minusqtQsumTerms = map (e_minusqt *) sigQsumTerms

    -- A chance for some memoisation here in the k factorials
    -- and also the qt to the power k.
    -- This is the:
    -- sum from k=0 to n-1 of ((qt)^ k) / k! )
    -- Part of the equation.
    -- qSum
    --   | any (< 0) qSumTerms = error qSumError
    --   | otherwise           = sum sigQsumTerms


    -- An error msg in the case that any of the qsum terms is negative
    -- qSumError = unlines $ ( map show qSumTerms ) ++ 
    --             [ "One of the qSum terms is less than zero" ]

    -- For very large k, k! obliterates (qt)^k so we get very tiny
    -- qSumTerms which are in fact equal to zero, so we try to speed
    -- things up a bit by taking only those that are above zero.
    sigQsumTerms = qSumTerms -- takeWhile (> 0.0) qSumTerms
  
    -- Each of the qSum terms is ((qt)^k) / k! for k = 0 to (n-1)
    -- We perform from k = 0 to n-2, because we wish to compute the
    -- erlang of (n-1) and then take away from it the erlang of (n)
    -- to get the probability of completing EXACTLY n hops by time t
    qSumTerms    = map makeQSumTerm [ 0 .. (n - 1) ]

    -- However to compute this we compute
    -- (qt/1) * (qt/2) * (qt/3) ... * (qt/(k)
    -- But doesn't require us to compute to (potentially) very large values
    -- namely (qt^k) and k!.
    -- In fact we already have computed: 1.0, (qT/1), (q/2), (q/3) ...
    -- So we just take the first k values of that, and multiply each
    -- by 't'.
    makeQSumTerm :: Int -> Double
    makeQSumTerm 0 = 1.0
    makeQSumTerm k = product $ take k qTOverKTerms
    -- We are trying to compute ((qt)^k/k!) in order to avoid computing
    -- very large number we do qt / 1 * qt / 2 * qt / 3 * .. * qt/n
    qTOverKTerms = map (qT /) [1.0 ..]

  -- Must calculate the qSum for each value of t and for each hop n
  -- the qSum is sum of k=0 to (n-1) of (qt^k)/k!
  -- when the number of hops becomes large the value of (qt^k) when
  -- can become very large (and indeed so can k!).
  -- So we factor this out to become:
  -- ((qt)/1) * (qt/2) * ... (qt/k)
  -- Now for the sake of speed we can factor out the divisions further
  -- since q does not change for any value of t. So we can create the
  -- infinite list of
  -- (q/1), (q/2), (q/3) ...
  -- and then for each value of t, the infinite list of
  -- t(q/1), t(q/2), ....
  -- And then for each value of k, we just take
  -- product of (take the first k values of the above list)
  -- 
  -- This is broken for the time being as it seems to give incorrect
  -- results.
  -- qOverKTerms :: [ Double ]
  -- qOverKTerms = (map (qValue /) [1.0 ..])


{-
  NOTE: this returns the list of hops starting with (pi(0)).
  In general, well in particular for calculating a pdf or cdf function
  you will need the hops from pi(1) onwards (up to some limit).

  Additionally this calculates the infinite list of hops so if you actually
  want to use this you will have to in some way inspect only a finite
  prefix of this list.
-}
calculateNHopsUniformMatrix :: Maybe ProbabilityDistribution
                               -- ^ The steady state distribution
                            -> UniformMatrix
                               -- ^ The initial uniform matrix
                            -> Set StateId
                               -- ^ The source states
                            -> [ PiHop ]
                               -- ^ The resulting list of hops
calculateNHopsUniformMatrix probDistribution uniMatrix sources =
  getHops piZero
  where
  -- The first hop, or actually the hop before the first hop.
  -- This is the original probability distribution on states
  -- at the start of the passage of interest. Essentially it
  -- is the normalised steady-state probabilities of being
  -- in the source states.
  piZero :: PiHop
  piZero
    | (Set.size sources) == 1 = singleSourcePiZero
    | otherwise               = manySourcePiZero 

 
  -- This is the pi(0) used in the case that there is only one source
  -- state. In this case pi(0) is just the zero vector with a 1 in 
  -- the single source state. The number of states can be gotten from
  -- number of rows in the uniform matrix.
  -- Note that it is one row long.  
  singleSourcePiZero = Matrix.vectorInsert (States.stateIdIndex 1 source) 
                                           1.0 emptyVector
                       where
                       -- We can use 'Set.findMin' since we know 
                       -- the set has only one element
                       source = Set.findMin sources

  emptyVector  = Matrix.empty zeroEntry hopSize
  hopSize      = Matrix.numberOfRows $ uniformMatrix uniMatrix
  zeroEntry    = 0.0


  -- If there are multiple sources then we will need the steady
  -- state distribution (of the EMBEDDED markov chain)
  -- So we add each member of the set of sources to the pizero
  -- of course we have to calculate their sum total first.
  manySourcePiZero  = Set.fold addSource emptyVector sources
    where
    addSource :: StateId -> Markov.ProbVector -> Markov.ProbVector
    addSource i = Matrix.vectorInsert (States.stateIdIndex 1 i)$ 
                  getProbabilitySourceState i
  
    getSteadyProbOfState :: StateId -> Probability
    getSteadyProbOfState i = 
      Utils.maybeError "state not found in steady probability distribution" $
                       Map.lookup i steadyProbDist

    steadyProbDist = Maybe.fromMaybe (error errMsg) probDistribution
    errMsg         = unlines [ "Fatal Error: If we haven't given a"
                             , "probability distribution, then we"
                             , "shouldn't have more than one source state."
                             ]

    sumProbabilitySourceStates  = sumMapSet getSteadyProbOfState sources

    getProbabilitySourceState :: StateId -> Probability 
    getProbabilitySourceState i = 
      (getSteadyProbOfState i) / sumProbabilitySourceStates

  -- Each hop is calculated by multiplying the current hop by the
  -- uniform matrix.
  -- Note that this will return them in reverse order (which is generally
  -- what we will want since we will want the nth one and not the first
  -- one). I *could* make this tail recursive but I think in general we
  -- won't be asking for a large amount of hops.
  -- Elsewise when we are actually calculating the hops just for getting
  -- the last one we need not actually return a list.
  getHops :: PiHop -> [ PiHop ]
  getHops current =
    current : (getHops nextHop)
    where
    nextHop :: PiHop
    nextHop = Matrix.rowMultiply current uniformisedMatrix
  -- Our uniformised matrix which we use to multipy each hop by
  -- is then transformed from a list representation into a matrix
  -- representation to allow fast multiplication.
  uniformisedMatrix = uniformMatrix uniMatrix
  -- also the same as hopSize
  -- numberStates      = Matrix.numberOfRows uniformisedMatrix
  



{-|
  Return the passage time results from a list of source states to
  a list of target states. For now the result is just a list of matrices
  demonstrating the n/m hops of the uniform matrix

  The computation of a cummulative distribution function involves the
  computation of the following function:

  sum from n = 1 to infinity  of
    ( 1 - ((e ^ (-qt)) * sum from k=0 to n-1 of
                           ((qt)^ k) / k! )
    *
    sum of k in target state of
      pi(n) k

  In otherwords the top part of the equation multiplied by the
  probability of completing the passage in exactly 'n' hops.
-}
getPassageEndTimes :: Bool
                   -- ^ Whether or not we should normalise the
                   -- probabilities against the probability of
                   -- completing at all.
                -> [ Time ]
                   -- ^ The times to calculate for
                -> SteadyState
                   -- ^ The steady state distribution
                -> UniformMatrix
                   -- ^ The initial uniform matrix
                -> Set StateId
                   -- ^ The source state
                -> [ (String, Set StateId) ]
                   -- ^ The sets of target states
                -> MainControl PassageEndResult
                   -- ^ The resulting values at the given times
getPassageEndTimes normalise times steadystate uniMatrix sources targetSets
  | Set.null sources = MainControl.resultError "No source states provided"
  | nullTargets      = MainControl.resultError "No target states provided"
  | otherwise        = 
  valueResult ptResults "uniform-hops" logInformation
  where
  nullTargets = all Set.null $ map snd targetSets

 
  ptResults = map makePassageTime targetSets

  makePassageTime :: (String, Set StateId) -> (String, PassageResult)
  makePassageTime (name, _targets) = 
    (name, PassageResult { passagePdf = map (selectPdf name) timeValues
                         , passageCdf = map (selectCdf name) timeValues
                         }
    )

  -- Note that in contrast to transient analysis performed below
  -- the 'mHops' are calculated over the modified uniformised-matrix.
  -- This is one which has had an absorbing state added such that we
  -- calculate only the first passage and not subsequent passages.
  mHops          = calculateNHopsUniformMatrix probDist absorbMatrix sources
  probDist       = Just $ steadyProbabilities steadystate
  absorbResult   = addAbsorbingState allTargets uniMatrix 
  absorbMatrix   = fst absorbResult
  absorbIndex    = snd absorbResult
  -- Get the value of 'q' that was used to uniformise the matrix
  qValue         = uniformQValue uniMatrix
  allTargets     = Set.unions $ map snd targetSets

  selectPdf :: String
           -> (Time, [ (String, (Double, Double, Double)) ]) 
           -> (Time, Double)
  selectPdf name (time, values) =
     case lookup name values of
       Just (_, pdf, _) 
         | normalise -> (time, divProb pdf allPdfs)
         | otherwise -> (time, pdf)
       Nothing          -> (time, 0.0) -- maybe should be an error
     where
     allPdfs = sum $ map  (Utils.snd3 . snd) values
  selectCdf :: String
           -> (Time, [ (String, (Double, Double, Double)) ]) 
           -> (Time, Double)
  selectCdf name (time, values) = 
     case lookup name values of
       Just (cdf, _, _) 
         | normalise -> (time, divProb cdf allCdfs)
         | otherwise -> (time, cdf)
       Nothing          -> (time, 0.0) -- maybe should be an error
     where
     allCdfs = sum $ map (Utils.fst3 . snd) values
  timeValues    = map getValuesAtTime times

  divProb :: Probability -> Probability -> Probability
  divProb p1 p2
    | p1 > p2             = error "how can p1 be larger than p2?"
    | p1 > 1              = error "how can p1 be greater than one?"
    | (p1 / p2) > 1       = error "how??"
    | p1 == 0.0           = 0.0
    | p2 == 0.0           = error "How can p2 be zero if p1 is not?"
    | otherwise           = p1 / p2

  getValuesAtTime :: Time -> (Time, [ (String, (Double, Double, Double)) ])
  getValuesAtTime time =
    (time, theseValues)
    where
    theseValues  = calculateValueAtTime validHop
                                        getIntermediate 
                                        combineProbs
                                        qValue
                                        mHops
                                        time

    -- We want to take some range of hops, it would be nice to
    -- calculate until n=infinity but since that's not possible
    -- we calculate until n=X for some X such that the probability
    -- of being in the absorbing state is very close to one.
    validHop :: PiHop -> Bool
    validHop hop = (1 - (lastValueInHop hop)) > 0.0001

    lastValueInHop :: PiHop -> Probability
    lastValueInHop hop = Matrix.safeVectorIndex hop absorbIndex
    
    getIntermediate :: Probability -> Probability -> Probability 
                    -> PiHop -> [ (String, (Double, Double, Double)) ]
    getIntermediate erlangCdf erlangPdf erlangNewPdf piHop =
      map getTargetSetValues targetSets

      where
      getTargetSetValues :: (String, Set StateId) 
                         -> (String, (Double, Double, Double))
      getTargetSetValues (targetName, targets) =
        ( targetName, ( erlangCdf    * targetsProbability
                      , erlangPdf    * targetsProbability
                      , erlangNewPdf * targetsProbability
                      )
        )
        where 
        -- This is the probability that in this hop we are in one
        -- of the target states.
        targetsProbability = 
          Utils.errorIf condition errorMsg sumValue
          where 
          condition = sumValue > (1.0 + errorThreshold)
          errorMsg  = error $ concat [ "sum probs greater than one: "
                                     , show sumValue ]
          sumValue = sumMapSet getTargetProb targets      
          getTargetProb :: StateId -> Probability
          getTargetProb = stateIdIndexProbVector piHop

  
    combineProbs :: [ [(String, (Double,Double,Double)) ] ]
                  ->  [(String, (Double,Double,Double)) ]
    combineProbs =
      foldl addValues $ map makeZeroTargets targetSets
      where
      makeZeroTargets :: (String, Set StateId) 
                      -> (String, (Double, Double, Double))
      makeZeroTargets (s, _) = (s, (0.0, 0.0, 0.0))
      addValues :: [ (String, (Double, Double, Double)) ] 
                -> [ (String, (Double, Double, Double)) ] 
                -> [ (String, (Double, Double, Double)) ] 
      addValues = zipWith addSingleTarget
        where
        addSingleTarget :: (String, (Double, Double, Double))
                        -> (String, (Double, Double, Double))
                        -> (String, (Double, Double, Double))
        addSingleTarget (s1, (cdf1, pdf1, npdf1))
                        (s2, (cdf2, pdf2, npdf2))
          | s1 /= s2  = error "We have a serious fatal flow getPassageEndTimes"
          | otherwise = (s1, ( cdf1 + cdf2
                             , pdf1 + pdf2
                             , npdf1 + npdf2
                             )
                        )
                  
  -- The log information 
  -- (maybe this would be better in calculateValueAtTime)
  logInformation = unlines  [ sourceStatesLog
                            -- , targetStatesLog
                            , mHopsString
                            ]

  sourceStatesLog = "The source states were: " ++
                    (Utils.mkCSlist $ map show $ Set.toList sources)

  -- targetStatesLog = "The target states were: " ++
     --                (Utils.mkCSlist $ map show $ Set.toList targets)

  -- A log string of some of the hops, this might be useful for 
  -- debugging, or also just for educational use on uniformisation.
  mHopsString = unlines $ map showHop $ take 20 mHops

  -- Show, for the purposes of logging, a probability vector.
  showHop :: PiHop -> String
  showHop row1 = unlines [ "Length of row is : " ++ 
                            (show $ Matrix.dimension row1)
                         , "Sum of row is : " ++ 
                           (show $ sum $ Matrix.toList row1)
                         , show row1
                         ]

-- Utility function

stateIdIndexProbVector :: ProbVector -> StateId -> Probability
stateIdIndexProbVector pv =
  Markov.indexProbVector pv . States.stateIdIndex 1

-- Maps the values in a set into some other values which can be
-- added together and sums the resulting set. (actually does the
-- mapping and summing without an intermediate set).
sumMapSet :: Num b => (a -> b) -> Set a -> b
sumMapSet f = Set.fold (\a -> \soFar -> (f a) + soFar) 0 
