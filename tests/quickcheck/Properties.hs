{-# OPTIONS -fglasgow-exts #-}

{- Imported Standard Libraries -}
import Prelude hiding
   ( all )
import Control.Monad
   ( liftM 
   , when
   , mapAndUnzipM
   )
import Data.Char
   ( ord )
import Data.Foldable
   ( all )
import Data.List
   ( sort
   , group
   , intersperse
   , partition
   )
import Data.Set
   ( Set
   , member
   , toList
   )
import System.Environment
   ( getArgs )
import System.IO
   ( hFlush
   , stdout
   )
import System.Random
   ( StdGen
   , newStdGen
   , split
   , mkStdGen
   )
import Test.QuickCheck
   ( Arbitrary      ( .. )
   , Gen            ( .. )

   , Testable

   , choose
   , defaultConfig
   , configMaxTest
   , configEvery
   , Config
   , evaluate
   , Result
   , configMaxTest
   , configMaxFail
   , configEvery
   , arguments
   , ok
   , stamp
   , generate
   , configSize
   )
import Text.Printf
   ( printf )
{- Imported External Libraries -}
{- Imported Local Libraries -}
import Language.Pepa.Syntax
   ( QualifiedName    ( .. )
   , ActionIdentifier
   , ProcessDef
   , ParsedComponent  ( .. )
   , ParsedTrans
   , ParsedAction     ( .. )
   , ParsedRateExp    ( .. )
   , nameOfAction
   )
import Language.Pepa.Analysis.Analysis
   ( possibleActionsNonAliased )
import Language.Pepa.Probes.StochasticProbes
   ( ProbeR         ( .. )
   , firstNames
   , probeAlphabet
   , containsNeedlessRep
   )
import Language.Pepa.Probes.Translate
   ( translateProbeR )
{- End of Imports -}


-- ---------------------------------------------------------------------
-- QuickCheck properties for the Probes

-- Some general hints for creating StackSet properties:
--
-- *  ops that mutate the StackSet are usually local
-- *  most ops on StackSet should either be trivially reversible, or 
--    idempotent, or both.

--
-- The all important Arbitrary instance for ProbeR.
--
instance Arbitrary ProbeR where
   arbitrary   = arbitraryProbe 10
   coarbitrary = error "no coarbitrary for ProbeR"

{-
data ProbeR = SPact    ActionIdentifier -- SPaction
            | SPlabel  ProbeR ActionIdentifier -- | R : label
            | SPseq    ProbeR ProbeR           -- | R1, R2
            | SPchoice ProbeR ProbeR           -- | (R1 | R2)
            | SPiter   ProbeR Int              -- | R{n}
            | SPbit    ProbeR (Int, Int)       -- | R{m,n}
            | SPoom    ProbeR                  -- | R+
            | SPzom    ProbeR                  -- | R*
            | SPzoo    ProbeR                  -- | R?
            | SPwo     ProbeR ActionIdentifier 
              deriving (Show, Eq)  -- don't derive just temporary for debugging
-}
-- As we get deeper into the tree it becomes more and more likely that we
-- will choose just an action (ie a leaf node)
arbitraryProbe :: Int -> Gen ProbeR
arbitraryProbe limit =
   do n <- choose (1, limit)
      case n of
         2  -> arbitraryLabel
         3  -> arbitraryTwo SPseq
         4  -> arbitraryTwo SPchoice
         5  -> arbitraryIter
         6  -> arbitraryBit
         7  -> arbitraryOne SPoom
         8  -> arbitraryOne SPzom
         9  -> arbitraryOne SPzoo
         10 -> arbitraryWithout
         _  -> arbitraryAction
   where
   newLimit = limit + 10
   arbitraryAction :: Gen ProbeR
   arbitraryAction = do s <- arbitrary
                        return (SPact $ Unqualified s)

   arbitraryLabel :: Gen ProbeR
   arbitraryLabel = do probe <- arbitraryProbe newLimit
                       label <- arbitrary
                       return (SPlabel probe $ Unqualified label)

   -- creates two arbitrary probes and uses the given function to
   -- combine them together to form one probe.
   arbitraryTwo :: (ProbeR -> ProbeR -> ProbeR) -> Gen ProbeR
   arbitraryTwo combine =
      do r1 <- arbitraryProbe newLimit
         r2 <- arbitraryProbe newLimit
         return $ combine r1 r2

   -- creates one arbitrary probe and uses the given function to
   -- to create a composite probe
   arbitraryOne :: (ProbeR -> ProbeR) -> Gen ProbeR
   arbitraryOne mk =
      do r1 <- arbitraryProbe newLimit
         return $ mk r1

   arbitraryWithout :: Gen ProbeR
   arbitraryWithout =
      do r1    <- arbitraryProbe newLimit
         label <- arbitrary
         return (SPwo r1 $ Unqualified label)

   arbitraryIter :: Gen ProbeR
   arbitraryIter =
      do r1 <- arbitraryProbe newLimit
         i  <- choose (1, 6)
         return $ SPiter r1 i

   -- Note that this may generate a probe such as R1 {20 , 1}
   -- (ie, with m > n)
   -- which doesn't really make sense, but for testing purposes
   -- may be desirable. If we decide not we can easily say
   -- @ m <- choose (1, 49)
   --   n <- choose (m, 50)
   -- @    
   arbitraryBit :: Gen ProbeR
   arbitraryBit =
      do r1 <- arbitraryProbe newLimit
         m  <- choose (1, 6)
         n  <- choose (1, 10)
         return $ SPbit r1 (m, n)



------------------------------------------------------------------------
-- Properties that probes should have


propInvariant :: Bool
propInvariant = True

firstNamesInAlphabet :: ProbeR -> Bool
firstNamesInAlphabet probe =
   all inAlphabet $ firstNames probe
   where
   inAlphabet :: ActionIdentifier -> Bool
   inAlphabet a = member a $ probeAlphabet probe

{-
   Here we pretend there is a model to attach the probe to
   and just make the alphabet of the probe into the transitions
   of the model. Then we check that the translation has some
   properties.

   'performsAllInStates' fails if the labels can also be performed
   by the model. In this case unfortunately the labels are not
   seen as communication actions. I'm not sure how to direct
   the generation such that this does not occur. This is definitely
   a todo.

   'performsAllInStates' also fails if we have a replicator probe
   attached to a label. This is for technical reasons, eg
   (a, b)+ : start will have a state which can perform the 'start'
   communication or the 'a' activity. To avoid this we allow the
   test to succeed in the case that the probe does this.

   Another todo, we do not get the correct shape if we have
   @(a, b:start)/p@ in other words a label as the last thing
   which can be done in a without. Because the reset will add
   a reset for p in the state in which we are about to perform the
   immediate communication. This is a bit more tricky since I think
   it is reasonable to write this down.
-}
probeTranslation :: ProbeR -> Bool
probeTranslation probe =
   translatedProbeOkay pDefs &&
   ( containsNeedlessRep probe ||
     performsAllInStates actions pDefs )
   where
   pDefs       = translateProbeR [] "ProbeTest" transitions probe
   transitions = map makeTrans $ toList actions
   makeTrans :: ActionIdentifier -> ParsedTrans
   makeTrans a = (Action a, RateTop)
   actions     =  probeAlphabet probe


translatedProbeOkay :: [ ProcessDef ] -> Bool
translatedProbeOkay =
  all (compOfCorrectShape . snd)

{- Checks if a component of translated def is of the correct
   shape. This should check that it is made up of choice and
   simple prefixes. That is, of the form
   @ (a, r) . P
   + (b, r) . P1
   + (c, r) . P2 
   @
   where each of the 'next' part of the prefixes are named components.
-}
compOfCorrectShape :: ParsedComponent -> Bool
compOfCorrectShape (ComponentSum left right)         =
   compOfCorrectShape left
   && compOfCorrectShape right
compOfCorrectShape (PrefixComponent _ (IdProcess _)) = True
compOfCorrectShape _                                 = False


{- 
   Can either do all of the actions exactly once, or one
   communication action.
-}
performsAllInStates :: Set ActionIdentifier -> [ ProcessDef ] -> Bool
performsAllInStates actionNames pDefs =
   all isCommunicationOrPerformsAll pDefs
   where
   isCommunicationOrPerformsAll :: ProcessDef -> Bool
   isCommunicationOrPerformsAll pDef =
      case possibleActionsNonAliased $ snd pDef of
         [ ComAction _ ] -> True
         transitions     ->
            all occursExactlyOnce actionNames
            where
            transActionNames = map nameOfAction transitions
            occursExactlyOnce :: ActionIdentifier -> Bool
            occursExactlyOnce a =
               1 == ((length . fst) $ partition (== a) transActionNames)

------------------------------------------------------------------------
-- Main program for driving the tests.

main :: IO ()
main = getArgs >>= processArgs

processArgs :: [ String ] -> IO ()
processArgs []                 = runTests 100
processArgs ("--looptest" : _) = printAndRun 100
processArgs ( h : _ )          = runTests $ read h

runTests :: Int -> IO ()
runTests n =
   do (results, passed) <- mapAndUnzipM showTestResult tests
      putStrLn $ unwords [ "Passed"
                         , show $ sum passed
                         , "tests!"
                         ]
      when (not . and $ results) $ fail "Not all tests passed!"
   where
   tests = [ ( "ProbeR invariants"  , mytest propInvariant )
           , ( "ProbeR names"       , mytest firstNamesInAlphabet )
           , ( "ProbeR translation" , mytest probeTranslation ) 
           ]

   showTestResult (s, a) = printf "%-25s: " s >> a n

printAndRun :: Int -> IO ()
printAndRun n
   | n <= 0    = return ()
   | otherwise =
      do print probe
         print $ probeTranslation probe
         printAndRun (n - 1)
      where
      probe :: ProbeR
      probe = generate n (mkStdGen n) arbitrary

------------------------------------------------------------------------
--
-- QC driver
--

debug = False

mytest :: Testable a => a -> Int -> IO (Bool, Int)
mytest a n = mycheck defaultConfig
    { configMaxTest=n
    , configEvery   = \n args -> let s = show n in s ++ [ '\b' | _ <- s ] } a
 -- , configEvery= \n args -> if debug then show n ++ ":\n" ++ unlines args else [] } a


mycheck :: Testable a => Config -> a -> IO (Bool, Int)
mycheck config a = do
    rnd <- newStdGen
    mytests config (evaluate a) rnd 0 0 []


mytests :: Config -> Gen Result -> StdGen -> Int -> Int -> [[String]] -> IO (Bool, Int)
mytests config gen rnd0 ntest nfail stamps
    | ntest == configMaxTest config = done "OK," ntest stamps >> return (True, ntest)
    | nfail == configMaxFail config = done "Arguments exhausted after" ntest stamps >> return (True, ntest)
    | otherwise               =
      do putStr (configEvery config ntest (arguments result)) >> hFlush stdout
         case ok result of
           Nothing    ->
             mytests config gen rnd1 ntest (nfail+1) stamps
           Just True  ->
             mytests config gen rnd1 (ntest+1) nfail (stamp result:stamps)
           Just False ->
             putStr ( "Falsifiable after "
                   ++ show ntest
                   ++ " tests:\n"
                   ++ unlines (arguments result)
                    ) >> hFlush stdout >> return (False, ntest)
     where
      result      = generate (configSize config ntest) rnd2 gen
      (rnd1,rnd2) = split rnd0

done :: String -> Int -> [[String]] -> IO ()
done mesg ntest stamps = putStr ( mesg ++ " " ++ show ntest ++ " tests" ++ table )
  where
    table = display
            . map entry
            . reverse
            . sort
            . map pairLength
            . group
            . sort
            . filter (not . null)
            $ stamps

    display []  = ".\n"
    display [x] = " (" ++ x ++ ").\n"
    display xs  = ".\n" ++ unlines (map (++ ".") xs)

    pairLength xss@(xs:_) = (length xss, xs)
    entry (n, xs)         = percentage n ntest
                       ++ " "
                       ++ concat (intersperse ", " xs)

    percentage n m        = show ((100 * n) `div` m) ++ "%"


------------------------------------------------------------------------

instance Arbitrary Char where
    arbitrary   = choose ('a','z')
    coarbitrary = coarbitrary . ord

{-
instance Random Word8 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

instance Arbitrary Word8 where
  arbitrary     = choose (minBound,maxBound)
  coarbitrary n = variant (fromIntegral ((fromIntegral n) `rem` 4))

instance Random Word64 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

instance Arbitrary Word64 where
  arbitrary     = choose (minBound,maxBound)
  coarbitrary n = variant (fromIntegral ((fromIntegral n) `rem` 4))

integralRandomR :: (Integral a, RandomGen g) => (a,a) -> g -> (a,g)
integralRandomR  (a,b) g = case randomR (fromIntegral a :: Integer,
                                         fromIntegral b :: Integer) g of
                            (x,g) -> (fromIntegral x, g)

instance Arbitrary Position  where
    arbitrary = do n <- arbitrary :: Gen Word8
                   return (fromIntegral n)
    coarbitrary = undefined

instance Arbitrary Dimension where
    arbitrary = do n <- arbitrary :: Gen Word8
                   return (fromIntegral n)
    coarbitrary = undefined

instance Arbitrary Rectangle where
    arbitrary = do
        sx <- arbitrary
        sy <- arbitrary
        sw <- arbitrary
        sh <- arbitrary
        return $ Rectangle sx sy sw sh
    coarbitrary = undefined

instance Arbitrary Rational where
    arbitrary = do
        n <- arbitrary
        d' <- arbitrary
        let d =  if d' == 0 then 1 else d'
        return (n % d)
    coarbitrary = undefined

------------------------------------------------------------------------
-- QC 2

-- from QC2
-- | NonEmpty xs: guarantees that xs is non-empty.
newtype NonEmptyList a = NonEmpty [a]
 deriving ( Eq, Ord, Show, Read )

instance Arbitrary a => Arbitrary (NonEmptyList a) where
  arbitrary   = NonEmpty `fmap` (arbitrary `suchThat` (not . null))
  coarbitrary = undefined

newtype NonEmptyNubList a = NonEmptyNubList [a]
 deriving ( Eq, Ord, Show, Read )

instance (Eq a, Arbitrary a) => Arbitrary (NonEmptyNubList a) where
  arbitrary   = NonEmptyNubList `fmap` ((liftM nub arbitrary) `suchThat` (not . null))
  coarbitrary = undefined


type Positive a = NonZero (NonNegative a)

newtype NonZero a = NonZero a
 deriving ( Eq, Ord, Num, Integral, Real, Enum, Show, Read )

instance (Num a, Ord a, Arbitrary a) => Arbitrary (NonZero a) where
  arbitrary = fmap NonZero $ arbitrary `suchThat` (/= 0)
  coarbitrary = undefined

newtype NonNegative a = NonNegative a
 deriving ( Eq, Ord, Num, Integral, Real, Enum, Show, Read )

instance (Num a, Ord a, Arbitrary a) => Arbitrary (NonNegative a) where
  arbitrary =
    frequency
      [ (5, (NonNegative . abs) `fmap` arbitrary)
      , (1, return 0)
      ]
  coarbitrary = undefined

-- | Generates a value that satisfies a predicate.
suchThat :: Gen a -> (a -> Bool) -> Gen a
gen `suchThat` p =
  do mx <- gen `suchThatMaybe` p
     case mx of
       Just x  -> return x
       Nothing -> sized (\n -> resize (n+1) (gen `suchThat` p))

-- | Tries to generate a value that satisfies a predicate.
suchThatMaybe :: Gen a -> (a -> Bool) -> Gen (Maybe a)
gen `suchThatMaybe` p = sized (try 0 . max 1)
 where
  try _ 0 = return Nothing
  try k n = do x <- resize (2*k+n) gen
               if p x then return (Just x) else try (k+1) (n-1)
-}