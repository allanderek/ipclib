{- For now we translate the L.P.C.Model into a hierarchical
   model and then output that as a java program. Essentially we're
   compiling to a Java representation of a hydra model. This may lead
   to enormous rate expressions. What might be better is to translate
   this into a java representation of a L.P.C.Model, this may have the
   disadvantage that it takes longer to work out the states and the
   next transition list, but has the advantage that we do not output
   massively large rate expressions. Additionally it may be quicker
   because for a very coupled model it may be that for the hydra model
   we end up with a large amount of possible moves but actually a lot of
   these are never enabled because the model cannot get into that state.
-}

module Language.Pepa.Compile.JavaSimulator
  ( SimulatorConfig        ( .. )
  , modelToPassSim
  , statespaceToPassSim
  , statespaceToSimulator
  )
where

{- Standard Library Modules Imported -}
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Set
  ( Set )
{- External Library Modules Imported -}
{- Local Modules Imported -}
import qualified Language.Pepa.Utils as Utils
import qualified Language.Pepa.QualifiedName as Qualified
import Language.Pepa.QualifiedName
  ( QualifiedName )
import qualified Language.Pepa.Rates as Rates
import qualified Language.Pepa.Print as Print
import qualified Language.Pepa.Compile.States as States
import Language.Pepa.Compile.States
  ( State       ( .. )
  , StateSpace  ( .. )
  , StateId
  , StateCondition
  )
import qualified Language.Pepa.Compile.Model as Model
import Language.Pepa.Compile.Model
  ( Model       ( .. )  
  , Motion     ( .. )
  )
import qualified Language.Pepa.Compile.Hydra as Compile.Hydra
import qualified Language.Hydra.Syntax as Hydra
  
import Language.Pepa.MainControl
  ( MainControl )
{- End of Module Imports -}

type Time = Double

data SimulatorConfig =
  SimulatorConfig { sconfigSimulations :: Int
                  , sconfigTimes       :: [ Time ]
                  }

{-| Given a description of a model turn it into a java simulator
    to calculate the passage-time quantiles
-}
modelToPassSim :: String              -- ^ The name of the generated java class
               ->  SimulatorConfig    -- ^ Configuration for the simulator 
               ->  StateCondition     -- ^ The target state conditions
               ->  Model              -- ^ The model description
               ->  String             -- ^ The returned java program contents
modelToPassSim className sconfig targets model =
  concat [ headers, classHead, mainMethod, sampleClass ]
  where
  transModel = Model.modelToTransModel model

  times      = sconfigTimes sconfig
  timeLimit  = Utils.safeList 10.0 last times
  simulRuns  = show $ sconfigSimulations sconfig

  timesString = List.intercalate "\n          , " $ map show times

  headers    = unlines [ "import java.util.*;" 
                       , "import java.io.*;"
                       , "" 
                       ]
  classHead  = unlines [ "public class " ++ className ++ " {"
                       , ""
                       , "   private static double getPercent(int p, int t){"
                       , "     return 100 * ((double) p) / ((double) t) ;"
                       , "   } "
                       , "    "
                       , concat [ "   private static double timeLimit = "
                                , show timeLimit
                                , " ;"
                                ]
                       , concat [ "   public static double[] times = {" 
                                , timesString
                                , "};"
                                ]
                       , concat [ "   public static int numberBuckets = "
                                , show $ length times
                                , " ; "
                                ] 
                       , concat [ "   private static int samples   = "
                                , simulRuns 
                                , " ;"
                                ]
                       ]

  mainMethod = unlines [ "   public static void main(String[] args) {"
                       , concat [ "     int buckets[]    = new int[ "
                                , "numberBuckets"
                                , "] ;"
                                ]
                       , concat [ "     double cdfBuckets[]    = new double[ "
                                , "numberBuckets"
                                , "] ;"
                                ]
                       , "     int outOfTime    = 0 ;"
                       , "     Sampler sampler  = new Sampler (timeLimit) ;"
                       , "     SAMPLER_LOOP:"
                       , "     for (int i = 0; i < samples; i++){"
                       , "        double sampleTime = sampler.sample () ;"
                       , "        for (int k = 0 ; k < numberBuckets; k++){"
                       , "           if (sampleTime < times[k]){"
                       , "             buckets[k]++ ; continue SAMPLER_LOOP ;"
                       , "            }"
                       , "        }"
                       , "       // If we get here we ran out of time"
                       , "       outOfTime++;"
                       , "     }"
                       , "     // Now we set up the cdf buckets"
                       , "     int allDone      = 0 ;"
                       , "     for(int i = 0; i < numberBuckets; i++) {"
                       , "        allDone += buckets[i] ;"
                       , "        cdfBuckets[i] = getPercent(allDone, samples) ;"
                       , "     }"
                       , ""
                       , ""
                       , "     // Now we just want to print out the numbers in each bucket"
                       , "     for (int i = 0; i < numberBuckets; i++) {"
                       , "       System.out.println"
                       , "              (\"# buckets[\" + i + \"] chosen \" +"
                       , "               buckets[i] + \" times, i.e. \" + "
                      ,  "               getPercent(buckets[i], samples) +"
                       , "               \"% of the time, the cdf is: \" + cdfBuckets[i] ) ;"
                       , "     }"
                       , "     System.out.println"
                       , "              (\"We ran out of time: \" + outOfTime + \" i.e. \" +"
                       , "                getPercent (outOfTime, samples) + \"% of the time\") ;"
                       , "     if (sampler.deadlocked > 0) {"
                       , "        System.out.println (\"We deadlocked (not in a target state) \" "
                       , "                             + sampler.deadlocked + \" times\" ) ;"
                       , "     }"
                       , ""

                       , "    "
                       , "    try"
                       , "    {"
                       , concat [ "       FileOutputStream fout = new FileOutputStream (\""
                                , className
                                , "-cdf.csv\");"
                                ]
                       , "       PrintStream pout = new PrintStream(fout) ;"
                       , "       for (int i = 0; i < numberBuckets; i++){"
                       , "         pout.println (times[i] + \", \" + cdfBuckets[i] / 100.0);"
                       , "       }"
                       , "       // Close our output stream"
                       , "       fout.close();"
                       , "    // Catches any error conditions"
                       , "    }"
                       , "    catch (IOException e)"
                       , "    {"
                       , "       System.err.println (\"Unable to write to file\");"
                       , "       System.exit(-1);"
                       , "    }"
                       , "   return ;"
                       , "   }"
                       , ""
                       , "}"
                       , ""
                       ]

  sampleClass = unlines [ "class Sampler {"
                        , "   public Sampler (double limitTime) {"
                        , "      timeLimit = limitTime ;"
                        , "   }"
                        , " "
                        , "   private Random generator = new Random();"
                        , "   private double timeLimit = 10.0  ;"
                        , ""
                        , ""
                        , "   // sample from the exponential distribution "
                        , "   private double expDelay(double mean) {"
                        , "     return - mean * Math.log(generator.nextDouble());"
                        , "   }"
                        , ""
                        , "   private List<Transition> nextTransitions ; "
                        , "   public int deadlocked = 0 ;"
                        , ""
                        , "   /* Note shouldn't be called if nextTransitions is null */"
                        , "   private Transition chooseMove () {"
                        , "      int length       = nextTransitions.size () ;    "
                        , "      double totalRate = 0 ;"
                        , "      for (int i = 0; i < length ; i++){"
                        , "        double thisRate = (nextTransitions.get(i)).rate ;"
                        , "        totalRate += thisRate ;"
                        , "      }"
                        , "      double passedProbability = 0 ;"
                        , "      double picker            = generator.nextDouble () ;"
                        , "      double chooser           = totalRate * picker ;"
                        , ""
                        , "      for (int k = 0; k < length ; k++){"
                        , "        Transition kTrans = nextTransitions.get(k) ;"
                        , "        passedProbability += kTrans.rate ;"
                        , "        if (chooser < passedProbability) {"
                        , "          return (new Transition (kTrans.stateRepr, totalRate, kTrans.action)) ;"
                        , "        }"
                        , "      }"
                        , "      System.out.println (\"WE SHOULD NEVER GET HERE!!!\") ;"
                        , "      // If we haven't returned by now, it's the last one"
                        , "      double rate       = totalRate ;"
                        , "      Transition chosen = nextTransitions.get(length - 1) ;"
                        , "      return (new Transition (chosen.stateRepr, rate, chosen.action)) ;"
                        , "   }"
                        , ""
                        , "   public double sample() {"
                        , "     double total    = 0.0 ; "
                        , "     double dice     = 0.0 ;"
                        , "     StateRepr state = new StateRepr () ;"
                        , ""
                        , ""
                        , "     while (total <= timeLimit && !state.isTargetState()){"
                        , "        /* See definition for why this apparently needlessly"
                        , "           takes in the current total time taken */"
                        , "        nextTransitions       = state.getNext (total) ;"
                        , "        /* We might be in a deadlocked state, if this occurs then"
                        , "           nextTransitions will be null, we simply break out of the"
                        , "           while loop since we will not be in the target state but"
                        , "           never will be either."
                        , "        */"
                        , "        if (nextTransitions.isEmpty())"
                        , "           { total = timeLimit ; deadlocked++ ; break ; }"
                        , "        Transition transition = chooseMove () ;"
                        , "        state                 = transition.stateRepr ;"
                        , "        total                 += expDelay(1 / transition.rate) ;"
                        , "     }"
                        , "     return total ;"
                        , "   }"
                        , "}// End of class Sampler"
                        , ""
                        , "class StateRepr implements Cloneable {"
                        , ""
                        , unlines processLines
                        {-
                        , "  private int p1 ; // Number of processes in p1"
                        , "  private int p2 ; // Number of processes in p2"
                        , "  private int p3 ; // Number of processes in p3"
                        -}
                        , ""
                        , "  // The initial state is represented here."
                        , concat [ "  public StateRepr () { "
                                 , initialAssigns
                                 , "}"
                                 ]
                        , ""
                        , "  public StateRepr clone (){"
                        , "    StateRepr result = new StateRepr () ;"
                        , unlines cloneLines
                        , "    return result ;"
                        , "  }"
                        , " "
                        , concat [ "  public boolean isTargetState () { return ("
                                 , targetCondition 
                                 , ") ;  }"
                                 ]
                        , ""
                        , "  public double min(double a, double b) {"
                        , "       return Math.min(a,b) ; }"
                        , ""
                        , ""
                        , "  /* This function unnecessarily takes in the current time"
                        , "     if that value is used then we are non-markovian."
                        , "     It is useful though if you sometimes wish to hand-"
                        , "     modify the simulation for example to have different"
                        , "     behaviours in day and night"
                        , "  */"
                        , "  public LinkedList<Transition> getNext (double total) {"
                        , "    LinkedList <Transition> results = new LinkedList<Transition> () ;"
                        , "    // These switch statements are in parallel because the"
                        , "    // two components are composed with ||"
                        , "    StateRepr next ;"
                        , ""
                        , unlines addTransitionLines
{-
                        , "    // First do the cooperations:"
                        , "    if (p1 == 4)"
                        , "    { next    = this.clone () ;"
                        , "      next.p1 = 0 ;"
                        , "      next.p2 = 4 ;"
                        , "      results.add (new Transition (next, rate_a, \"a\")) ;"
                        , "    }"
                        , ""
                        , "    // Now the non-cooperations"
                        , "    if (p2 > 0)"
                        , "    { next = this.clone () ;"
                        , "      next.p2 = p2 - 1 ;"
                        , "      next.p3 = p3 + 1 ;"
                        , "      results.add (new Transition (next, rate_b * p2, \"b\")) ;"
                        , "    } "
                        , ""
                        , "    if (p3 > 0)"
                        , "    { next    = this.clone () ;"
                        , "      next.p3 = p3 - 1 ;"
                        , "      next.p1 = p1 + 1 ;"
                        , "      results.add (new Transition (next, rate_c * p3, \"c\")) ;"
                        , "    }"
-}
                        , "    return results ;"
                        , "  }"
                        , "}"
                        , ""
                        , "class Transition {"
                        , "  public StateRepr stateRepr ;"
                        , "  public double rate ;"
                        , "  public String action ;"
                        , ""
                        , "  public Transition (StateRepr s, double r, String a) {"
                        , "    stateRepr = s ;"
                        , "    rate      = r ;"
                        , "    action    = a ;"
                        , "  }"
                        , "}"
                        ]

  -- The target condition will not print out as is and still work
  -- because any of the process identifiers to which it refers will
  -- be printed out with a leading capital letter. Hence we have
  -- to first modify the rate expression
  targetCondition    = Print.hprintRateExpr $ uncapitaliseRateExpr targets
  uncapitaliseRateExpr :: Rates.RateExpr -> Rates.RateExpr
  uncapitaliseRateExpr =
    -- A little ugly this but the passed in function is just uncapitalising
    -- the name of the rate identifier, unfortunately to do so we get back
    -- a string which we need to make into a qualified name before a rate expr. 
    Rates.transformRateExpr (Rates.Cident . 
                             Qualified.unqualified . 
                             makeIdentName)
  addTransitionLines = mkTransitionLines $ Model.tmodelTransitions transModel
  
  mkTransitionLines :: [ Motion ]-> [ String ]
  mkTransitionLines motions =
    concatMap mkTrans motions
    where
    mkTrans :: Motion -> [ String ]
    mkTrans motion =
      [ concat [ "if (", conditions, "){" ]
      , " next = this.clone () ;"
      , unlines $ map makeDecrement $ Model.motionDecrements motion
      , unlines $ map makeIncrement $ Model.motionIncrements motion
      , concat [ "double localRate = "
               , Print.hprintParsedRate $ 
                 Rates.modifyRate uncapitaliseRateExpr $ 
                 motionSpeed motion
               , ";"
               ]
      , concat [ "  if (localRate > 0.0) {"
               , "     results.add (new Transition (next, localRate, "
               , Utils.doubleQuoteString 
                 $ Qualified.textual $ Model.motionKind motion
               , ")) ;"
               , " }"
               , "}"
               ]
      ]  
      where
      makeDecrement = makeAdjustment "--"
      makeIncrement = makeAdjustment "++"
      makeAdjustment :: String -> QualifiedName -> String
      makeAdjustment s d = concat [ "next."
                               , name
                               , s
                               , " ; "
                               ]
                        where
                        name = makeIdentName d
      conditions = foldr1 andCond sources 
      sources    = map makeCondition $ Model.motionDecrements motion
      makeCondition :: QualifiedName -> String
      makeCondition qName = (makeIdentName qName) ++ (" > 0")
      andCond :: String -> String -> String
      andCond c1 c2 = concat [ c1, " && ", c2 ]

  processLines  = map makeProcessLine processStates
  cloneLines    = map makeCloneLine processStates
  makeProcessLine :: QualifiedName -> String
  makeProcessLine qName = 
    concat [ "  private int "
           , p1
           , " = 0 ; // Number of processes in "
           , p1
           ]
    where
    p1 = makeIdentName qName

  makeCloneLine :: QualifiedName -> String
  makeCloneLine qName =
    concat [ "   result.", p1, " = ", p1, " ;" ] 
    where
    p1 = makeIdentName qName
  processStates = concat $ Model.tmodelDerivatives transModel 

  initialAssigns = unwords $ map makeInitial (Model.tmodelInitial transModel)
  makeInitial (c, i) = unwords [ makeIdentName c, "=", show i, ";" ]
  makeIdentName :: QualifiedName -> String
  makeIdentName = Utils.uncapitaliseFirst . Qualified.textual
{- This version just uses the flat model
  unlines [ "import java.util.*;"
          , ""
          , "public class " ++ className ++ " {"
          , ""
          , "   private static double getPercent(int p, int t){"
          , "     return 100 * ((double) p) / ((double) t) ;"
          , "   } "
          , "    "
          , "   private static int timeLimit = 10  ;"
          , "   private static int samples   = 10000 ;"
          , ""
          , "   public static void main(String[] args) {"
          , "     int buckets[]    = new int[timeLimit + 1] ;"
          , "     int cdfBuckets[] = new int[timeLimit + 1] ;"
          , "     int outOfTime    = 0 ;"
          , "     int allDone      = 0 ;"
          , "     Sampler sampler  = new Sampler (timeLimit) ;"
          , ""
          , "     for (int i = 0; i < samples; i++){"
          , "         int bucket = sampler.sample () ;"
          , "        if (bucket <= timeLimit)"
          , "          buckets[bucket]++ ;"
          , "        else outOfTime++ ;"
          , "     }"
          , ""
          , "     // Now we just want to print out the numbers in each bucket"
          , "     for (int i = 0; i < timeLimit + 1; i++) {"
          , "       allDone += buckets[i] ;"
          , "       System.out.println"
          , "              (\"# buckets[\" + i + \"] chosen \" +"
          , "               buckets[i] + \" times, i.e. \" + getPercent(buckets[i], samples) +"
          , "               \"% of the time, the cdf is: \" + getPercent(allDone, samples) ) ;"
          , "     }"
          , "     System.out.println"
          , "              (\"We ran out of time: \" + outOfTime + \" i.e. \" +"
          , "                getPercent (outOfTime, samples) + \"% of the time\") ;"
          , ""
          , "    "
          , "   return ;"
          , "   }"
          , ""
          , "}"
          , ""
          , ""
          , "class Sampler {"
          , "   public Sampler (int limitTime) {"
          , "      timeLimit = limitTime ;"
          , "   }"
          , " "
          , "   private Random generator = new Random();"
          , "   private int timeLimit = 10  ;"
          , ""
          , ""
          , "   // sample from the exponential distribution "
          , "   private double expDelay(double mean) {"
          , "     return - mean * Math.log(generator.nextDouble());"
          , "   }"
          , ""
          , "   private List<Transition> nextTransitions ; "
          , ""
          , "   private Transition chooseMove () {"
          , "      int length       = nextTransitions.size () ;    "
          , "      double totalRate = 0 ;"
          , "      for (int i = 0; i < length ; i++){"
          , "        double thisRate = (nextTransitions.get(i)).rate ;"
          , "        totalRate += thisRate ;"
          , "      }"
          , "      double passedProbability = 0 ;"
          , "      double picker            = generator.nextDouble () ;"
          , "      double chooser           = totalRate * picker ;"
          , ""
          , "      // note, k < length - 1, so that if it gets to length - 1"
          , "      // then we just return the last transition."
          , "      for (int k = 0; k < length - 1; k++){"
          , "        Transition kTrans = nextTransitions.get(k) ;"
          , "        passedProbability += kTrans.rate ;"
          , "        if (chooser < passedProbability) {"
          , "          return (new Transition (kTrans.stateRepr, totalRate)) ;"
          , "        }"
          , "      }"
          , "      // If we haven't returned by now, it's the last one"
          , "      double rate       = totalRate ;"
          , "      Transition chosen = nextTransitions.get(length - 1) ;"
          , "      return (new Transition (chosen.stateRepr, rate)) ;"
          , "   }"
          , ""
          , "   public int sample() {"
          , "     double total    = 0.0 ; "
          , "     double dice     = 0.0 ;"
          , "     StateRepr state = new StateRepr () ;"
          , ""
          , ""
          , "     while (total <= timeLimit && !state.isTargetState()){"
          , "        nextTransitions       = state.getNext () ;"
          , "        Transition transition = chooseMove () ;"
          , "        state                 = transition.stateRepr ;"
          , "        total                 += expDelay(1 / transition.rate) ;"
          , "     }"
          , "     int bucket = (int) Math.ceil(total) ; // /size) ;"
          , "     return bucket ;"
          , "   }"
          , "}// End of class Sampler"
          , ""
          , "class StateRepr implements Cloneable {"
          , ""
          , "  private int p1 ; // 0 = P1, 1 = P2"
          , "  private int p2 ; // 0 = P1, 1 = P2"
          , "  public StateRepr () { p1 = 0 ; p2 = 0 ; }"
          , "  public void setp1 (int i) { p1 = i ; }"
          , "  public void setp2 (int i) { p2 = i ; }"
          , ""
          , "  private double duration_a = 1.5 ; // seconds"
          , "  private double duration_b = 1.5 ; // seconds"
          , ""
          , "  public StateRepr clone (){"
          , "    StateRepr result = new StateRepr () ;"
          , "    result.p1 = p1 ;"
          , "    result.p2 = p2 ;"
          , "    return result ;"
          , "  }"
          , " "
          , "  public boolean isTargetState () { return (p1 == 1 && p2 == 1) ;  }"
          , ""
          , "  public LinkedList<Transition> getNext () {"
          , "    LinkedList <Transition> results = new LinkedList<Transition> () ;"
          , "    // These switch statements are in parallel because the"
          , "    // two components are composed with ||"
          , "    StateRepr next ;"
          , "    switch (p1) {"
          , "      case 0:"
          , "        next = this.clone () ;"
          , "        next.p1 = 1 ;"
          , "        results.add(new Transition(next, 1 / duration_a)) ;"
          , "        break ;"
          , "      case 1:"
          , "        next = this.clone () ;"
          , "        next.p1 = 0 ;"
          , "        results.add(new Transition(next, 1 / duration_b)) ;"
          , "        break ;"
          , "      default :"
          , "            System.out.println (\"Serious bother; undefined state\");"
          , "            System.exit(1);"
          , "    }"
          , "    switch (p2) {"
          , "      case 0:"
          , "        next = this.clone () ;"
          , "        next.p2 = 1 ;"
          , "        results.add(new Transition(next, 1 / duration_a)) ;"
          , "        break ;"
          , "      case 1:"
          , "        next = this.clone () ;"
          , "        next.p2 = 0 ;"
          , "        results.add(new Transition(next, 1 / duration_b)) ;"
          , "        break ;"
          , "      default :"
          , "            System.out.println (\"Serious bother; undefined state\");"
          , "            System.exit(1);"
          , "    }"
          , ""
          , "    return results ;"
          , "  }"
          , "}"
          , ""
          , "class Transition {"
          , "  public StateRepr stateRepr ;"
          , "  public double rate ;"
          , ""
          , "  public Transition (StateRepr s, double r) {"
          , "    stateRepr = s ;"
          , "    rate      = r ;"
          , "  }"
          , "}"
          ]
-}
          


{-| The third argument is the class name -}
statespaceToPassSim :: Set StateId -- ^ The set of source states
                    ->  Set StateId -- ^ The set of target states
                    ->  String      -- ^ The name of the generated java class
                    ->  StateSpace  -- ^ The generated state space
                    ->  String      -- ^ The returned java program contents
statespaceToPassSim sources targets className statespace = 
  unlines simLines 
  where
  simLines = 
            [ "import java.util.Random;"
            , "import java.util.Collection;"
            , concat [ "public class ", className, " {" ]
            , ""
            , "private static Random generator = new Random();"
            , concat [ "   private static int ", timeLimitName, " = 12  ;" ]
            , "   private static int samples    = 10000 ;    "
            , ""
            , "   // sample from the exponential distribution "
            , "   private static double expDelay(double mean) {"
            , "     return - mean * Math.log(generator.nextDouble());"
            , "   }"
            , "   private static double getPercent(int p, int t){"
            , "     return 100 * ((double) p) / ((double) t) ;"
            , "   } "
            , ""
            , ""
            , concat [ "   private static int[] targetStatesArray = { "
                     , targetIds
                     , "} ;" ]
            , concat [ "   private static int[] sourceStatesArray = { "
                     , sourceIds
                     , "} ;" ]
            , "   "
            , ""
            , "   private static boolean isTargetState (int s){"
            , "     for (int i = 0; i < targetStatesArray.length; i++) {"
            , "       if (s == targetStatesArray[i])"
            , "         return true ;"
            , "     }"
            , "     return false ;"
            , "   }"
            , "   private static boolean isSourceState (int s){"
            , "     for (int i = 0; i < sourceStatesArray.length; i++) {"
            , "       if (s == sourceStatesArray[i])"
            , "         return true ;"
            , "     }"
            , "     return false ;"
            , "   }"
            , "    private static int sample() {"
            , "      double total = 0.0 ; "
            , "      double dice  = 0.0 ;"
            , "      int state    = 1 ;"
            , "      boolean started = false ;"
            , ""
            , "      while (!started || (total <= time_limit + 1 && (!isTargetState(state)))){"
            , "        // Fixed a nice bug here, we have to say !started"
            , "        // otherwise we reset on re-entering the source state"
            , "        // which means we only count runs which never re-enter"
            , "        // the source state, which is likely to mean we drop slower"
            , "        // runs."
            , "        if (isSourceState(state) && !started){"
            , "          started = true;"
            , "          total   = 0.0;"
            , "        }"
            , "        switch (state) {"
            ]
            ++ stateLines ++
            [ "          default: "
            , "            System.out.println (\"Serious bother; undefined state\");"
            , "            System.exit(1);"
            , "        }"
            , "        if (!started) total = 0 ;"
            , "      }// end of the while loop"
            , "      int bucket = (int) Math.ceil(total) ; // /size) ;"
            , "      return bucket ;"
            , "    }"
            , "    "
            , "   public static void main(String[] args) {"
            , "     int buckets[]    = new int[time_limit + 1] ;"
            , "     int cdfBuckets[] = new int[time_limit + 1] ;"
            , "     int outOfTime    = 0 ;"
            , "     int allDone      = 0 ;"
            , "     for (int i = 0; i < samples; i++){"
            , "        int bucket = sample () ;"
            , "        if (bucket < buckets.length)"
            , "          buckets[bucket]++ ;"
            , "        else outOfTime++ ;"
            , "     }"
            , ""
            , "     // Now we just want to print out the numbers in each bucket"
            , "     for (int i = 0; i < buckets.length; i++) {"
            , "       allDone += buckets[i] ;"
            , "       System.out.println"
            , "              (\"# buckets[\" + i + \"] chosen \" +"
            , "               buckets[i] + \" times, i.e. \" + getPercent(buckets[i], samples) +"
            , "               \"% of the time, the cdf is: \" + getPercent(allDone, samples) ) ;"
            , "     }"
            , "     System.out.println"
            , "              (\"We ran out of time: \" + outOfTime + \" i.e. \" +"
            , "                getPercent (outOfTime, samples) + \"% of the time\") ;"
            , ""
            , "    "
            , "   return ;"
            , "   }"
            , ""
            , "}"
            ]

  timeLimitName = "time_limit"
  stateLines    = concat $ map getStateLines allStates
  getStateLines :: State -> [ String ]
  getStateLines state =
    [ concat [ "          case ", show number, ":" ]
    , concat [ "          // ", States.stateReprName $ States.stateRep state ]
    , delayCommand
    ,          "            dice   = generator.nextDouble();"
    -- , concat [ "            state = ", show (number + 1), ";" ]
    , stateIfs 0.0 movements 
    ,          "            break;"             
    ]
    where
    number       = States.stateNumberInt state
    delayCommand
      | null movements = 
        concat [ "            "
               , "System.out.println (\" deadlocked state reached\") ;\n"
               , "            "
               , "total += ", timeLimitName, " ;" ]
      | otherwise      =
       concat [ "            total += expDelay(", show overallDelay, ") ;" ]
    movements    = stateMovements state
 
    -- Note that this CAN be zero since it may be a deadlocked state
    overallRate :: Double
    overallRate = sum $ map (Rates.rateNumber . States.moveRate) movements
    -- If the overallRate is zero then we have deadlocked state
    -- we should say something like 'total = time-limit' or so.
    overallDelay :: Double
    overallDelay = 1.0 / overallRate

    stateIfs :: Double -> States.StateMovements -> String
    stateIfs soFar []         = 
      unlines [ concat [ "System.out.println(\"Out of movements: soFar is: \" + "
                       , show soFar
                       , ");" 
                       ]
              , "System.exit(1);"
              ]
    stateIfs soFar (h : rest)
      | null rest = concat [ "    state = ", show targetId, ";" ]
      | otherwise =
        unlines [ concat [ "if ( dice < ", show probCond, ") {" ]
                , concat [ "    state = ", show targetId, ";" ]
                , "}"
                , concat [ "else ", stateIfs probCond rest ]
                ]
      where
      probCond   = soFar + thisProb
      thisProb   = thisRate / overallRate
      thisRate   = Rates.rateNumber $ States.moveRate h
      targetId   = States.stateNumberInt target
      target     = Maybe.fromMaybe (error "No such state") $
                   States.findStateRepr statespace targetRepr
      targetRepr = States.moveTarget h

  targetIds      = List.intercalate ", " 
                   $ map (show . States.stateIdInt) 
                   $ Set.elems targets
  sourceIds      = List.intercalate ", " 
                   $ map (show . States.stateIdInt) 
                   $ Set.elems sources

  allStates      = States.allStatesInSpace statespace


{-| The first argument is the class name -}
statespaceToSimulator :: String -> StateSpace -> String
statespaceToSimulator className statespace = 
  unlines simLines 
  where
  simLines = 
            [ "import java.util.Random;"
            , concat [ "public class ", className, " {" ]
            , ""
            , "private static Random generator = new Random();"
            , "   private static int time_limit = 10  ;"
            , "   private static int samples    = 10000 ;    "
            , ""
            , "   private static final double duration_a = 1.5 ; // seconds"
            , "   private static final double duration_b = 1.5 ; // seconds"
            , ""
            , "   // sample from the exponential distribution "
            , "   private static double expDelay(double mean) {"
            , "     return - mean * Math.log(generator.nextDouble());"
            , "   }"
            , "   private static double getPercent(int p, int t){"
            , "     return 100 * ((double) p) / ((double) t) ;"
            , "   } "
            , ""
            , ""
            , "   private static int targetState = 3 ;"
            , "   "
            , "    private static int sample() {"
            , "      double total = 0.0 ; "
            , "      double dice  = 0.0 ;"
            , "      int state    = 1 ;"
            , ""
            , "      while (total <= time_limit && state != targetState){"
            , "        switch (state) {"
            ]
            ++ stateLines ++
            [ "          case 3:"
            , "            System.out.println (\"Uhm, I'm in the target state???\");"
            , "            System.exit(1);"
            , "          default: "
            , "            System.out.println (\"Serious bother; undefined state\");"
            , "            System.exit(1);"
            , "        }"
            , "      }"
            , "      int bucket = (int) Math.ceil(total) ; // /size) ;"
            , "      return bucket ;"
            , "    }"
            , "    "
            , "   public static void main(String[] args) {"
            , "     int buckets[]    = new int[time_limit] ;"
            , "     int cdfBuckets[] = new int[time_limit] ;"
            , "     int outOfTime    = 0 ;"
            , "     int allDone      = 0 ;"
            , "     for (int i = 0; i < samples; i++){"
            , "        int bucket = sample () ;"
            , "        if (bucket <= time_limit - 1)"
            , "          buckets[bucket]++ ;"
            , "        else outOfTime++ ;"
            , "     }"
            , ""
            , "     // Now we just want to print out the numbers in each bucket"
            , "     for (int i = 0; i < time_limit; i++) {"
            , "       allDone += buckets[i] ;"
            , "       System.out.println"
            , "              (\"# buckets[\" + i + \"] chosen \" +"
            , "               buckets[i] + \" times, i.e. \" + getPercent(buckets[i], samples) +"
            , "               \"% of the time, the cdf is: \" + getPercent(allDone, samples) ) ;"
            , "     }"
            , "     System.out.println"
            , "              (\"We ran out of time: \" + outOfTime + \" i.e. \" +"
            , "                getPercent (outOfTime, samples) + \"% of the time\") ;"
            , ""
            , "    "
            , "   return ;"
            , "   }"
            , ""
            , "}"
            ]


  stateLines    = concat $ map getStateLines allStates
  getStateLines :: State -> [ String ]
  getStateLines state
    | number == 3 = []
    | otherwise  = [ concat [ "          case ", show number, ":" ]
                   , "            total += expDelay(1) ;"
                   , concat [ "            state = ", show (number + 1), ";" ]
                   , "            break;"             
                   ]
    where
    number = States.stateNumberInt state

{-
"          // State 0, P1 || P1, we go to either P2 || P1"
            , "          // or P1 || P2"
            , "          case 0:"
            , "            // roll dice to decide which way to go"
            , "            dice  = generator.nextDouble() ;"
            , "            total += expDelay(duration_a / 2) ;"
            , "            if (dice < 0.5){"
            , "              state = 1 ;"
            , "            } else {"
            , "              state = 2 ;"
            , "            }"
            , "            break ;"
            , "          // State 1, P2 || P1"
            , "          case 1:"
            , "          // State 2, P1 || P2 whose logic is the same as state 1"
            , "          case 2:"
            , "            // roll dice to decide which way to go"
            , "            dice  = generator.nextDouble() ;"
            , "            total += expDelay(1.5 / 2) ; "
            , "            if (dice < 0.5){"
            , "              state = 0 ;"
            , "            } else {"
            , "              state = 3 ;"
            , "            }"
            , "            break ;"
            , "          // State 3, P2 || P2, but this is the target state,"
            , "          // so we should never actually get here"
-}

  allStates      = States.allStatesInSpace statespace

{-
  allActions     = concat $ map stateToActions allStates
  
  
  stateToActions :: State -> [ SimAction ]
  stateToActions state =
    map translateMovement $ zip [0 ..] (stateMovements state)
    where
    sName = stateName state
    translateMovement :: (Int, StateMove) -> SimAction
    translateMovement (i, (trans, targetRepr)) =
      SimAction { simActionName         = concat [ sName
                                                 , "__"
                                                 , aName
                                                 , "__"
                                                 , show i
                                                 ]
                , simActionPreCondition = sName
                , simActionEffect       = parens $ unlines effects
                , simActionComment      = comment
                }
      where
      comment = unwords [ "We're in state: "
                        , States.stateReprName $ stateRep state ]
      aName   = Qualified.textual $ Pepa.actionNameOfTrans trans
      effects = [ parens $ concat [ "not "
                                  , parens sName
                                  ]
                , parens $ stateName target
                ]
      target   = Maybe.fromMaybe (error "No such state") $
                 States.findStateRepr statespace targetRepr
  -}  

-- stateName :: State -> String
-- stateName = ("state_" ++) . show . States.stateNumberInt
  
  
