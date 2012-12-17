{-|
   This module is intended to be a formatter for hydra models.
   I don't think these necessarily have to be particularly pretty.
   They should be human readable, but not precisely so. So in other
   words they should be human readable but not necessarily human
   maintainable. It is tempting to split this into two modules,
   one for simply outputting for hydra to read and the other for
   pretty printing for a human to read. If the formatting ever
   becomes a burden that is what I will do. That is this will
   become strictly a pretty printer and another module
   @Language.Hydra.Format@ will format it such that hydra can read
   it.
-}
module Language.Hydra.Print
   ( printDnamModelFile
   , printDnamModel
   , printPerformMeasure
   , printHydraSolutionControl
   , DNAMspeedTarget      ( .. )

   , printSpeed
   , printCondition
   , printDNAMaction
   , printCassign
   , printCexp
   )
where


{- Imported Standard Libraries -}
import Numeric
   ( showFloat )
import qualified Data.List as List
{- Imported Local Libraries -}
import Language.Pepa.QualifiedName
  ( hprintQualifiedName )
import Language.Pepa.Rates
   ( Rate        ( .. )
   , RateExpr    ( .. )
   )
import Language.Pepa.Print
  ( hprintRateExpr )
import Language.Hydra.Syntax
   ( DNAMmodelFile         ( .. )
   , DNAMmodel             ( .. )
  
   , DNAMheader            ( .. )

   , DNAMconstantDef       ( .. )

   , DNAMstateVector
   , DNAMstateDesc         ( .. )

   , DNAMinitialVector
   , DNAMinitialAssign

   , DNAMmodelTransitions
   , DNAMtransition        ( .. )
   , DNAMcondition         ( .. )
   , DNAMaction
   , DNAMspeed             ( .. )
   , DNAMpriority

   , DNAMmodelInvariant    ( .. )

   , DNAMsolutionControl   ( .. )
   , DNAMsolutionMethod    ( .. )

   , DNAMperformMeasure    ( .. )
   , DNAMpassageMeasure    ( .. )
   , DNAMtransientMeasure  ( .. )
   , DNAMsteadyMeasure     ( .. )
   , DNAMcountMeasure      ( .. )
   , DNAMsteadyEstimator   ( .. )


   , DNAMcassignment       ( .. )
   , DNAMcexp
   , DNAMctype             ( .. )
   )
{- End of Imports -}

{-|
   A DNAM model is parameterised by the rates of the transitions
   so we need a class to print those out. It must be able to print
   out either a rate or a weight. Which is why 'HumanPrint' or 'Show'
   is not good enough. The easiest way to do this is to just translate
   it to a 'DNAMspeed'. Note that if the type in question already has
   quite a good printer then you can use 'CFreeForm' for the expression.
-}
class DNAMspeedTarget a where
   toDnamSpeed :: a -> DNAMspeed

instance DNAMspeedTarget DNAMspeed where
   toDnamSpeed = id

-- instance DNAMspeedTarget () where
--   toDnamSpeed () = DNAMrate $ Creal 0.0



{-
   Obviously we would need to translate the real rate into a c expression.
instance DNAMspeedTarget (Rate a) where
   toDnamSpeed (RateImmediate e) = DNAMweight e
   toDnamSpeed (RateTop _)       =
      error "Some action is performed passively without synchronisation"
   toDnamSpeed (RateTimed _)     =
      DNAMrate $ error "this doesn't get called like this does it?"
-}


{-|
   Print out a dnam model file include the solutions controls
   and measurement specifications.
-}
printDnamModelFile :: DNAMspeedTarget b => 
                      (a -> String) -> String -> DNAMmodelFile a b -> String
printDnamModelFile printActionKind header 
                   (DNAMmodelFile model solControl pMeasures) =
   unlines $ ( printDnamModel printActionKind header model ) :
             ( printHydraSolutionControl solControl) :
             performMeasures
   where
   performMeasures = case pMeasures of
                       []  -> [ "%% Oh no performance measures, unusual" ]
                       _   -> map printPerformMeasure pMeasures


{-|
   Print out a dnam model in a format that Hydra can read.
   The initial string given is a string which will be commented out
   and attached at the head of the file. It is intended to be something
   such as a message saying how the model was generated.
-}
printDnamModel :: DNAMspeedTarget b => 
                  (a -> String) -> String -> DNAMmodel a b -> String
printDnamModel printActionKind header dModel =
   unlines [ hydraComment header
           , "\\model{"
           , printHeaders       $ modelHeaders     dModel
           , printConstantDefs  $ modelConstants   dModel
           , printStateVector   $ modelStateVector dModel
           , printInitialVector $ modelInitial     dModel
           , printTransitions printActionKind
                                $ modelTransitions dModel
           , printInvariants    $ modelInvariants  dModel
           , "}"
           ]

printHeaders :: [ DNAMheader ] -> String
printHeaders = unlines . (map printDNAMheader)

printDNAMheader :: DNAMheader -> String
printDNAMheader (DNAMheader s) =
   unlines [ "\\header{"
           , "inline double "
             ++ ipcMinimumString
             ++ " (double e, double l){ double a = e, b = l; return (a < b ? a : b); }"
           , "inline double "
             ++ ipcApparentRateString
             ++ " (double r1, double r2, double raP, double raQ)\n"
             ++ "   { return (r1/raP) * (r2/raQ) * (" 
                      ++ ipcMinimumString ++ "(raP, raQ)) ; }"
             -- Again note the difference when combining an active and a passive
             -- rate we do not have to apply the minimum function since the
             -- second rate is assumed to be infinty.
           , "inline double "
             ++ ipcActivePassiveFun
             ++ " (double r1, double r2, double raP, double raQ)\n"
             ++ "   { return (r1/raP) * (r2/raQ) * raP ; }" 
           , s
           , "}"
           ]

ipcMinimumString :: String
ipcMinimumString = "ipc_min"

ipcApparentRateString :: String
ipcApparentRateString = "ipc_apparent"

ipcActivePassiveFun :: String
ipcActivePassiveFun = "ipc_active_passive"

{-
   Printing out the constant definitions
-}
printConstantDefs :: [ DNAMconstantDef ] -> String
printConstantDefs = unlines . (map printConstantDef)

printConstantDef :: DNAMconstantDef -> String
printConstantDef (DNAMconstantDef ident cexp) =
   concat [ "\\constant{ "
          , hprintQualifiedName ident
          , " }{ "
          , printCexp cexp
          , "}"
          ] 

{- 
   Here we just print them all out one at a time, 
   but if we wished to we could separate them out into
   the different types and then have one type definition
   for each of the same kind (which  would often mean
   them all going in the same declaration).
-}
printStateVector :: DNAMstateVector -> String
printStateVector sVector =
   unlines [ "\\statevector{"
           , unlines $ map printStateDescGroup typeGroups
           , "}"
           ]
   where
   -- Group the type declarations by their type, in ipc there will
   -- be one group.
   typeGroups = List.groupBy sameType sVector
   sameType :: DNAMstateDesc -> DNAMstateDesc -> Bool
   sameType (DNAMstateDesc ctype1 _) (DNAMstateDesc ctype2 _) = 
      ctype1 == ctype2

   printStateDescGroup :: DNAMstateVector -> String
   printStateDescGroup []                                      = []
   printStateDescGroup tGroup@((DNAMstateDesc ctype _) : _)    =
      unwords [ "\\type{"
              , printCtype ctype
              , "}{"
              , List.intercalate "\n , " idents
              , "}"
              ] 
      where
      idents = map identOfStateDesc tGroup
   
      -- Okay okay so this also prints the ident rather than returning it
      identOfStateDesc :: DNAMstateDesc -> String
      identOfStateDesc (DNAMstateDesc _ ident) = hprintQualifiedName ident


{-
   We do not require this anymore since we group them before printing them.
printStateDesc :: DNAMstateDesc -> String
printStateDesc (DNAMstateDesc ctype ident) =
   unwords [ "\\type{"
           , printCtype ctype
           , "}{"
           , hprintQualifiedName ident
           , "}"
           ]
-}

printInitialVector :: DNAMinitialVector -> String
printInitialVector iVector =
   unlines [ "\\initial{"
           , unlines $ map printInitAssign iVector
           , "}"
           ]

printInitAssign :: DNAMinitialAssign -> String
printInitAssign = printCassign

{- The printing of transitions -}
printTransitions :: DNAMspeedTarget b =>
                   (a -> String) -> DNAMmodelTransitions a b -> String
printTransitions printA = 
  unlines . (map $ printTransition printA)

{-
   Notice we are using 'HumanPrint' here, so if we do decide to split the
   formatter into a human-readable and a machine-parsable then this would,
   for the machine-parsable one be changed to a different class.
-}
printTransition :: DNAMspeedTarget b =>
                   (a -> String) -> DNAMtransition a b -> String
printTransition printActionKind trans =
   unlines [ header
           , "{"
           , hydraComment    $ printActionKind (transActionKind trans)
           , printConditions $ transConditions trans
           , printActions    $ transActions    trans
           , printSpeed speed
           , printPriority   $ transPriority   trans
           , "}"
           ]
   where
   header = unwords [ "\\transition{"
                    , hprintQualifiedName $ transName trans
                    , "}"
                    ]
   speed      = toDnamSpeed $ transSpeed trans

printConditions :: [ DNAMcondition ] -> String
printConditions conds = 
   unwords [ "\\condition{"
           , List.intercalate " && " $ map printCondition conds
           , "}"
           ]


printCondition :: DNAMcondition -> String
printCondition (DNAMcond cexp)         = printCexp cexp
printCondition (DNAMprocpresent ident) =
   (hprintQualifiedName ident) ++ " > 0"


printActions :: [ DNAMaction ] -> String
printActions assignments = 
   unlines [ "\\action{"
           , unlines $ map printDNAMaction assignments
           , "}"
           ]

{-
   Here, it's a little ugly to add 'next ->' on to the front of all
   the action assignments. I guess it should really be that the
   assignments do have as their representation something like
   @CStruct CDerefence @
-}
printDNAMaction :: DNAMaction -> String
printDNAMaction = ("next -> " ++ ) . printCassign

printSpeed :: DNAMspeed -> String
printSpeed (DNAMrate   cexp) = concat [ "\\rate{ "  , printCexp cexp, " }" ]
printSpeed (DNAMweight cexp) = concat [ "\\weight{ ", printCexp cexp, " }" ]

printPriority :: DNAMpriority -> String
printPriority i = concat [ "\\priority{", show i, "}" ]


printInvariants :: [ DNAMmodelInvariant ] -> String
printInvariants = unlines . (map printInvariant)

printInvariant :: DNAMmodelInvariant -> String
printInvariant (DNAMinvariant cexp) = 
   concat[ "\\invariant{ ", (printCexp cexp), " }" ]

-- ---------------------------------------------------------
{-| The printing of hydra solution controls -}
printHydraSolutionControl :: DNAMsolutionControl -> String
printHydraSolutionControl control =
   unlines [ "\\solution{ "
           , printMethod $ solutionMethod control
           , "}"
           ]
   where
   printMethod :: DNAMsolutionMethod -> String
   printMethod = (hydraCommandWrap "method") . printSolutionMethod

printSolutionMethod :: DNAMsolutionMethod -> String
printSolutionMethod DNAMgauss         = "gauss"
printSolutionMethod DNAMgrassman      = "grassman"
printSolutionMethod DNAMgaussSeidel   = "gauss_seidel"
printSolutionMethod DNAMsor           = "sor"
printSolutionMethod DNAMbicg          = "bicg"
printSolutionMethod DNAMcgnr          = "cgnr"
printSolutionMethod DNAMbicgstab      = "bicgstab"
printSolutionMethod DNAMbicgstab2     = "bicgstab2"
printSolutionMethod DNAMcgs           = "cgs"
printSolutionMethod DNAMtfqmr         = "tfqmr"
printSolutionMethod DNAMai            = "ai"
printSolutionMethod DNAMair           = "air"
printSolutionMethod DNAMautomatic     = "automatic"



{- Printing performance measurement results -}
printPerformMeasure :: DNAMperformMeasure -> String
printPerformMeasure (DNAMpassage pMeasure)     =
   printPassageMeasure pMeasure
printPerformMeasure (DNAMpassageCond pMeasure) =
   printPassageMeasure pMeasure
printPerformMeasure (DNAMtransient tMeasure)   =
   printTransientMeasure tMeasure
printPerformMeasure (DNAMsteady sMeasure)      =
   printSteadyMeasure sMeasure
printPerformMeasure (DNAMcount cMeasure)       =
   printCountMeasure cMeasure


{- Printing the passage measurements, they should look like this:

\passage{
        \sourcecondition{<boolean expression>}
        \targetcondition{<boolean expression>}
        \t_start{<real number>}
        \t_stop{<real number>}
        \t_step{<real number>}
}
-}

printPassageMeasure :: DNAMpassageMeasure -> String
printPassageMeasure pMeasure =
   unlines [ "\\passage{"
           , hydraCommandWrap "sourcecondition" sCond
           , hydraCommandWrap "targetcondition" tCond
           , hydraCommandWrap "t_start" $ showFloat tStart ""
           , hydraCommandWrap "t_stop"  $ showFloat tStop  ""
           , hydraCommandWrap "t_step"  $ showFloat tStep  ""
           , "}"
           ]
   where
   sCond  = printCondition $ passageSourceCond pMeasure
   tCond  = printCondition $ passageTargetCond pMeasure
   tStart = passageStartTime pMeasure
   tStop  = passageStopTime  pMeasure
   tStep  = passageStepTime  pMeasure


{- transient measures are the same except with the @\transient@
   hydra command.
-}
printTransientMeasure :: DNAMtransientMeasure -> String
printTransientMeasure pMeasure =
   unlines [ "\\transient{"
           , hydraCommandWrap "sourcecondition" sCond
           , hydraCommandWrap "targetcondition" tCond
           , hydraCommandWrap "t_start" $ showFloat tStart ""
           , hydraCommandWrap "t_stop"  $ showFloat tStop  ""
           , hydraCommandWrap "t_step"  $ showFloat tStep  ""
           , "}"
           ]
   where
   sCond  = printCondition $ transientSourceCond pMeasure
   tCond  = printCondition $ transientTargetCond pMeasure
   tStart = transientStartTime pMeasure
   tStop  = transientStopTime  pMeasure
   tStep  = transientStepTime  pMeasure


{--------------------------------------------------------------------
 Printint a steady state measure as:
\performance{
   \statemeasure{my_measure}
   { \estimator { mean }
     \expression{ _CliProbe_run_0_1 > 0 }
   }
}
---------------------------------------------------------------------}

printSteadyMeasure :: DNAMsteadyMeasure -> String
printSteadyMeasure sMeasure =
   unlines [ "\\performance{"
           , hydraCommandWrap "statemeasure" name
           , "{"
           , hydraCommandWrap "estimator" estimator
           , hydraCommandWrap "expression" sCond
           , "}"
           , "}"
           ]
   where
   name      = hprintQualifiedName $ steadyMeasureName sMeasure
   sCond     = printCondition $ steadyCondition sMeasure
   estimator = printSteadyEstimator $ steadyEstimator sMeasure

   printSteadyEstimator :: DNAMsteadyEstimator -> String
   printSteadyEstimator EstimatorMean         = "mean"
   printSteadyEstimator EstimatorVariance     = "variance"
   printSteadyEstimator EstimatorStdDeviation = "stddev"
   printSteadyEstimator EstimatorDistribution = "distribution"


{----------------------------------------------------------------------
 Finally a count measure of the form
 \performance{
   \countmeasure{my_measure}
   { \estimator { mean }
     \precondition{ 1 }
     \postcondition{ 1 }
     \transition{ P4___P1_0_0, P3___P4_0_0 }
   }
}
Currently there seems to be a bug in hydra such that post conditions
are not evaluated correctly, so for now we set both the pre and post
conditions to be false and measure just specific transitions.
---------------------------------------------------------------------}
printCountMeasure :: DNAMcountMeasure -> String
printCountMeasure cMeasure =
   unlines [ "\\performance{"
           , hydraCommandWrap "countmeasure" name
           , "{"
           , hydraCommandWrap "estimator" "mean"
           , hydraCommandWrap "precondition" "1"
           , hydraCommandWrap "postcondition" "1"
           , hydraCommandWrap "transition" transitions
           , "}"
           , "}"
           ]
   where
   name        = hprintQualifiedName $ countMeasureName cMeasure
   transitions = List.intercalate ", " $ 
                 map hprintQualifiedName (countTransitions cMeasure)

-- ----------------------------------------------------------
{- The printing of the C stuff now, beginning with types -}
printCtype :: DNAMctype -> String
printCtype CInt   = "int"
printCtype CShort = "short"
printCtype CChar  = "char"
printCtype CLong  = "long"

{- The printing of C expressions -}
printCexp :: DNAMcexp -> String
printCexp = hprintRateExpr


printCassign :: DNAMcassignment -> String
printCassign (DNAMcassignment ident cexp) =
   unwords [ hprintQualifiedName ident
           , "="
           , printCexp cexp
           , ";"
           ]
printCassign (DNAMidentDecr ident)        =
   unwords [ hprintQualifiedName ident
           , "="
           , hprintQualifiedName ident
           , " - 1 ;"
           ]
printCassign (DNAMidentIncr ident)        =
   unwords [ hprintQualifiedName ident
           , "="
           , hprintQualifiedName ident
           , " + 1 ;"
           ]


-- ------------------------------------------------------------------
{- Hydra printing utilities -}
hydraComment :: String -> String
hydraComment = unlines . (map ("% " ++)) . lines

hydraCommandWrap :: String -> String -> String
hydraCommandWrap command arg = concat [ "\\", command, "{ ", arg, " }" ]
