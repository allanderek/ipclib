
{-|
   A suite of data types describing the abstract syntax of Hydra models.

   [@todo@] Somethings which are available in hydra but which are not
   yet incapsulated here include:
   helpvalues, custom output functions, custom hash functions and generation
   control.
-}
module Language.Hydra.Syntax
   ( DNAMmodelFile         ( .. )
   , modelOfModelFile
   , solControlOfModelFile
   , perfMeasuresOfModelFile
   , processNamesOfModel
   , DNAMmodel             ( .. )

   , DNAMidentifier

   , DNAMheader            ( .. )

   , DNAMconstantDef       ( .. )

     -- * Initial states
   , DNAMstateVector
   , DNAMstateDesc         ( .. )

   , DNAMinitialVector
   , DNAMinitialAssign

     -- * Model transitions
   , DNAMmodelTransitions
   , DNAMtransition        ( .. )
   , isImmediateTransition
   , DNAMcondition         ( .. )
   , cExpOfDNAMcond
   , DNAMaction
   , DNAMspeed             ( .. )
   , isImmediateSpeed
   , DNAMpriority
   , defaultPriority

   , DNAMmodelInvariant    ( .. )

     -- * Solution Control
   , DNAMsolutionControl   ( .. )
   , DNAMsolutionMethod    ( .. )
   , parseSolutionMethod
   , defaultSolutionMethod

     -- * Measurement specification
   , DNAMperformMeasure    ( .. )
   , DNAMpassageMeasure    ( .. )
   , DNAMtransientMeasure  ( .. )
   , DNAMsteadyMeasure     ( .. )
   , DNAMcountMeasure      ( .. )
   , DNAMsteadyEstimator   ( .. )

     -- * DNAM c constructs.
   , DNAMcassignment       ( .. )
   , DNAMctype             ( .. )
   , DNAMcexp
   , dnamCtrue
   , dnamCfalse
   , dnamMinimum
   , namesOfCexp
   )
where

{- Imported Standard Libraries -}
import Data.List
  ( union )
{- Imported Local Libraries -}
import Language.Pepa.QualifiedName
  ( QualifiedName )
import Language.Pepa.Rates
  ( RateExpr      ( .. ) ) 
{- End of Imports -}

{-| The type of a DNAM model file. This contains more information than
    simple the model as there may be solution control methods for example.
-}
data DNAMmodelFile a b = 
   DNAMmodelFile (DNAMmodel a b) DNAMsolutionControl [ DNAMperformMeasure ] 

{-| Returns the model underlying a model file -}
modelOfModelFile :: DNAMmodelFile a b -> DNAMmodel a b
modelOfModelFile (DNAMmodelFile model _ _) = model

{-| Returns the solution control of the dnam model file -}
solControlOfModelFile :: DNAMmodelFile a b -> DNAMsolutionControl
solControlOfModelFile (DNAMmodelFile _ solCon _) = solCon

{- Returns the performance measures of the dnam model file -}
perfMeasuresOfModelFile :: DNAMmodelFile a b -> [ DNAMperformMeasure ]
perfMeasuresOfModelFile (DNAMmodelFile _ _ pMeasures) = pMeasures

{- | Returns the state names within a hydra model -}
processNamesOfModel :: DNAMmodel a b -> [ DNAMidentifier ]
processNamesOfModel model = 
  [ ident | DNAMstateDesc _ ident <- modelStateVector model ]

{-| The type of a DNAM model -}
data DNAMmodel a b = DNAMmodel { modelHeaders     :: [ DNAMheader ]
                               , modelStateVector :: DNAMstateVector
                               , modelConstants   :: [ DNAMconstantDef ]
                               , modelInitial     :: DNAMinitialVector
                               , modelTransitions :: DNAMmodelTransitions a b
                               , modelInvariants  :: [ DNAMmodelInvariant ]
                               }

{- 
   This is not a great representation of this, it should include some
   representation of C++ statements, class definitions pre-processor
   commands. I think that for this library that is probably a little too
   much effort however if we were to use an external C library (for example
   there is one that is somehow associated with gtk2hs/c2hs) then it would
   become viable.
-}
{-| The type of a dnam header -}
data DNAMheader = DNAMheader String

{-| The type of a state vector -}
type DNAMstateVector = [ DNAMstateDesc ]

{-| The type of a state in a dnam model -}
data DNAMstateDesc = DNAMstateDesc DNAMctype DNAMidentifier

{-| The type of identifiers in a dnam model -}
type DNAMidentifier = QualifiedName

{-| The type of constants in the header of the model file -}
data DNAMconstantDef = DNAMconstantDef DNAMidentifier DNAMcexp

{-| The initial state that we are in, so basically initialise all variables -}
type DNAMinitialVector = [ DNAMinitialAssign ]

{-| The type of a C assignment used in the initial state assignments -}
type DNAMinitialAssign = DNAMcassignment


{- Now follow the data type declarations for dnam transition declarations -}
{-| The transitions of a dnam model -}
type DNAMmodelTransitions a b = [ DNAMtransition a b ]

{-| A single transition in a dnam model -}
data DNAMtransition a b = 
   DNAMtrans { transName       :: QualifiedName
             , transActionKind :: a
             , transConditions :: [ DNAMcondition ]
             , transActions    :: [ DNAMaction ]
             , transSpeed      :: b
             , transPriority   :: DNAMpriority
             }

{-| Returns true if the given transition is an immediate transition
    without a rate (but instead a weight)
-}
isImmediateTransition :: DNAMtransition a DNAMspeed -> Bool
isImmediateTransition = isImmediateSpeed . transSpeed


{-| The type of a condition against a transition in a dnam model -}
data DNAMcondition = DNAMcond DNAMcexp -- must be a boolean C expression
                   | DNAMprocpresent DNAMidentifier

{-| Translate a condition into a dnam C expression -}
cExpOfDNAMcond :: DNAMcondition -> DNAMcexp
cExpOfDNAMcond (DNAMcond e)            = e
cExpOfDNAMcond (DNAMprocpresent ident) =
   Cgt (Cident ident) (Cconstant 0)

{-| The type of an action to be taken within a DNAM model transition -}
type DNAMaction = DNAMcassignment

{-| The type of a priority within a DNAM model transition -}
type DNAMpriority = Int

{-| The default priority -}
defaultPriority :: DNAMpriority
defaultPriority = 1

{-| The type of a transition speed, essentially this will either be a rate
    for timed activities or a weight for immediate actions.
-}
data DNAMspeed = DNAMrate   DNAMcexp
               | DNAMweight DNAMcexp

{-| Returns whether or not the give speed is immediate -}
isImmediateSpeed :: DNAMspeed -> Bool
isImmediateSpeed (DNAMrate _)   = False
isImmediateSpeed (DNAMweight _) = True

{-| The type of an invariant in a dnam model -}
data DNAMmodelInvariant = DNAMinvariant DNAMcexp


{---------------------------------------------------------------------
  The solution control aspects of dnam model files, these should be
  able to describe the following grammar:

solution_control = \solution {
  { \method{gauss | grassman | gauss_seidel | sor | bicg | cgnr |
        bicgstab | bicgstab2 | cgs | tfqmr | ai | air | automatic} |
    \accuracy{<real>} |
    \maxiterations{<long int>} |
    \relaxparameter{<real> | dynamic} |
    \startvector{ <filename> } |
    \reportstyle{full | short | none} |
    \reportinterval{<long int>}
  }*
}
---------------------------------------------------------------------}
{-| The type of a solution control within a hydra model file.
   Currently missing:
   accuracy, maxiterations, relaxparameter, startvector, reportstyle
   and reportinterval.
-}
data DNAMsolutionControl = 
   DNAMsolution { solutionMethod :: DNAMsolutionMethod }

{-| The type of a hydra solution method -}
data DNAMsolutionMethod =
   DNAMgauss   
 | DNAMgrassman 
 | DNAMgaussSeidel 
 | DNAMsor 
 | DNAMbicg 
 | DNAMcgnr 
 | DNAMbicgstab 
 | DNAMbicgstab2 
 | DNAMcgs 
 | DNAMtfqmr 
 | DNAMai 
 | DNAMair 
 | DNAMautomatic


parseSolutionMethod :: String -> DNAMsolutionMethod
parseSolutionMethod "gauss"        = DNAMgauss
parseSolutionMethod "grassman"     = DNAMgrassman
parseSolutionMethod "gauss_seidel" = DNAMgaussSeidel
parseSolutionMethod "sor"          = DNAMsor
parseSolutionMethod "bicg"         = DNAMbicg
parseSolutionMethod "cgnr"         = DNAMcgnr
parseSolutionMethod "bicgstab"     = DNAMbicgstab
parseSolutionMethod "bicgstab2"    = DNAMbicgstab2      
parseSolutionMethod "cgs"          = DNAMcgs
parseSolutionMethod "tfqmr"        = DNAMtfqmr
parseSolutionMethod "ai"           = DNAMai
parseSolutionMethod "air"          = DNAMair
parseSolutionMethod "automatic"    = DNAMautomatic
parseSolutionMethod _              = error "Invalid solution method"

defaultSolutionMethod :: DNAMsolutionMethod
defaultSolutionMethod = DNAMautomatic

{-
   A performance measurement can either be a steady-state,
   transient or passage-time measurement.
-}
data DNAMperformMeasure =
   -- A DNAMpassage is the result of an observation probe and hence
   -- requires that we are careful to add a probe-tracker (to correctly
   -- identify the probe start states as opposed to the probe running states)
   -- while DNAMpassageCond has resulted from directly stating the source and
   -- target conditions so we needn't add a probe-tracker.
   -- (for probe-trackers see Language.Pepa.Compile.Hydra.addProbeTracker)
   DNAMpassage   DNAMpassageMeasure
 | DNAMpassageCond DNAMpassageMeasure
 | DNAMtransient DNAMtransientMeasure
 | DNAMsteady    DNAMsteadyMeasure
 | DNAMcount     DNAMcountMeasure

{---------------------------------------------------------
 A passage-time measurement is specified using the
 following grammar.
\passage{
        \sourcecondition{<boolean expression>}
        \targetcondition{<boolean expression>}
        \t_start{<real number>}
        \t_stop{<real number>}
        \t_step{<real number>}
}

---------------------------------------------------------------------}
data DNAMpassageMeasure =
   DNAMpassageMeasure { passageSourceCond :: DNAMcondition
                      , passageTargetCond :: DNAMcondition
                      , passageStartTime  :: Double
                      , passageStopTime   :: Double
                      , passageStepTime   :: Double
                      }

{--------------------------------------------------------------------
A transient analysis measurement is specified as:

\transient{
        \sourcecondition{<boolean expression>}
        \targetcondition{<boolean expression>}
        \t_start{<real number>}
        \t_stop{<real number>}
        \t_step{<real number>}
}
---------------------------------------------------------------------}


data DNAMtransientMeasure =
   DNAMtransientMeasure { transientSourceCond :: DNAMcondition
                        , transientTargetCond :: DNAMcondition
                        , transientStartTime  :: Double
                        , transientStopTime   :: Double
                        , transientStepTime   :: Double
                        }

{--------------------------------------------------------------------
 A steady state measure is specified as:
\performance{
   \statemeasure{my_measure}
   { \estimator { mean }
     \expression{ _CliProbe_run_0_1 > 0 }
   }
}
---------------------------------------------------------------------}

data DNAMsteadyMeasure =
   DNAMsteadyMeasure { steadyMeasureName :: DNAMidentifier
                     , steadyEstimator   :: DNAMsteadyEstimator
                     , steadyCondition   :: DNAMcondition
                     }

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
data DNAMcountMeasure =
   DNAMcountMeasure { countMeasureName  :: DNAMidentifier
                    , countTransitions :: [ DNAMidentifier ] }




data DNAMsteadyEstimator =
   EstimatorMean
 | EstimatorVariance
 | EstimatorStdDeviation
 | EstimatorDistribution
                        


{---------------------------------------------------------------------
  The C constructs that are used in dnam model files.
---------------------------------------------------------------------}
{-| The C types allowed within a state description -}
data DNAMctype = CInt | CShort | CChar | CLong
                 deriving Eq


{-| The type of a C assignment, used in both the initial state assignments
    and in the transitions
-}
data DNAMcassignment = DNAMcassignment DNAMidentifier DNAMcexp
                     | DNAMidentIncr DNAMidentifier
                     | DNAMidentDecr DNAMidentifier


{-| The type of a rate expression in a DNAM mod file is the same
    as that in a PEPA file.
-}
type DNAMcexp = RateExpr



{-| The c expression for 'True' -}
dnamCtrue :: DNAMcexp
dnamCtrue = Cconstant 1

{-| The c expression for 'False' -}
dnamCfalse :: DNAMcexp
dnamCfalse = Cconstant 0

{-| The c expression to take the minimum of two sub-expressions -}
dnamMinimum :: DNAMcexp -> DNAMcexp -> DNAMcexp
dnamMinimum = Cminimum -- e1 e2 = Cifte (Clt e1 e2) e1 e2




{-| The names used within a Cexp -}
namesOfCexp :: DNAMcexp -> [ DNAMidentifier ]
namesOfCexp (Cident ident)   = [ ident ]
namesOfCexp (Cconstant _)    = []
namesOfCexp (Creal _)        = []
namesOfCexp (Cadd  e1 e2)    = union (namesOfCexp e1) (namesOfCexp e2)
namesOfCexp (Csub  e1 e2)    = union (namesOfCexp e1) (namesOfCexp e2)
namesOfCexp (Cmult e1 e2)    = union (namesOfCexp e1) (namesOfCexp e2)
namesOfCexp (Cdiv  e1 e2)    = union (namesOfCexp e1) (namesOfCexp e2)
namesOfCexp (Cifte e1 e2 e3) = union (namesOfCexp e1) $
                               union (namesOfCexp e2) (namesOfCexp e3)
namesOfCexp (Cand  e1 e2)    = union (namesOfCexp e1) (namesOfCexp e2)
namesOfCexp (Cor   e1 e2)    = union (namesOfCexp e1) (namesOfCexp e2)
namesOfCexp (Cgt   e1 e2)    = union (namesOfCexp e1) (namesOfCexp e2)
namesOfCexp (Cge   e1 e2)    = union (namesOfCexp e1) (namesOfCexp e2)
namesOfCexp (Clt   e1 e2)    = union (namesOfCexp e1) (namesOfCexp e2)
namesOfCexp (Cle   e1 e2)    = union (namesOfCexp e1) (namesOfCexp e2)
namesOfCexp (Ceq   e1 e2)    = union (namesOfCexp e1) (namesOfCexp e2)
namesOfCexp (Cnot  e)        = namesOfCexp e
namesOfCexp (Cminimum e1 e2) = union (namesOfCexp e1) (namesOfCexp e2)
                                   