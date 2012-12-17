{-| This module implements a typing phase over the model syntax.
    The main aim is to make sure that elements are used in a consistent manner.
    Rate identifiers are typed according to their use in rate expressions.
    Each identifier may have a particular derivative, that is it may be 
    a scalar, a rate, a rate product or higher. Each rate expression which is
    actually used as a rate, should have the derivation.

    For process definitions we wish to ensure that a parallel process is not
    used in a sequential position. Hence we do not have
    (effectively the same as) @ P.(Q <> R) @
-}
module Language.Pepa.Analysis.Typing
   ( typeModel
   , Message
   )
where

{- Standard Modules Imported -}
import Control.Arrow 
  ( second )
{- External library modules imported -}
{- Local Modules Imported -}
import Language.Pepa.Rates
  ( Rate      ( .. )
  , RateExpr  ( .. )
  )
import Language.Pepa.Syntax
   ( RateIdentifier
   , ParsedComponentId    
   , ParsedModel        ( .. )
   , RateSpec
   , ProcessDef
   , ParsedComponent    ( .. )
   , Transition         ( .. )
   , ParsedRate
   )
{- End of imports -}


typeModel :: ParsedModel -> [ Message ] -- Should be 'AnalysisMessage'
typeModel model =
   procConsOfModel model
   where
   _rateMapping = rateMappingOfModel model
   _constrants  = derivExpsOfModel model

{-
  For the first we wish to build up a set of constraints and then 
  attempt to solve them. Obviously this could potentially take a long
  time but I think that for most models this is fairly simple.

  A derivative is simply a number:
-}
-- type Derivative = Int

{- 
   A derivative expression may contain unknown derivatives which
   will in general will refer to rate identifiers.

   A 'Dany' is the derivative given to a literal number in a rate
   expression, this is because it could be any derivative.
   That is @1.0@ could be a probablilty, a rate, an acceleration
   or anything higher.
-}
data DerivExp = Dany
--              | Dint Int
              | Dname RateIdentifier
              | Dplus DerivExp DerivExp
--               | Dminus DerivExp DerivExp

{-
  A constraint then is a derivative expression which must ultimately
  'be able' to evaluate to a given value.
-}
-- type Constraint = (Derivative, DerivExp)

{-
    We need three remaining functions.
    1. Obtain the constraints from a pepa model
    2. A mapping from rate identifiers to derivative expressions
    3. A function for solving the constraints.
    
    I think that one and two could be done simultaneously.
-}
derivExpsOfModel :: ParsedModel -> [ DerivExp ]
derivExpsOfModel model =
  concatMap (derivExps . snd) pDefs
  where
  pDefs = modelProcessDefs model
  derivExps :: ParsedComponent -> [ DerivExp ]
  derivExps (StopProcess)                     =
    []
  derivExps (IdProcess _)                     = 
    []
  derivExps (PrefixComponent  trans next) = 
    (convertRate $ pepaTransRate trans) : derivExps next
  derivExps (ComponentSum left right )        =
    (derivExps left) ++ (derivExps right)
  derivExps (CondBehaviour _ comp)            =
    derivExps comp
  -- All the  remaining ones are parallel and should contain no rate expressions
  derivExps (ProcessArray _ _ _)              = 
    []
  derivExps (Cooperation _ _ _)               = 
    []
  derivExps (Hiding _ _)                      =
    []

{- converts a rate into derived expressions -}
convertRate :: ParsedRate -> DerivExp
convertRate (RateTop _)       = Dany -- should be Dint 1 or something
convertRate (RateImmediate _) = Dany -- again 
convertRate (RateTimed e )    = convertRateExp e
   
{-
  Converts rate expressions into derivative expressions
-}
convertRateExp :: RateExpr -> DerivExp
convertRateExp (Cconstant _)        = Dany
convertRateExp (Creal _)            = Dany
convertRateExp (Cident ident)       = Dname ident
convertRateExp (Cmult r1 r2)        =
    Dplus (convertRateExp r1) (convertRateExp r2)
convertRateExp _                    = 
   error "Unimplemented rate expression typing"

type RateMapping = [ (RateIdentifier, DerivExp) ]

rateMappingOfModel :: ParsedModel -> RateMapping
rateMappingOfModel model =
  map rateMapping $ modelRateSpecs model
  where
  rateMapping :: RateSpec -> (RateIdentifier, DerivExp)
  rateMapping = second convertRateExp


{-
    Now for Processes we really only need to know whether a process
    defined to be a parallel process or a sequential derivative.
    So we first build up a list of constraints, for processes these
    are; must be parallel, must be sequential, aliases ..
-}
data ProcConstraint = Parallel
                    | Sequential

type ProcMap = [ (ParsedComponentId, ProcConstraint) ]

-- | The type of a message from an analysis function
type Message = String

{-
  
-}
procConsOfModel :: ParsedModel -> [ Message ]
procConsOfModel model =
    concatMap (procCons []) pDefs
    where
    pDefs = modelProcessDefs model
    procCons :: ProcMap -> ProcessDef -> [ Message ]
    procCons _  (_ident, StopProcess)             = []
    procCons _  (_ident, IdProcess _ident2)       = []
    procCons pm (_ident, PrefixComponent _ nextP) =
        mustBeSequential pm nextP
    procCons pm (_ident, ComponentSum left right) =
        (mustBeSequential pm left) ++ (mustBeSequential pm right)
    procCons pm (_ident, CondBehaviour _ comp)    =
       mustBeSequential pm comp
    -- Note here, if we instead kept a running proc map
    -- we could add these into the procmap as being definitely
    -- parallel
    procCons _pm (_ident, Cooperation _ _ _)      = []
    procCons _pm (_ident, ProcessArray _ _ _)     = []
    procCons _pm (_ident, Hiding _ _ )            = []

    mustBeSequential :: ProcMap -> ParsedComponent -> [ Message ]
    mustBeSequential _pm (StopProcess)    =
       []
    mustBeSequential pm (IdProcess ident) =
        case lookup ident pm of
            Just Sequential -> []
            Just Parallel   -> [ "process " ++ (show ident) ++ "should be sequential" ]
            Nothing         ->
                case lookup ident pDefs of
                    Just proc -> mustBeSequential ((ident, Sequential) : pm) proc
                    -- This process is undefined, another analysis should see to that.
                    Nothing   -> []
    mustBeSequential pm (PrefixComponent _ nextP)  =
        mustBeSequential pm nextP
    mustBeSequential pm (CondBehaviour _ nextP)  =
        mustBeSequential pm nextP
    mustBeSequential pm (ComponentSum left right)  =
        (mustBeSequential pm left) ++ (mustBeSequential pm right)
    mustBeSequential _pm proc@(Cooperation _ _ _)  =
        [ "component " ++ show proc ++ "should be sequential" ]
    mustBeSequential _pm proc@(ProcessArray _ _ _) =
        [ "component " ++ show proc ++ "should be sequential" ]
    mustBeSequential _pm proc@(Hiding _ _ )        =
        [ "component " ++ show proc ++ "should be sequential" ]