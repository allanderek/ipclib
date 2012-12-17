{-|
  A module which implements the translation from pepa models to timed
  system equations.
  The model should have already been transformed into canonical form,
  and in particular sequential derivatives should all be named.
  Although actually maybe I can get rid of the whole process
  concentrations database by simply not removing the process arrays
  before we get here? Ach actually no I don't think we can.
-}
module Language.Pepa.Ode.TimedSystemEquation
  ( TimedSystemEqn      ( .. )
  , TimedSystem
  , ConcentrationDB
  , toTimedSystem
  , apparentTSErate
  , formatTimedSystem
  , formatTimedEquation
  , NVFcomp             ( .. )
  , getNvfIdentifier
  , formatNumericalForm
  , TSEcptExpression    ( .. )
  , formatTseCompExpr
  )
where

{- Standard Library Modules Imported -}
import Prelude hiding       
  ( lookup )
import Data.List
  ( union
  , nub
  , partition
  )
import Data.Map
  ( Map
  , empty
  , lookup
  )
import Data.Maybe
  ( fromMaybe )
{- Non-Standard Library Modules Imported -}
{- Local Library Modules Imported -}
import Language.Pepa.Utils
  ( mkCSlist )
import Language.Pepa.Print
  ( HumanPrint         ( .. ) )
import Language.Pepa.Syntax
  ( ParsedModel        ( .. )
  , ParsedComponentId
  , ParsedComponent    ( .. )
  , ParsedAction
  , ParsedRate
  , RateSpec
  )
import Language.Pepa.PepaUtils
  ( isPepaActionEnabled )
import Language.Pepa.Analysis.Analysis
  ( possibleSuccessors )
import Language.Pepa.ApparentRates 
  ( summedPepaRate )
{- End of Module Imports -}

type TimedSystem = ([ RateSpec ], ConcentrationDB, TimedSystemEqn)

{-| The data type for the main system equation of a pepa model translated
    into a timed system equation.
-}
data TimedSystemEqn = 
    TSECooperation TimedSystemEqn [ ParsedAction ] TimedSystemEqn
  | TSEHide TimedSystemEqn [ ParsedAction ]
  | TSENumVerForm [ NVFcomp ] [ ParsedAction ]

{-| The numerical vector form of a component is used to count the number
    of the sequential derivatives of the process @S@ in the cooperation
    @S <L> S <L> S <L> S ... <L> S <L> S@
-}
data NVFcomp = NVFcomp CompCount ParsedComponentId

type CompCount = Int

data TSEcptExpression = 
      TSEmin TSEcptExpression TSEcptExpression
    | TSEsum TSEcptExpression TSEcptExpression
    | TSEproduct TSEcptExpression TSEcptExpression
    | TSEatom ParsedComponentId ParsedRate
    | TSEcpt ParsedComponentId -- used when simplifying min(c1*r1, c2*r2)
    | TSErate ParsedRate
    | TSEind TSEcptIndicator 
    | TSEzero


-- Indicator function on cs list of component types I((\sum_{c\in cs} c) > 0)
-- TSEindC is the complement of the indicator function
type TSEiInvIndElem = ( ParsedComponentId, ParsedRate )


-- Not exactly sure what this is for?
data TSEcptIndicator = 
      TSEiInd        [ ParsedComponentId ] 
    | TSEiIndC       [ ParsedComponentId ]  
    | TSEiInverseInd [ TSEiInvIndElem ]
    deriving Show

instance Show TSEcptExpression where
    show (TSEmin t1 t2) = concat ["min", " (", show t1, ", ", show t2, ")"]
    show (TSEsum t1 t2) = concat [show t1, " + ", show t2]
    show (TSEproduct t1 t2) = concat [show t1, " * ", show t2]
    show (TSEatom c r) = (show c)++"*"++(show r) 
    show (TSEcpt c) = (show c)
    show (TSErate r) = show r
    show (TSEind i) = "I( "++(show i)++" )"
    show TSEzero = "TSEzero"


instance Eq TSEcptExpression where
    (==) TSEzero TSEzero = True
    (==) _ _ = False

instance Ord TSEcptExpression where
    min = TSEmin

instance Num TSEcptExpression where
    (+)           = TSEsum
    (*)           = TSEproduct
    signum _      = error "TSEcptExpression.signum: not defined"
    abs _         = error "TSEcptExpression.abs: not defined"
    fromInteger 0 = TSEzero
    fromInteger _ = error "TSEcptExpression.fromInteger: only defined for 0"



{-| A bit of a temporary 'toTimedSystem' obviously this should return
    something more sensible for the rate specifications and for
    the concentration data base.
-}
toTimedSystem :: ParsedModel -> TimedSystem
toTimedSystem model@(ParsedModel rDefs _pDefs mainComp) =
    ( rDefs
    , concentrations
    , translateComposition mainComp model concentrations
    )
    where
    concentrations = empty


{-| translates a composition into a timed system equation -}
-- A small point here, we should really check if it is a simple
-- cooperation between the same two components ie @ P <L> P @
-- and in such a case we can return a 'NVFcomp'
translateComposition :: ParsedComponent -> ParsedModel -> ConcentrationDB
                     -> TimedSystemEqn
translateComposition (IdProcess ident)                      model  cDb =
    translateComponent ident model cDb
translateComposition (Cooperation left actions right)       model  cDb =
    TSECooperation tLeft actions tRight
    where
    tLeft  = translateComposition left  model cDb
    tRight = translateComposition right model cDb

translateComposition (ProcessArray _ident _size _mActions) _model _cDb =
    error "Not yet implemented process arrays in TimedSystemEquations"
translateComposition (Hiding _ident _actions)              _model _cDb =
    error "Not yet implemented hiding in TimedSystemEquations"
translateComposition (ComponentSum _ _)                    _model _cDb =
    error "There should be no component sums in the main composition"
translateComposition (PrefixComponent _ _)                 _model _cDb =
    error "There should be no prefix components in the main composition"


{-| translates a component into a timed system equation -}
translateComponent :: ParsedComponentId -> ParsedModel -> ConcentrationDB
                   -> TimedSystemEqn
translateComponent ident model concentrations =
    TSENumVerForm nvfcomps [] -- The action set is empty
    where
    nvfcomps    = mkHeadNVFC ident : (map mkTailNVFC derivatives)
    -- Actually holds derivatives minus the component itself, since
    -- we this is added at the head above.
    derivatives = nub (filter (ident /=) $ sequentialDerivatives ident model )
    -- makes an nvfc, with the given concentration, however the given
    -- concentration may be overridden by what is in the concentration
    -- database.
    mkNVFC :: CompCount -> ParsedComponentId -> NVFcomp
    mkNVFC i p = NVFcomp (fromMaybe i $ lookup p concentrations) p
   
    -- So now making the head or any of the tail components is 
    -- just the above but for the head component the default concentration
    -- is 1 and for the others it is 0.
    -- Actually I'm thinking it should *always* be zero, basically for the
    -- tail components they shouldn't be a process array.
    mkTailNVFC :: ParsedComponentId -> NVFcomp
    mkTailNVFC = mkNVFC 0
 
    mkHeadNVFC :: ParsedComponentId -> NVFcomp
    mkHeadNVFC = mkNVFC 1


{-| Return the Component name of a Numerical Vector Component -}
getNvfIdentifier :: NVFcomp -> ParsedComponentId
getNvfIdentifier (NVFcomp _count ident) = ident

{- 
   I should note that perhaps all this getting the sequential derivatives
   should be in the 'Language.Pepa.Analysis.Analysis' module somewhere.
-}

{-| We require a mapping from process identifiers to concentrations -}
type ConcentrationDB = Map ParsedComponentId Int

{-| We will require a pepa database mapping process identifiers
    to their process definitions
-}
type ProcessDataBase = Map ParsedComponentId ParsedComponent

{-| We wish to return a list of sequential derivatives of a component
    these therefore can be stored in a mapping.
-}
type DerivativeDataBase = Map ParsedComponentId [ ParsedComponentId ]

{-| Given a process data base and a process identifier we return all the derivatives
    of the given process identifier.
-}
sequentialDerivatives :: ParsedComponentId -> ParsedModel
                      -> [ ParsedComponentId ]
sequentialDerivatives ident model =
    getDeriv [ident] []
    where
    -- Use breadth first search to find the derivatives of the given components
    -- the first argument is a todo list, that is components we have yet to visit
    -- and the second argument is seen list, that is, we've already seen this
    -- component and it's either been done or on the todo list.
    getDeriv :: [ ParsedComponentId ] -> [ ParsedComponentId ] 
             -> [ ParsedComponentId ]
    getDeriv []             visited = visited
    getDeriv (first : rest) visited
        | elem first visited = getDeriv rest visited
        | otherwise          = getDeriv stillToVisit nowVisited
        where
        stillToVisit = union rest reachable
        nowVisited   = first : visited
        -- reachable with just one transition
        reachable = possibleSuccessors first model


{-|
    @r_a(P)@ function from eqn (5) of Bradley-Hillston 2006
    NOTE: this function uses 
    'Language.Pepa.ApparentRates.summedPepaRate'
    and I think it should use
    'Language.Pepa.ApparentRates.apparentPepaRate'
    but the code that was given to me did not and hence I am not.
-}
apparentTSErate  :: ParsedAction -> TimedSystemEqn
                 -> ParsedModel -> TSEcptExpression
apparentTSErate a (TSECooperation t1 ls t2) model
    | elem a ls = 
        min (apparentTSErate a t1 model) (apparentTSErate a t2 model)
    | otherwise =  
        (apparentTSErate a t1 model) + (apparentTSErate a t2 model)
apparentTSErate a (TSEHide t ls) model
    | elem a ls = 0
    | otherwise = apparentTSErate a t model
apparentTSErate a (TSENumVerForm cs ls) model
    | elem a ls =   
        (TSEind $ TSEiIndC $ map getNvfIdentifier cnes) 
        * minimum [ TSEind $ TSEiInverseInd inverseElems ]
    | otherwise = 
        sum $ map mkAtom cs
    where
    inverseElems = map mkInvElem ces
    mkInvElem :: NVFcomp -> TSEiInvIndElem
    mkInvElem c = ( ident, summedPepaRate a ident model )
                  where ident = getNvfIdentifier c

    -- ces: list of NVF components enabling a
    -- cnes: list of NVF components not enabling a 
    (ces, cnes) = partition partitionFun cs
    partitionFun :: NVFcomp -> Bool
    partitionFun c = isPepaActionEnabled a (getNvfIdentifier c) model

    mkAtom :: NVFcomp -> TSEcptExpression
    mkAtom c = TSEatom ident (summedPepaRate a ident model) 
               where ident = getNvfIdentifier c


{-| The printing of a timed system -}
formatTimedSystem :: TimedSystem -> String
formatTimedSystem (_rates, _concentrations, timedEqn) =
    formatTimedEquation timedEqn


formatTimedEquation :: TimedSystemEqn -> String
formatTimedEquation (TSECooperation left actions right) =
    unlines [ formatTimedEquation left
            , concat [ "<", mkCSlist $ map hprint actions, ">" ]
            , formatTimedEquation right
            ]
formatTimedEquation( TSEHide left actions)              =
    unwords [ formatTimedEquation left
            , "/{"
            , mkCSlist $ map hprint actions
            , "}"
            ]
formatTimedEquation (TSENumVerForm nForms actions) =
     concat [ "("
            , mkCSlist $ map formatNumericalForm nForms
            , ")"
            , "<"
            , mkCSlist $ map hprint actions
            , ">"
            ]


formatTseCompExpr :: TSEcptExpression -> String
formatTseCompExpr (TSEmin t1 t2)     = 
    unwords [ "min"
            , "("
            , formatTseCompExpr t1
            , ","
            , formatTseCompExpr t2
            , ")"
            ]
formatTseCompExpr (TSEsum t1 t2)     = 
    unwords [ formatTseCompExpr t1
            , "+"
            , formatTseCompExpr t2
            ]
formatTseCompExpr (TSEproduct t1 t2) = 
    unwords [formatTseCompExpr t1
            , "*"
            , formatTseCompExpr t2
            ]
formatTseCompExpr (TSEcpt c)         = 
    hprint c
formatTseCompExpr (TSEatom c r)      = 
    concat [ hprint c
           , "*"
           , hprint r
           ]
formatTseCompExpr (TSErate r)        = 
    hprint r
formatTseCompExpr (TSEind i)         =
    unwords [ "I("
            , show i
            , ")"
            ]
formatTseCompExpr (TSEzero)          = "0"


{-| The formatting of a component in numerical vector form -}
formatNumericalForm :: NVFcomp -> String
formatNumericalForm ( NVFcomp i ident) =
    concat [ "("
           , show i
           , ","
           , hprint ident
           , ")"
           ]