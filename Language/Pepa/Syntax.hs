{-|
   A suite of data types describing the abstract syntax of pepa models.
-}
module Language.Pepa.Syntax
  ( -- * Identifier Types
    Identifier -- A generic identifier
  , RateIdentifier
  , ActionIdentifier
  , ProcessIdentifier
  
  -- * Parsed Syntax
  , ParsedModel          ( .. )
  , emptyModel
  , VirtualSpec
  , RateSpec
  , ProcessDef
  , processDefsOfModel
  , mainCompOfModel
  , rateDefsOfModel
  , combineModels
  , ParsedDefinition     ( .. )
  , filterProcessDefintions
  , filterRateSpecifications
  , filterVirtualComps
  , filterOtherDefinitions
  
  , ParsedComposition
  , ParsedComponent      ( .. )
  , CooperationSet       ( .. )
  , parallelComposition
  , wildcardComposition
  , ProcessConcentration
  , Transition           ( .. )
  , ParsedTrans
  , ParsedPriority
  , modifyTransitionRate
  , defaultPepaPriority
  , defaultIncreasedPriority
    
  , actionNameOfTrans
  , involvesAction
  , ParsedComponentId
  , ConcentrationSpec
  , ParsedRate

  , ParsedAction         ( .. )
  , makeTauAction
  , isTauAction
  , nameOfAction
  ) 
where

{- Imported Standard Libraries -}
import Data.Function
  ( on )
{- Imported External Library Modules -}
{- Imported Local Libraries -}
import Language.Pepa.Rates
  ( RateIdentifier
  , RateExpr
  , ModelRate
  )
import Language.Pepa.QualifiedName
  ( QualifiedName        ( .. ) )
{- End of Imports -}


{-|
    A generic Identifier type, this is used for all names in a pepa
    model which are either not rates, actions or processes or not yet
    known to be one of those.
-}
type Identifier        = String 

-- | The type associated with activity names
type ActionIdentifier  = QualifiedName

-- | The type associated with process identifiers
type ProcessIdentifier = QualifiedName

{-|
  To date we do not parse in process concentrations but it is on the
  to do list.
-}
type ProcessConcentration = Int

{-| Concentration specification is basically the type which stores
    a pair of the component state, and its initial concentration.
-}
type ConcentrationSpec = (ParsedComponentId, ProcessConcentration)

{-|
  Rate definitions are given on the command line as well as in
  the pepa source file. The rate identifiers are initially mapped
  to rate expressions but hopefully we can reduce them to actual
  values. Of course this is not always possible with the introduction
  of functional rates.
-}
type RateSpec = (RateIdentifier, RateExpr)

{-
  A 'RateDictionary' is used to map rate identifier to
  their values. This is used in the reduction of rate expressions.
  The rate dictionaries themselves must be built up not just from
  the pepa model in the input file but also from the command line
  arguments.
-}
--type RateDictionary = [(RateIdentifier, Double)]

{-|
   A process definition is a process identifier and a parsed component.
-}
type ProcessDef = (ParsedComponentId, ParsedComponent)

{-|
  A virtual process definition gives us a name with which to refer
  to some function of the concentrations in a PEPA model.
  For example we might do:
  @Unavailable = [ Broken + Offline ] @
-}
type VirtualSpec = (ParsedComponentId, RateExpr)

{-
  All of these derive the 'Show' class, this is required for printing
  out during testing and also (perhaps) printing the parsed model to
  the log file, however I should actually instantiate the show functions
  using some pretty printing. Well perhaps not, perhaps for debugging the
  derived show is enough, and for pretty printing to the log file a
  separate pretty printing function should be written.
-}

{-|
  The type of a whole parsed pepa model.
-}
data ParsedModel = 
  ParsedModel { modelRateSpecs    :: [ RateSpec ]
              , modelProcessDefs  :: [ ProcessDef ]
              , modelVirtualComps :: [ VirtualSpec ]
              , modelSystemEqn    :: ParsedComposition
              }
              deriving (Show, Read)


{-|
  Create a model with the given system component but no definitions whatsoever.
  This will not be a valid model but is usually used when combining models
  together.
-}
emptyModel :: ParsedComposition -> ParsedModel
emptyModel comp = ParsedModel { modelSystemEqn    = comp
                              , modelProcessDefs  = []
                              , modelVirtualComps = []
                              , modelRateSpecs    = []
                              }

{-| Combine two models together using the given function to compose
    the two system equations together.
-}
combineModels :: (ParsedComposition -> ParsedComposition -> ParsedComposition) 
               -> ParsedModel -> ParsedModel -> ParsedModel
combineModels combinef m1 m2 =
  ParsedModel { modelRateSpecs    = on (++) modelRateSpecs m1 m2
              , modelProcessDefs  = on (++) modelProcessDefs m1 m2
              , modelVirtualComps = on (++) modelVirtualComps m1 m2
              , modelSystemEqn    = on combinef modelSystemEqn m1 m2
              }

{-|
  Return the rate definitions within a model
-}
rateDefsOfModel :: ParsedModel -> [ RateSpec ]
rateDefsOfModel = modelRateSpecs

{-|
  Returns the process definitions of a model
-}
processDefsOfModel :: ParsedModel -> [ ProcessDef ]
processDefsOfModel = modelProcessDefs

{-|
  Returns the main system equation of the given model
-}
mainCompOfModel :: ParsedModel -> ParsedComponent
mainCompOfModel = modelSystemEqn

{-|
  A definition in a pepa model can define one of two things:
  1. A process definition
  2. A rate definition
  We would like to restrict the type a bit more. A process definition
  should consist only of sequential componenents. However we also want
  to allow such definitions as:
  @P = P1 <> P1@
  to aid in the writing of the system component.
  We remove this from the 'ParsedModel' which is returned from the parser
  because we separate them out there. So this is a temporary holder data
  structure.
   
  Update: We now parameterise this type and allow a generic definition
  which could be used to allow many different extensions to Pepa.
-}
data ParsedDefinition a   = 
    ProcessDef ProcessDef
  | RateDef RateSpec
  | VirtualDef VirtualSpec
  | OtherDef a

filterProcessDefintions :: [ ParsedDefinition a ] -> [ ProcessDef ]
filterProcessDefintions defs =
  [ (pident, comp) | ProcessDef (pident, comp) <- defs ]

filterRateSpecifications :: [ ParsedDefinition a ] -> [ RateSpec ]
filterRateSpecifications defs =
  [ (rident, rexpr) | RateDef (rident, rexpr) <- defs ]

filterVirtualComps :: [ ParsedDefinition a ] -> [ VirtualSpec ]
filterVirtualComps defs =
  [ (vident, rexpr) | VirtualDef (vident, rexpr) <- defs ]

filterOtherDefinitions :: [ ParsedDefinition a ] -> [ a ]
filterOtherDefinitions defs = 
  [ a | OtherDef a <- defs ]

{-|
    The main pepa composition is simply a component.
-}
type ParsedComposition   = ParsedComponent

{-|
  A parsed parallel component is cooperation between two
  components over a set of actions as in @(P1 <a b> P2)@.
  We now refuse to have sequential components within parallel components.
  It would be nice if we could have a sub-type such that the operands to
  component sums and prefix components could only be sequential components.
  However we require a typing phase anyway since we may have something:
  @
  P  = (a, r).P1 ;
  P1 = Q <> Q
  @
  Hence although syntactically the prefix looks okay, it is not since @P1@
  is actually a parallel component.
  The parser should still disallow obvious nonsense such as
  @ P = (a, r).(Q <> Q) ; @
-}
data ParsedComponent    = 
    IdProcess ParsedComponentId
  | PrefixComponent ParsedTrans ParsedComponent
  | ComponentSum ParsedComponent ParsedComponent
  | CondBehaviour RateExpr ParsedComponent
  | StopProcess
  | Cooperation ParsedComponent CooperationSet ParsedComponent
  | ProcessArray ParsedComponent RateExpr ( Maybe [ ParsedAction ] )
  | Hiding ParsedComponent [ ParsedAction ]
  deriving (Show, Read)

{-|
  A data type for the cooperation set within a parallel composition.
  We allow wildcard expansion, as yet we do not allow the definition
  of a set by name but we may.
-}
data CooperationSet = 
     WildCard 
   | ActionSet [ ParsedAction ]
   deriving (Show, Read, Eq)

{-| The empty cooperation set -}
emptyCooperationSet :: CooperationSet
emptyCooperationSet = ActionSet []

{-| The component caused by combining two components together in
    a parallel composition, that is a cooperation with the empty
    set of actions.
-}
parallelComposition :: ParsedComponent -> ParsedComponent -> ParsedComponent
parallelComposition l = Cooperation l emptyCooperationSet

{-| The component caused by combining two components together in
    a parallel composition in which the two processes synchronise
    over all of the names which they share.
    CLEARLY CURRENTLY WRONG
-}
wildcardComposition :: ParsedComponent -> ParsedComponent -> ParsedComponent
wildcardComposition l = Cooperation l WildCard

{-| Definition of a transition parameterised by the kind of the
    rate. A rate may be for example a rate expression or a concrete
    rate value.
    The 'pepaTransCoalsced' field will be set to the empty list when
    first parsed in and should remain so until we remove the immediate
    actions. The idea is that this is a list of immediate actions which
    have been coalsced into this one 'timed' activity. From this we can
    then compute the throughput of an immediate action.
-}
data Transition a =
  Transition { pepaTransAction     :: ParsedAction
             , pepaTransCoalsced   :: [ ParsedAction ]
             , pepaTransPriority   :: ParsedPriority
             , pepaTransRate       :: a
             , pepaTransConditions :: [ RateExpr ]
             }
             deriving (Show, Read, Eq, Ord) 

{-|
  A parsed transition from one component to another, this forms
  part of a prefix component.
-}
type ParsedTrans = Transition ParsedRate

modifyTransitionRate :: (a -> b) -> Transition a -> Transition b
modifyTransitionRate f transition =
  transition { pepaTransRate = f $ pepaTransRate transition }


{-| The type of a priority is simply and integer -}
type ParsedPriority = Int

{-| The default pepa priority -}
defaultPepaPriority :: ParsedPriority
defaultPepaPriority = 1

{-| The default increased priority -}
defaultIncreasedPriority :: ParsedPriority
defaultIncreasedPriority = 1000


{-| Returns the name of the action in a 'ParsedTrans' -}
actionNameOfTrans :: Transition a -> ActionIdentifier
actionNameOfTrans = nameOfAction . pepaTransAction

{-| Returns whether or not a given transition involves the
    given action identifier.
-}
involvesAction :: ActionIdentifier -> Transition a -> Bool
involvesAction ident = (ident ==) . actionNameOfTrans

{-
  Finally the base definitions.
-}
type ParsedComponentId = ProcessIdentifier

{-|
  'ParsedAction' is a bit of a misnomer now because they contain actions
  which cannot be parsed such as 'Tau' which may result from hiding and
  'ComAction' which are inserted by the translation of a performance
  specification probe.
-}
data ParsedAction      = Tau       ActionIdentifier
                       | Action    ActionIdentifier
                       | ComAction ActionIdentifier
                       deriving (Show, Read, Ord)
       
instance Eq ParsedAction where
    (==) = equalParsedActions

equalParsedActions :: ParsedAction -> ParsedAction -> Bool
equalParsedActions (Action id1)    (Action id2)    = id1 == id2
equalParsedActions (ComAction id1) (ComAction id2) = id1 == id2
equalParsedActions _               _               = False

makeTauAction :: ParsedAction -> ParsedAction
makeTauAction (Action ident)    = Tau ident
makeTauAction (ComAction ident) = Tau ident
makeTauAction act@(Tau _)       = act

isTauAction :: ParsedAction -> Bool
isTauAction (Tau _)       = True
isTauAction (Action _)    = False
isTauAction (ComAction _) = False

nameOfAction :: ParsedAction -> ActionIdentifier
nameOfAction (Tau       ident) = ident
nameOfAction (Action    ident) = ident
nameOfAction (ComAction ident) = ident

{-| A rate within the parsed model -}
type ParsedRate = ModelRate
