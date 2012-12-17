module Language.BioPepa.Syntax
  ( Model         ( .. )
  , ModelOptions  ( .. )
  , defaultModelOptions
  , MaxConcentrations
  , MaxConcentration
  , InitialConcentrations
  , InitialConcentration
  , RateParameter
  , RateParams
  , RateDef
  , RateExpr      ( .. )
  , RateIdent
  , Component     ( .. )
  , ComponentName
  , ComponentDef
  , StoCoeff
  , Action
  , PrefixOp      ( .. )
  , Concentration
  )
where

{- Standard Library Modules Imported -}
{- External Library Modules Imported -}
{- Local Modules Imported -}
import Language.Pepa.QualifiedName
  ( QualifiedName )
{- End of Module Imports -}


{-| The model options are settings relevant to a model which may
    be defined separately from the model itself such as the rate
    parameters and the step size.
-}
data ModelOptions =
  ModelOptions { modelOptsStepSize   :: Maybe Int
               , modelOptsRateParams :: RateParams
               , modelOptsMaxConcs   :: MaxConcentrations
               , modelOptsInitConcs  :: InitialConcentrations
               , modelOptsRateCalc   :: Bool
               }
               deriving (Read, Show)

{-| The default model options if none are given with the model -}
defaultModelOptions :: ModelOptions
defaultModelOptions =
  ModelOptions { modelOptsStepSize   = Nothing
               , modelOptsRateParams = []
               , modelOptsMaxConcs   = []
               , modelOptsInitConcs  = []
               , modelOptsRateCalc   = False
               }

{-| The representation of an entire biopepa model -}
data Model =
  Model { modelRateDefs  :: [ RateDef ]
        , modelCompDefs  :: [ ComponentDef ]
        , modelSystemEqn :: Component
        }
        deriving (Show, Read)

{-| The max concentrations allow the user to give the maximum concentration
    of a particular species (this is different from the initial concentration
-}
type MaxConcentrations = [ MaxConcentration ]
type MaxConcentration  = (ComponentName, Int)

{-| An initial concentration mapping is the same as max concentration mapping -}
type InitialConcentrations = [ InitialConcentration ]
type InitialConcentration  = (ComponentName, Int)

{-| Rate parameters these are slightly different from rate definitions -}
type RateParameter = (RateIdent, Double)
type RateParams    = [ RateParameter ]

{-| Rate definitions.
-}
type RateDef = (RateIdent, RateExpr)

{-| A Rate expression is our own small language for dealing with rates.
    It may be nice to unify this with PEPA rate expressions as defined
    in Language.Pepa.Rates, but there are some complications.
-}
data RateExpr = 
    RateName RateIdent
  | RateConstant Double
  | RateMult RateExpr RateExpr
  | RateAdd  RateExpr RateExpr
  | RateDiv  RateExpr RateExpr
  | RateSub  RateExpr RateExpr
  deriving (Show, Read)

{-| The type of an identifier within a rate expression -}
type RateIdent = QualifiedName


{-| The type of a component definition -}
type ComponentDef = (ComponentName, Component)

{-| The type of a component defined in the model.
    Currently we are doing the same as in normal Pepa,
    that is we do not distinguish between sequential
    or parallel components in the type. The reason for this
    is to allow parallel definitions.
-}
data Component =
    Named  ComponentName
  | PrefixComp RateIdent StoCoeff PrefixOp ComponentName
  | Choice Component Component
  | Cooperation Component [ Action ] Component
  | CompConcent ComponentName Concentration
  deriving (Show, Read)

{-| The type used to represent the name of a component -}
-- TODO: we should consider having a list of qualified names
-- where the list represents those parts separated by colons.
-- If this turns out to be useful then I'll do this.
type ComponentName = QualifiedName

{-| The type of stoichometery coefficient -}
type StoCoeff = String

{-| The type of an operator used in the prefix components -}
data PrefixOp = Reactant
              | Product
              | Activator
              | Inhibitor
              | GenModifier
              deriving (Show, Read)

{-| The type of an action -}
type Action = QualifiedName

{-| The type of a concentration -}
type Concentration = Double