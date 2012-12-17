module Language.BioPepa.OdesTranslate
  ( biopepaToOdes )
where

{- Standard Library Modules Imported -}
{- External Library Modules Imported -}
import qualified  Horddes.Solve
import qualified  Horddes.Odes as Odes
import Horddes.Odes
  ( ChangeFunction   ( .. ) )
{- Local Modules Imported -}
import Language.Pepa.QualifiedName as Qualified
import Language.BioPepa.Syntax
  ( Model         ( .. )
  , ModelOptions  ( .. )
  , ComponentDef
  , Component     ( .. )
  , ComponentName
  , PrefixOp      ( .. )
  , RateIdent
  , RateExpr      ( .. )
  )
import qualified Language.Pepa.MainControl as MainControl
import Language.Pepa.MainControl 
  ( MainControl )
{- End of Module Imports -}



biopepaToOdes :: ModelOptions -> Model 
               -> MainControl (Odes.Odes, Horddes.Solve.Environment)
biopepaToOdes modelOptions model =
  MainControl.valueResult (odes, initEnv) logKey logInfo
  where
  logKey          = "translated-ode-system"
  logInfo         = Odes.hprintOdes odes
  odes            = map translateComponentDef componentDefs
  componentDefs   = modelCompDefs model
  rateSpecs       = modelRateDefs model

  initEnv         = Horddes.Solve.makeInitialEnvironment $ 
                    map makeConcPair modelSpecies
  makeConcPair :: ComponentName -> (String, Double)
  makeConcPair n = (Qualified.textual n, getConcentration n)
  modelSpecies    = map fst componentDefs

  getConcentration :: ComponentName -> Double
  getConcentration name = maybe 0 fromIntegral $ lookup name initialConcMapping
  initialConcMapping = modelOptsInitConcs modelOptions

  translateComponentDef :: ComponentDef -> Odes.Ode
  translateComponentDef (name, component) = 
    (Qualified.textual name, translateComponent component)
  translateComponent :: Component -> ChangeFunction
  translateComponent (Named name)                        =
    NameAtT $ Qualified.textual name
  translateComponent (Choice left right)                 =
    Plus (translateComponent left) (translateComponent right)
  translateComponent (PrefixComp rateIdent _ Reactant _) =
    -- We assume the component involved is the one being defined.
    Minus (Number 0) $ translateRateId rateIdent
  translateComponent (PrefixComp rateIdent _ Product _)  =
    -- We assume the component involved is the one being defined.
    translateRateId rateIdent
  translateComponent (PrefixComp _ _ _ _)                =
    error "Apologies, that kind of prefix component not yet supported"
  translateComponent (Cooperation _ _ _)                 =
    error "Sorry, parallel definitions not yet supported"
  translateComponent (CompConcent _ _ )                  =
    error "Sorry, parallel definitions not yet supported"

  rateParamMapping = modelOptsRateParams modelOptions
  
  translateRateId :: RateIdent -> ChangeFunction
  translateRateId rateId
    | Just re <- lookup rateId rateSpecs = translateRate re
    | otherwise                          = error "Unbound rate name"
  translateRate :: RateExpr -> ChangeFunction
  translateRate (RateName name)
    | elem name modelSpecies = NameAtT $ Qualified.textual name
    | otherwise              = 
      maybe (Number 0.01) Number $ lookup name rateParamMapping
  translateRate (RateConstant d)      = Number d
  translateRate (RateAdd left right)  =
    Plus (translateRate left) (translateRate right)
  translateRate (RateSub left right)  =
    Minus (translateRate left) (translateRate right)
  translateRate (RateMult left right) =
    Mult (translateRate left) (translateRate right)
  translateRate (RateDiv left right)  =
    Divide (translateRate left) (translateRate right)

-- Utility function
-- pair :: (a -> b) -> a -> (a,b)
-- pair f a = (a, f a)