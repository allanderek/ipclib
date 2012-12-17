module Language.BioPepa.PepaTranslate
  ( biopepaToPepa )
where

{- Standard Library Modules Imported -}
import Control.Arrow
  ( second )
import qualified Data.Maybe as Maybe
{- External Library Modules Imported -}
{- Local Modules Imported -}
import qualified Language.Pepa.QualifiedName as Qualified
import Language.Pepa.QualifiedName
  ( QualifiedName )
import Language.BioPepa.Syntax
  ( Model         ( .. )
  , ModelOptions  ( .. )
  , ComponentDef
  , Component     ( .. )
  , ComponentName
  , PrefixOp      ( .. )
  , RateDef
  , RateIdent
  , RateExpr      ( .. )
  )
import qualified Language.Pepa.Rates as PepaRates
import qualified Language.Pepa.Syntax as Pepa
import Language.Pepa.Syntax
  ( ParsedModel
  , ParsedComponent
  , ParsedComponentId
  , Transition       ( .. )
  , ParsedAction     ( .. )
  )
import qualified Language.Pepa.Print as PepaPrint
import qualified Language.Pepa.MainControl as MainControl
import Language.Pepa.MainControl 
  ( MainControl )
{- End of Module Imports -}

biopepaToPepa :: ModelOptions -> Model -> MainControl ParsedModel
biopepaToPepa modelOptions model =
  MainControl.valueResult pepaModel logKey logInfo
  where
  logKey    = "translated-pepa-model"
  logInfo   = PepaPrint.hprintPepaModel pepaModel

  -- The step-size which we should use, if this is not set
  -- then we should assume the largest concentration in the
  -- system, but for now we will just use 100
  stepSize     = Maybe.fromMaybe 100 $ modelOptsStepSize modelOptions
  
  pepaModel = 
    Pepa.ParsedModel { Pepa.modelRateSpecs    = pepaRateSpecs
                     , Pepa.modelProcessDefs  = newProcessDefs ++
                                                rateControlDefs
                     , Pepa.modelSystemEqn    = systemEquation
                     , Pepa.modelVirtualComps = []
                     }

  systemEquation   = foldl1 Pepa.wildcardComposition systemComponents

  systemComponents = concat [ systemHighs, systemLows, rateControllers ]

  systemHighs      = map makeHighComp highComponents
  makeHighComp :: ComponentName -> ParsedComponent
  makeHighComp = makeComponent . makeHighName 

  -- The low components are those which are defined but not mentioned
  -- in the system equation.
  systemLows     = map makeLowComp lowComponents
  makeLowComp :: ComponentName -> ParsedComponent
  makeLowComp = makeComponent . makeLowName
  lowComponents  = filter (\c -> not $ elem c highComponents) componentNames

  -- Note that when we call this we have already made the component
  -- into a high or low component name
  makeComponent :: ComponentName -> ParsedComponent
  makeComponent name
    | numberLevels == 1 = singleComponent
    | otherwise         = arrayComponent
    where
    -- The single component is what we will have an array of, of course if
    -- the number of levels equals one then we won't need an array of them.
    singleComponent  = Pepa.IdProcess name
    arrayComponent   = Pepa.ProcessArray singleComponent sizeExpression Nothing
    sizeExpression   = PepaRates.integerRateExp numberLevels
    -- For now we will assume that everyone species has a max concentration
    -- of 100 and that the initial concentration is 100 (but of course this
    -- may be a "low" component hence the actual species has an initial
    -- concentration of zero)
    maxConcentration = Maybe.fromMaybe 100 $ lookup name maxConcMapping
    maxConcMapping   = modelOptsMaxConcs modelOptions
    numberLevels     = div maxConcentration stepSize

  highComponents = getHighComponents $ modelSystemEqn model
  getHighComponents :: Component -> [ ComponentName ]
  getHighComponents (Named ident)               = [ ident ]
  getHighComponents (Cooperation left [] right) =
    (getHighComponents left) ++ (getHighComponents right)
  getHighComponents (Cooperation _left _acts _right) =
    error "Sorry we cannot yet handle cooperation with synchronisation"
  getHighComponents (CompConcent _ _)           =
    error "Sorry we cannot yet handle component concentrations"
  getHighComponents (Choice _ _)                =
    error "Sequential definition found in system equation"
  getHighComponents (PrefixComp _ _ _ _)        =
    error "Sequential definition found in system equation"
    

  rateControllers   = map makeRateController rateDefinitions
  makeRateController :: RateDef -> ParsedComponent
  makeRateController = Pepa.IdProcess .  makeRateControlName . fst

  rateControlDefs  = map makeRateControlDef rateDefinitions
  makeRateControlDef :: RateDef -> Pepa.ProcessDef
  makeRateControlDef (rIdent, _rExpr) = 
    -- Note here that we ignore the rate expression because the rate
    -- expression has already been translated into a pepa rate specification.
    -- We could forget about producing rate expressions and translate the
    -- rates here, since this should be the only place they are used
    -- (the processes are performing activities passively controlled
    -- by these rate controllers)
    (name, prefixComp)
    where
    prefixComp = Pepa.PrefixComponent transition $ Pepa.IdProcess name
    transition = Transition { pepaTransAction     = Action actionName
                            , pepaTransCoalsced   = []
                            , pepaTransPriority   = Pepa.defaultPepaPriority
                            , pepaTransRate       = rate
                            , pepaTransConditions = []
                            }
    -- Note that we must have the same name generated for the increase
    -- as the reduction (see below). This is because we want the
    -- reactants and the products to synchronise on the "reaction".
    actionName = makeActionName rIdent
    rate       = PepaRates.RateTimed $ PepaRates.Cident rIdent
    name       = makeRateControlName rIdent    

  makeRateControlName :: RateIdent -> ParsedComponentId
  makeRateControlName rIdent = Qualified.prefixQName rIdent "Control" 

  -- The pepa rates are the rate specifications given in the biopepa
  -- model together with the rate parameters, the rate specifications
  -- are translated slightly differently as they must take into account
  -- the step-size whereas the rate parameters do not.
  pepaRateSpecs    = rateParamSpecs ++ newRateSpecs
  rateParamSpecs   = map translateRateParamSpec $ 
                     modelOptsRateParams modelOptions

  translateRateParamSpec :: (RateIdent, Double) -> Pepa.RateSpec
  translateRateParamSpec = second PepaRates.realRateExp
  
  newRateSpecs    = map translateRateSpec rateDefinitions
  rateDefinitions = modelRateDefs model
  newProcessDefs  = concatMap translateComponentDef componentDefs
  componentDefs   = modelCompDefs model
  -- We use these to translate the rate expressions, if we come across
  -- a name we must decide whether it is refering to a concentration
  -- or another rate name. Additionally we use these to add to the
  -- translated system component those which are low (ie not present in
  -- the biopepa system equation).
  componentNames :: [ ComponentName ]
  componentNames = map fst componentDefs


  -- Translating a rate specification we must be careful, the whole
  -- thing is wrapped in a division by the step size hence we must
  -- be a little careful that we don't attempt to translate an
  -- "intermediary" rate definition, eg "k2 = k1" or "k2 = k1/2"
  -- These should be done as rate parameters specified either
  -- separately to the model or with a different syntax.
  translateRateSpec :: RateDef -> Pepa.RateSpec
  translateRateSpec (rident, rateExpr)
    | modelOptsRateCalc modelOptions = 
      (rident, PepaRates.divideRateExprs (translateRateExpr rateExpr)
                                         (PepaRates.integerRateExp stepSize)
      )
    | otherwise                      =
      (rident, translateRateExpr rateExpr)


  translateRateExpr :: RateExpr -> PepaRates.RateExpr
  translateRateExpr expr =
    -- All the rates must be translated by dividing by the step-size
    -- it's easiest to just go ahead and translate the rate expression
    -- normally and then divide the whole thing by the step size
    -- PepaRates.divideRateExprs (translateNumerator expr) denominator
    translateNumerator expr
    where
    -- denominator  = PepaRates.integerRateExp stepSize
    translateNumerator :: RateExpr -> PepaRates.RateExpr
    translateNumerator (RateConstant d) = PepaRates.realRateExp d
    translateNumerator (RateName ident)
      -- If it is a component name then we multiply the number of
      -- components in the "high" state by the step-size since this
      -- gives the "number" of components (or the concentration) 
      -- of that species.
      | elem ident componentNames =
        if modelOptsRateCalc modelOptions
           then PepaRates.multiplyRateExprs 
                          (PepaRates.integerRateExp stepSize)
                          (PepaRates.namedRateExp $ makeHighName ident)
           else PepaRates.namedRateExp $ makeHighName ident
      -- If it is not a component name then we just leave it as is
      -- this will be a rate constant or a rate parameter.
      | otherwise                 =
        PepaRates.namedRateExp ident
    translateNumerator (RateMult e1 e2) =
      PepaRates.multiplyRateExprs (translateNumerator e1)
                                  (translateNumerator e2)
    translateNumerator (RateDiv  e1 e2) =
      PepaRates.divideRateExprs (translateNumerator e1)
                                (translateNumerator e2)
    translateNumerator (RateAdd  e1 e2) =
      PepaRates.addRateExprs (translateNumerator e1)
                             (translateNumerator e2)
    translateNumerator (RateSub  e1 e2) =
      PepaRates.subtractRateExprs (translateNumerator e1)
                                  (translateNumerator e2)


  makeHighName :: QualifiedName -> QualifiedName
  makeHighName name = Qualified.suffixQName name "_high"
  makeLowName ::  QualifiedName -> QualifiedName
  makeLowName name = Qualified.suffixQName name "_low"

  -- Strictly speaking this could return a pair of process defs, rather
  -- than a list, since we will always return a definition for high
  -- and one for low, but since we are going to concatenate them all
  -- together anyway.
  translateComponentDef :: ComponentDef -> [ Pepa.ProcessDef ]
  translateComponentDef (compName, component) =
    [ highDef, lowDef ]
    where
    highDef  = ( highName, highPrefixes )
    highName =  makeHighName compName
    lowName  =  makeLowName compName
    lowDef   =  ( lowName, lowPrefixes )

    highPrefixes
      | null highActions = Pepa.StopProcess
      | otherwise        = foldr1 Pepa.ComponentSum highActions
    highActions = fst bothActions

    lowPrefixes
      | null lowActions = Pepa.StopProcess
      | otherwise       = foldr1 Pepa.ComponentSum lowActions
    lowActions  = snd bothActions

    
    bothActions = getComponentActions component
    getComponentActions :: Component -> ( [ Pepa.ParsedComponent ]
                                        , [ Pepa.ParsedComponent ] )
    getComponentActions (Choice left right)                     =
      concatPairs (getComponentActions left) (getComponentActions right)
    -- For now, we're assuming that all reactants are of the
    -- shorthand form. I should at least check that this is
    -- the case.
    getComponentActions (PrefixComp rateId _sto Reactant comp)
      | comp == compName = ([ prefixComp ], [])
      | otherwise        = error "Must be simple prefix to self"
      where
      prefixComp = Pepa.PrefixComponent transition $ Pepa.IdProcess lowName
      transition = Transition { pepaTransAction     = Action actionName
                              , pepaTransCoalsced   = []
                              , pepaTransPriority   = Pepa.defaultPepaPriority
                              , pepaTransRate       = rate
                              , pepaTransConditions = []
                              }
      -- Note that we must have the same name generated for the increase
      -- as the reduction (see below). This is because we want the
      -- reactants and the products to synchronise on the "reaction".
      actionName = makeActionName rateId
      rate       = PepaRates.topRate 
                   -- PepaRates.RateTimed $ PepaRates.Cident rateId
    getComponentActions (PrefixComp rateId _sto Product comp)
      | comp == compName = ( [], [ prefixComp ] )
      | otherwise        = error "Must be simple prefix to self"
      where
      prefixComp = Pepa.PrefixComponent transition $ Pepa.IdProcess highName
      transition = Transition { pepaTransAction     = Action actionName
                              , pepaTransCoalsced   = []
                              , pepaTransPriority   = Pepa.defaultPepaPriority
                              , pepaTransRate       = rate
                              , pepaTransConditions = []
                              }
      actionName = makeActionName rateId
      rate       = PepaRates.topRate
                   -- PepaRates.RateTimed $ PepaRates.Cident rateId

    -- Don't know what to do here.
    -- If it is not a reactant, then it's not a 'high' activity
    getComponentActions (PrefixComp rateId _sto Activator comp)
      | comp == compName = ( [ prefixComp ], [] )
      | otherwise        = error "Must be simple prefix to self"
      where
      -- Note, it is a high activity and the result is STILL the high
      -- activity, that is we do not change the concentration of ourselves
      -- we are here only to activate the rate. If we are in the low position
      -- we cannot do this activity (unless we also perform it as a product)
      prefixComp = Pepa.PrefixComponent transition $ Pepa.IdProcess highName
      transition = Transition { pepaTransAction     = Action actionName
                              , pepaTransCoalsced   = []
                              , pepaTransPriority   = Pepa.defaultPepaPriority
                              , pepaTransRate       = rate
                              , pepaTransConditions = []
                              }
      actionName = makeActionName rateId
      rate       = PepaRates.topRate
    -- I am completely unsure about this but it seems to be the same
    -- as for activiators. That is we must be present but we can
    -- do not change our concentration level.
    getComponentActions (PrefixComp rateId _sto GenModifier comp)
      | comp == compName = ( [ prefixComp ], [] )
      | otherwise        = error "Must be simple prefix to self"
      where
      -- Note, it is a high activity and the result is STILL the high
      -- activity, that is we do not change the concentration of ourselves
      -- we are here only to activate the rate. If we are in the low position
      -- we cannot do this activity (unless we also perform it as a product)
      prefixComp = Pepa.PrefixComponent transition $ Pepa.IdProcess highName
      transition = Transition { pepaTransAction     = Action actionName
                              , pepaTransCoalsced   = []
                              , pepaTransPriority   = Pepa.defaultPepaPriority
                              , pepaTransRate       = rate
                              , pepaTransConditions = []
                              }
      actionName = makeActionName rateId
      rate       = PepaRates.topRate
    getComponentActions (PrefixComp _ _ _ _)                    = 
      error "Not sure how to translate inhibitors yet"
    getComponentActions (CompConcent _ _ )                      =
      error "Sorry we cannot yet deal with parallel definitions"
    getComponentActions (Named _)                               =
      error "Sorry we cannot yet deal with parallel definitions"
    getComponentActions (Cooperation _ _ _ )                    =
      error "Sorry we cannot yet deal with parallel definitions"

  -- Creates a PEPA action name from a biopepa rate identifier.
  makeActionName :: RateIdent -> Pepa.ActionIdentifier
  makeActionName rateId = Qualified.suffixQName rateId "_action"

-- Utilities
concatPairs :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
concatPairs (leftAs, leftBs) (rightAs, rightBs) =
  (leftAs ++ rightAs, leftBs ++ rightBs)
