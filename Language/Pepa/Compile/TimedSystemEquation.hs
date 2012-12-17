{- 
  Compiling to a timed system equation which is then generally used to compute
  a system of ordinary differential equations.
-}

module Language.Pepa.Compile.TimedSystemEquation
  ( TimedSystemEquation
  , TSEcomp                ( .. )
  , RateEquation           ( .. )
  , pepaToRateEquations
  , pepaToTimedSystemEqn
  , printRateEquation
  )
where

{- External Library Modules Imported -}
{- Standard Library Modules Imported -}
import qualified Data.List as List
{- Local Modules Imported -}
import Language.Pepa.QualifiedName
  ( hprintQualifiedName )
import Language.Pepa.Syntax
  ( ParsedComponentId
  , ParsedModel         ( .. )
  , ParsedComponent     ( .. )
  , CooperationSet      ( .. )
  , ActionIdentifier
  , ParsedRate
  , Transition          ( .. )
  , actionNameOfTrans
  , nameOfAction
  )

import Language.Pepa.PepaUtils
  ( CanonicalSuccessors
  , CanonicalSuccessor
  , allSuccessorsCanonical
  , transitiveDerivatives
  )
import qualified Language.Pepa.Utils as Utils
import qualified Language.Pepa.MainControl as MainControl
import Language.Pepa.MainControl
  ( MainControl )
{- End of Module Imports -}

{-| A timed system equation view of a pepa model -}
type TimedSystemEquation = TSEcomp

{-| 
  Components in a timed system equation are represented as the following
  data type. Note that hiding is not represented because we intend that
  hiding be translated out of the model before we reach this stage.
-}
data TSEcomp = TSEcooperation TSEcomp [ ActionIdentifier ] TSEcomp
             -- | TSEhiding      TSEcomp [ ActionIdentifier ]
             | TSEderivatives [ ParsedComponentId ] [ ActionIdentifier ]

{-| The data model which represents rate equations -}
data RateEquation = RateEquation { rateEqnAction :: ActionIdentifier
                                 , rateEqnSrc    :: CompWeight
                                 , rateEqnTgt    :: CompWeight
                                 , rateEqnRate   :: TSErate
                                 }

type CompWeight = [ ParsedComponentId ]


-- theta looks wrong, it should just take in a component
-- id, BUT theta is never used as a rate itself, only as
-- part of a multiplication so we take
-- @TSEtheta P r@ to mean @theta(P) * r@
data TSErate = TSErate ParsedComponentId ParsedRate
             | TSEmin  TSErate TSErate
             | TSEtheta ParsedComponentId TSErate

{-| Convert a pepa model into a system of rate equations -}
pepaToRateEquations :: ParsedModel -> MainControl [ RateEquation ]
pepaToRateEquations model =
  do tse <- pepaToTimedSystemEqn model
     return $ getRateEquations tse
  where
  getRateEquations :: TimedSystemEquation -> [ RateEquation ]
  getRateEquations tse = 
    case successors of
      Just s  -> 
        timedSystemEqnToRateEqns s tse
      Nothing ->
        error "Attempt to compile to rate equations a model not in canonical form"
    where
    -- actions    = usedActionNames $ analyseModel model
    successors = allSuccessorsCanonical model

{-| 
  Convert a pepa model into a timed system equation view of the
  the same pepa model.
-}
pepaToTimedSystemEqn :: ParsedModel -> MainControl TimedSystemEquation
pepaToTimedSystemEqn model =
  MainControl.valueResult timedSystem "timed-system" logInformation
  where
  logInformation = hprintTimedSystemEquation timedSystem

  timedSystem    = translateSystem $ modelSystemEqn model

  translateSystem :: ParsedComponent -> TSEcomp
  translateSystem (IdProcess ident)                   = 
    translateIdent ident []
  translateSystem (ProcessArray (IdProcess ident) _size mActions) =
    translateIdent ident actions
    where
    actions = maybe [] (map nameOfAction) mActions
  translateSystem (Cooperation _left WildCard _right) =
     error "Cannot yet do a wildcard here for a timedsystem"
  translateSystem (Cooperation left (ActionSet actions) right)    =
    TSEcooperation (translateSystem left)
                   (map nameOfAction actions)
                   (translateSystem right)
  translateSystem (ProcessArray _ _ _)                = 
    -- Don't really know what to do about complex arrays
    -- but to be honest I probably won't use this module
    -- anyway.
    error "pepaToTimedSystemEqn: Complex array"
  translateSystem (Hiding _p _actions)                =
    error "Hiding should have been removed from the model before now"
  translateSystem (PrefixComponent _ _)               =
    error $ "pepaToTimedSystemEqn: prefix components " ++ badEqnError
  translateSystem (ComponentSum _ _)                  =
    error $ "pepaToTimedSystemEqn: component sums " ++ badEqnError
  translateSystem (CondBehaviour _ _)                 =
    error $ "pepaToTimedSystemEqn: conditional behaviour " ++ badEqnError
  translateSystem (StopProcess)                       =
    error $ "pepaToTimedSystemEqn: stop processes " ++ badEqnError

  translateIdent :: ParsedComponentId -> [ ActionIdentifier ] -> TSEcomp
  translateIdent ident actions =
    TSEderivatives derives actions
    where
    derives = List.nub $ ident : (transitiveDerivatives model ident)

  badEqnError = "should not be in the main system equation"


{- 
  Convert a timed system equation into system of rate equations.
  This follows the course of the paper:
    "Performance analysis of Stochastic Process Algebra Models
      using Stochastic Simulation" by jb, stg and Nigel Thomas.
-}
timedSystemEqnToRateEqns :: CanonicalSuccessors
                         -> TimedSystemEquation 
                         -> [ RateEquation ]
timedSystemEqnToRateEqns sTransitions = 
  tseRateEqns 
  where
  tseRateEqns :: TSEcomp -> [ RateEquation ]
  tseRateEqns (TSEderivatives derivs actions)
    | null actions = nonCooperating
    | otherwise    = 
      (error "Cooperating arrays not yet implemented in dizzy translation")
    where
    -- rate Equations for each of the sequential derivatives
    -- these will be split up into those corresponding to the
    -- actions in the cooperation set and those not.
    singleRateEqns = concatMap sequentialRateEqns derivs
    nonCooperating = filter (not . isCooperating) singleRateEqns
    _cooperating   = [ massCooperation a sources targets rate |
                       sources <- undefined -- as yet
                     , targets <- undefined -- as yet
                     , a       <- actions
                     ]

    -- This corresponds to the \Gamma(w,\theta) equation of the
    -- paper which this implementation follows.
    -- It does not (but should) check that any of the rates are
    -- on components with no concentration (see the paper).
    rate           = foldr1 minRates $ map rateEqnRate cooperates
    
    cooperates     = filter isCooperating singleRateEqns

    isCooperating :: RateEquation -> Bool
    isCooperating re = elem (rateEqnAction re) actions

  tseRateEqns (TSEcooperation left actions right) =
    leftNonL ++ rightNonL ++ cooperates
    where
    leftRateEqns  = tseRateEqns left
    rightRateEqns = tseRateEqns right
    leftNonL      = filter (not . isCooperating) leftRateEqns
    rightNonL     = filter (not . isCooperating) rightRateEqns

    leftCoops     = filter isCooperating leftRateEqns
    rightCoops    = filter isCooperating rightRateEqns

    isCooperating :: RateEquation -> Bool
    isCooperating re = elem (rateEqnAction re) actions

    cooperates    = [ combineCoops a b | a <- leftCoops, b <- rightCoops ]

    combineCoops :: RateEquation -> RateEquation -> RateEquation
    combineCoops reLeft reRight =
      RateEquation { rateEqnAction = rateEqnAction reLeft
                   , rateEqnSrc    = (rateEqnSrc reLeft) ++ 
                                     (rateEqnSrc reRight)
                   , rateEqnTgt    = (rateEqnTgt reLeft) ++ 
                                     (rateEqnTgt reRight)
                   , rateEqnRate   = minRates (rateEqnRate reLeft)
                                              (rateEqnRate reRight)
                   }

  sequentialRateEqns :: ParsedComponentId -> [ RateEquation ]
  sequentialRateEqns ident =
    map rateEqnOfTrans relevantTrans
    where
    relevantTrans = filter ((== ident) . fst) sTransitions
      
    -- we turn a transition of a sequential component into a
    -- rate equation, the key point is that the weight is simply
    -- 1
    rateEqnOfTrans :: CanonicalSuccessor -> RateEquation
    rateEqnOfTrans (source, (trans, target)) =
      RateEquation { rateEqnAction = aName
                   , rateEqnSrc    = [ source ]
                   , rateEqnTgt    = [ target ]
                   , rateEqnRate   = TSErate source rate
                   }
      where
      aName = actionNameOfTrans trans
      rate  = pepaTransRate trans


  massCooperation :: ActionIdentifier -> CompWeight -> CompWeight 
                  -> TSErate -> RateEquation
  massCooperation a src tar rate = 
    RateEquation { rateEqnAction = a
                 , rateEqnSrc    = src
                 , rateEqnTgt    = tar
                 , rateEqnRate   = rate
                 }

{- NOTE: clearly this is not correct, it is merely a place holder until I
   do the correct thing
-}
minRates :: TSErate -> TSErate -> TSErate
minRates = TSEmin

{- The printing of rate equations, perhaps this is not the best place for this
    but while I'm updating it, its quite nice to have it near the definition.
-}
printRateEquation :: RateEquation -> String
printRateEquation re =
  unwords [ hprintQualifiedName $ rateEqnAction re
          , ","
          , List.intercalate " + " $ map hprintQualifiedName (rateEqnSrc re)
          , "->"
          , List.intercalate " + " $ map hprintQualifiedName (rateEqnTgt re)
          , ", ["
          , hprintTSErate $ rateEqnRate re
          , "]"
          ]


hprintTSErate :: TSErate -> String
hprintTSErate (TSErate ident _r)= concat [ "("
                                         , hprintQualifiedName ident
                                         , " * "
                                         , error "TODO: print out the rate"
                                         , ")"
                                         ]
hprintTSErate (TSEmin r1 r2)    = concat [ "min("
                                         , hprintTSErate r1
                                         , ", "
                                         , hprintTSErate r2
                                         , ")"
                                         ]
hprintTSErate (TSEtheta ident r) = concat [ "("
                                         , "theta("
                                         , hprintQualifiedName ident
                                         , ")"
                                         , " * "
                                         , hprintTSErate r
                                         , ")"
                                         ]


hprintTimedSystemEquation :: TimedSystemEquation -> String
hprintTimedSystemEquation (TSEcooperation left actions right) =
  unlines [ indentLines $ hprintTimedSystemEquation left
          , show actions
          , indentLines $ hprintTimedSystemEquation right
          ]
hprintTimedSystemEquation (TSEderivatives derivs _actions)    =
  Utils.parenthise . Utils.mkCSlist . (map hprintQualifiedName) $ derivs


indentLines :: String -> String
indentLines s = 
  case lines s of
    []         -> ""
    [ _ ]      -> s
    (h : rest) -> unlines $ h : (map ("    " ++) rest)