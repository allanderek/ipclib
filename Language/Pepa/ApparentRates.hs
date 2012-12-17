{-|
    A module which implements the calculation of apparent rates in a
    pepa model.
-}
module Language.Pepa.ApparentRates
    ( summedPepaRate
    )
where

{- Standard Library Modules Imported -}
{- Non-Standard Library Modules Imported -}
{- Local Library Modules Imported -}
import Language.Pepa.Syntax 
   ( ParsedModel
   , ParsedComponentId
   , Transition          ( .. )
   , ParsedAction
   , ParsedRate
   )
import Language.Pepa.Rates
  ( Rate           ( .. )
  , sumRateExprs
  )
import Language.Pepa.PepaUtils
   ( possibleTransitions )
{- End of Module Imports -}

{-| 
    Calculates the summed rate of an action 
-}
summedPepaRate :: ParsedAction -> ParsedComponentId 
               -> ParsedModel -> ParsedRate
summedPepaRate act component model
  | null imms && null tops && null timeds = error "No rates to sum"
  | not $ null imms                       = RateImmediate $ sumRateExprs imms
  | null imms && null tops                = RateTimed $ sumRateExprs timeds
  | null imms && null timeds              = RateTop   $ sumRateExprs tops
  | (not $ null timeds) && 
    (not $ null tops)                     = error "Mixing passive and active"
  | otherwise                             = error "Cannot make it here"
  where
  trans      = filter ((act==) . pepaTransAction) $   
               possibleTransitions model component
  rates      = map pepaTransRate trans

  imms       = [ i | RateImmediate i <- rates ]
  tops       = [ p | RateTop p       <- rates ]
  timeds     = [ t | RateTimed t     <- rates ]

