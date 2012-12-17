{-| 
   This module implements transformations which take the form of
   replacing parts of a model with new parts.
   Generally names with either parts or other names.
-}
module Language.Pepa.Transform.Replace
   ( -- * Renaming processes
     ProcessRename
   , ProcessRenameMap
   , replaceComponentNames
   , renameCompsInRateDef
   , renameCompsProcessDef

   , ComponentReplace
   , ComponentMap
   , replaceCompNamesComponent

     -- * Renaming rates
   , RateRename
   , RateRenameMap
   , replaceRateNames
   , renameRatesInRateDef

     -- * Overriding\/adding rate definition
   , overrideRateDefinition
   , overrideRateDefinitionList
   )
where

{- Standard Libraries modules imported -}
import Control.Arrow
  ( first
  , second
  )
import Data.Maybe
  ( mapMaybe )
{- External Library modules imported -}
{- Local modules imported imported -}
import qualified Language.Pepa.QualifiedName as Qualified
import Language.Pepa.QualifiedName
  ( ShowOrig           ( .. ) )
import Language.Pepa.Rates
  ( Rate               ( .. )
  , RateExpr           ( .. )
  , renameRateExpr
  )
import Language.Pepa.Syntax
   ( ParsedModel       ( .. )
   , RateSpec
   , ProcessDef
   , ParsedComponentId
   , ParsedComponent   ( .. )
   , Transition        ( .. )
   , RateIdentifier
   , ParsedRate
   ) 
{- End of imports -}

{-| The type of a single process renaming -}
type ProcessRename = ( ParsedComponentId, ParsedComponentId )

{-| The type of a whole mapping from process names to process names -}
type ProcessRenameMap = [ ProcessRename ]

{-|
   'replaceComponentNames' takes in a mapping from component names to 
   components and a model and replaces each occurrence of a name 
   within the mapping with it's associated component.
   [@todo@] should be replaced within a rate expression as well.
-}
replaceComponentNames :: ProcessRenameMap -> ParsedModel -> ParsedModel
replaceComponentNames mapping model =
  ParsedModel  { modelRateSpecs    = newRateSpecs
               , modelProcessDefs  = newProcessDefs
               , modelVirtualComps = newVirtualComps
               , modelSystemEqn    = newSystemEqn
               }
  where
  newRateSpecs    = map (renameCompsInRateDef mapping) $ modelRateSpecs model
  newProcessDefs  = map (renameCompsProcessDef mapping) $ 
                    modelProcessDefs model
  newVirtualComps = map (renameCompsInVirtualDef mapping) $ 
                    modelVirtualComps model
  newSystemEqn    = renameCompsInComponent mapping $ modelSystemEqn model


{-| Renaming of component names applied to rate specifications -}
renameCompsInRateDef :: ProcessRenameMap -> RateSpec -> RateSpec
renameCompsInRateDef = second . replaceCompNamesInRateExp

{-| Renaming of component names applied to virtual component defs -}
renameCompsInVirtualDef :: ProcessRenameMap -> RateSpec -> RateSpec
renameCompsInVirtualDef = second . replaceCompNamesInRateExp

renameCompsProcessDef :: ProcessRenameMap -> ProcessDef -> ProcessDef
renameCompsProcessDef mapping (name, comp) =
   case lookup name mapping of
      Nothing -> (name, renameCompsInComponent mapping comp)
      Just p  -> (p   , renameCompsInComponent mapping comp)
  
{-| Rename all the component names within a given component -}
renameCompsInComponent :: ProcessRenameMap -> ParsedComponent -> ParsedComponent
renameCompsInComponent mapping = 
   replaceCompNamesComponent componentMap
   where   
   componentMap = map (second IdProcess) mapping


-- | The type of a single component replacement
type ComponentReplace = ( ParsedComponentId, ParsedComponent ) 

-- | The type of a mapping from process names to components
type ComponentMap = [ ComponentReplace ]


{-|
   'replaceCompNamesComponent' takes in a mapping from component names to 
   components and a component and replaces each occurrence of a name 
   within the mapping with it's associated component.
   [@todo@] should be replaced within a rate expression as well.
-}
replaceCompNamesComponent :: ComponentMap -> ParsedComponent -> ParsedComponent
replaceCompNamesComponent _db proc@(StopProcess)    = proc
replaceCompNamesComponent db proc@(IdProcess ident) =
   case lookup ident db of
      Nothing -> proc
      Just p  -> p
replaceCompNamesComponent db (PrefixComponent trans nextP)     =
   PrefixComponent newTrans $ replaceCompNamesComponent db nextP
   where
   newTrans = trans { pepaTransRate = replaceCompNamesInRate renameDb rate }
   rate     = pepaTransRate trans
   renameDb = convertComponentMap db
replaceCompNamesComponent db (CondBehaviour cond nextP)        =
   CondBehaviour newCond $ replaceCompNamesComponent db nextP
   where
   newCond  = replaceCompNamesInRateExp renameDb cond
   renameDb = convertComponentMap db

replaceCompNamesComponent db (ComponentSum left right)         =
   ComponentSum (replaceCompNamesComponent db left) 
                (replaceCompNamesComponent db right)
replaceCompNamesComponent db (Cooperation left actions right ) =
   Cooperation (replaceCompNamesComponent db left) actions
               (replaceCompNamesComponent db right)
replaceCompNamesComponent db  (ProcessArray comp s a)          =
   ProcessArray (replaceCompNamesComponent db comp) s a
replaceCompNamesComponent db (Hiding proc actions)             =
   Hiding (replaceCompNamesComponent db proc) actions


{- Converts a 'ComponentMap' into a 'ProcessRenameMap' -}
convertComponentMap :: ComponentMap -> ProcessRenameMap
convertComponentMap = 
   mapMaybe getRenameMapping
   where   
   getRenameMapping :: ComponentReplace -> Maybe ProcessRename
   getRenameMapping (pident1, IdProcess pident2) = Just (pident1, pident2)
   getRenameMapping _                            = Nothing


replaceCompNamesInRate :: ProcessRenameMap -> ParsedRate -> ParsedRate
replaceCompNamesInRate _db rate@(RateTop _)           = rate
replaceCompNamesInRate _db rate@(RateImmediate _)     = rate
replaceCompNamesInRate db  (RateTimed e)              =
   RateTimed $ replaceCompNamesInRateExp db e

{- 
   Replace component names within a rate expression, note that they may only be
   replaced with a component name, not with an arbitrary component.
-}
replaceCompNamesInRateExp :: ProcessRenameMap -> RateExpr -> RateExpr
replaceCompNamesInRateExp prenamemap =
  renameRateExpr newMapping
  where
  -- Okay so this means that we will be replacing the original
  -- names of the rates, is this okay ??
  newMapping = map (first showOrig . second Cident) prenamemap


{-| The type of a single rate replacement -}
type RateRename    = ( RateIdentifier, RateIdentifier )
{-| The type of a rate renaming mapping -}
type RateRenameMap = [ RateRename ]

{-| Replace the rate names in an entire model -}
{- Note that we do not replace the rates in the system component, since
   this should normally not contain any rates
-}
replaceRateNames :: RateRenameMap -> ParsedModel -> ParsedModel
replaceRateNames db model =
  model { modelRateSpecs   = newRDefs
        , modelProcessDefs = newPDefs
        }
  where
  newRDefs = map (renameRatesInRateDef db) $ modelRateSpecs model
  newPDefs = map renamePDef $ modelProcessDefs model
   
  renamePDef :: ProcessDef -> ProcessDef
  renamePDef = second $ replaceRateInComponent db


renameRatesInRateDef :: RateRenameMap -> RateSpec -> RateSpec
renameRatesInRateDef db (name, rexp) =
   case lookup name db of
      Just r  -> (r,    replaceRateRexp db rexp)
      Nothing -> (name, replaceRateRexp db rexp)

{- Replace the rate names in a rate. Of course if the rate is not a
   rate expression then it just remains the same (if it is immediate or infty)
   if however it's a rate expression then we must descent into the rate 
   expression (via 'replaceRateRexp')
-}
replaceRateInRate :: RateRenameMap -> ParsedRate -> ParsedRate
replaceRateInRate _db r@(RateTop _)              = r
replaceRateInRate _db r@(RateImmediate _)        = r
replaceRateInRate db (RateTimed e)               =
   RateTimed $ replaceRateRexp db e

replaceRateRexp :: RateRenameMap -> RateExpr -> RateExpr
replaceRateRexp db =
  renameRateExpr newMapping
  where
  -- Okay so this means that we will be replacing the original
  -- names of the rates, is this okay ??
  -- if not then Language.Hydra.Transform.transformCExp can be used
  newMapping = map (first showOrig . second Cident) db


replaceRateInComponent :: RateRenameMap -> ParsedComponent -> ParsedComponent
replaceRateInComponent _db p@(StopProcess)              = p
replaceRateInComponent _db p@(IdProcess _)              = p
replaceRateInComponent db (PrefixComponent trans next)  =
      PrefixComponent (trans { pepaTransRate = replaceRateInRate db rate } )
                      (replaceRateInComponent db next)
      where
      rate = pepaTransRate trans
replaceRateInComponent db (CondBehaviour cond next)     =
   CondBehaviour (replaceRateRexp db cond)
                 (replaceRateInComponent db next)
replaceRateInComponent db (ComponentSum left right)     =
      ComponentSum (replaceRateInComponent db left)
                   (replaceRateInComponent db right)
replaceRateInComponent db (Cooperation left acts right) =
      Cooperation (replaceRateInComponent db left) acts
                  (replaceRateInComponent db right)
replaceRateInComponent _db p@(ProcessArray _ _ _)       = p
replaceRateInComponent db (Hiding p acts)               =
      Hiding (replaceRateInComponent db p) acts

{-|
   Override a rate definition. Generally this is to allow a @--rate 1.0@
   style of command-line option. Note that if the rate is not originally
   defined then this has the effect of simply adding the given rate
   definition.
-}
overrideRateDefinition :: RateSpec -> ParsedModel -> ParsedModel
overrideRateDefinition (rateName, rExp) model =
   model { modelRateSpecs = newRdefs }
   where
   newRdefs = map updateRate $ modelRateSpecs model

   updateRate rateSpec@(r, _rexp)
     | Qualified.equalTextual rateName r = (r, rExp)
     | otherwise                         = rateSpec  

{-| Applies 'overrideRateDefinition' for all the given rate definitions -}
overrideRateDefinitionList :: ParsedModel -> [ RateSpec ] -> ParsedModel
overrideRateDefinitionList = foldr overrideRateDefinition