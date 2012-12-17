{-| 
   This module translates an Srmc model into a Pepa model.
-}
module Language.Pepa.Srmc.SrmcPepa
  ( TranslatedModel     ( .. )
  , NameSpace
  , RateArray -- Maybe this should be defined elsewhere
  , nullRateArray
  , headRateArray
  , tailRateArray
  , translateSrmc
  , hprintNamesTree
  )
where

{- Standard Libraries modules imported -}
import Control.Arrow
  ( second )
import qualified Data.Map as Map
import Data.Map
  ( Map )
import Data.Maybe
  ( mapMaybe )
import qualified Data.Set as Set
{- External Library modules imported -}
{- Local modules imported imported -}
import qualified Language.Pepa.QualifiedName as Qualified
import Language.Pepa.QualifiedName
  ( QualifiedName     ( .. )
  , textual
  )
import qualified Language.Pepa.Rates as Rates
import Language.Pepa.Rates
  ( Rate              ( .. )
  , RateExpr
  , namedRateExp
  )
import Language.Pepa.Syntax
  ( RateIdentifier 
  , RateSpec 
  , ProcessDef
  , ParsedModel       ( .. )
  , ParsedComponent   ( .. )
  , Transition        ( .. )
  , ParsedRate
  )   
import qualified Language.Pepa.Print as PepaPrint
import Language.Pepa.Srmc.Syntax
   ( SrmcModel         ( .. )
   , SrmcDef           ( .. )
   )
import Language.Hydra.Transform
  ( transformCExp )
import Ipc.Cli
   ( CliOpt )
import Smc.Cli
   ( SmcOptions )

import qualified Language.Pepa.Analysis.Analysis as Analysis
import qualified Language.Pepa.MainControl as MainControl
import Language.Pepa.MainControl
  ( MainControl )
{- End of imports -}

{-|
   The type of a rate array is the type of a rate identifier mapped
   to a list of possible rate expressions which will/could be
   ranged over in a parameter sweep computation.
-}
type RateArray = (RateIdentifier, [ RateExpr ] )

{-| Report whether or not a rate array is null -}
nullRateArray :: RateArray -> Bool
nullRateArray = null . snd

{-| Take the first element in a rate array to make a rate specification -}
headRateArray :: RateArray -> RateSpec
headRateArray = second head

{-| Remove the first element from a rate array -}
tailRateArray :: RateArray -> RateArray
tailRateArray = second tail

{-|
  The type returned by the translation function.
  Basically we wish to return the translated model
  as well as all the rates which way may range over
  in parameter sweep computations.

  The 'slated' prefix on all the names here is just short
  for 'translated', 'trans' seems more reasonable but this
  seems to be used all over ipclib.
-}
data TranslatedModel =
   TModel { slatedModel       :: ParsedModel 
          , slatedRates       :: [ RateArray ]
          , slatedProcesses   :: [ ProcessDef ]
          , slatedNamesChosen :: [ NameSpaceChoice ]
          }

-- Here it is a list of the name spaces which were
-- chosen, the first is the namespace which is defined
-- by the selection, the second is the choice made.
type NameSpaceChoice = ( NameSpace, NameSpace )

{-|
   The main exported function, this takes in an Srmc model and
   translates it into a list of pepa models.
   The pepa models correspond to the different ways in which the
   processes can be combined together. So for example if we had
   a simple srmc model in which the only two set of processes
   definitions were:
   @P = { P1, P2 } @
   @Q = { Q1, Q2 } @
   Then there would be four output pepa models each setting
   the first being
   @ P = P1 ; Q = Q1@
   the second being
   @ P = P2 ; Q = Q1@
   and so on.
-}
translateSrmc :: [ CliOpt SmcOptions ] 
              -> SrmcModel
              -> MainControl [ TranslatedModel ]
translateSrmc _options (SrmcModel sDefs mainComp) =
  MainControl.valueResult pepaTree logKey logInfo
  where
  logKey  = "translated-models"
  logInfo = unlines $ map printTranslatedModel pepaTree
  printTranslatedModel :: TranslatedModel -> String
  printTranslatedModel tModel =
    unlines [ "------------------"
            , PepaPrint.hprintPepaModel $ slatedModel tModel
            ]
  scopedDefs  = scopeDefinitions Nothing emptyScope sDefs

  -- I think we must build a tree of name space set because
  -- some sets need only have a choice made if their parent
  -- set is itself chosen from a set. Although we could hack
  -- it for now. Also that means that one particular set
  -- may be ranged over more than once, because it is chosen
  -- in two different sets. So we cannot just pick them all
  -- out and permute all the sets and be done with it.
  -- So I think we have to build a tree of sets.

  -- I will make the simplifying assumption that all of the
  -- name space sets in any given name space are used if
  -- the given name space is selected. In particular this means
  -- that all of the top level name space sets are used.
  
  -- With this 'tree' of name space selections we can then build
  -- each individual PEPA model (which will still contain rate arrays)
  -- in a 'tree' of PEPA models.
  namesTree = buildNameSpaceTree scopedDefs
  pepaTree  = translateNamesTree mainComp scopedDefs namesTree

data NamesTree = 
    NamesNode NameSpace [ (NameSpace, NamesTree) ]
  | NamesLeaf
  deriving Show

hprintNamesTree :: String -> NamesTree -> String
hprintNamesTree indent NamesLeaf          = indent ++ "--no more choices--"
hprintNamesTree indent (NamesNode name maps) =
  unlines $ topLine : (map makeLine maps)
  where
  topLine   = indent ++ (textual name) ++ ":"
  newIndent = replicate (length topLine) ' '
  makeLine :: (NameSpace, NamesTree) -> String
  makeLine (n, tree) = concat [ newIndent
                              , textual n
                              , " ->\n"
                              , hprintNamesTree newIndent tree
                              ]


{-
  This is an early version of which there are a number of deficiencies.
  This first is that all the name spaces must be within the top level.
  The second is that we do not do any ordering to determine whether one
  is only selected after another.
-}
buildNameSpaceTree :: [ SrmcDef ] -> NamesTree
buildNameSpaceTree defs = 
  foldNameDefs nameDefs
  where
  -- We should of course order the name space decisions since some
  -- may depend on others. But currently we're essentially just
  -- not allowing nested name space choosing (among other things).
  nameDefs  = [ (ident, names) | NameSpaceSet ident names <- defs ]

  foldNameDefs :: [(NameSpace, [NameSpace])] -> NamesTree
  foldNameDefs []                     = NamesLeaf
  foldNameDefs ((name, names) : rest) = 
    -- So here all of the others map to the same sub-tree.
    -- This should be the case because some choices will mean
    -- that other choices are not required.
    NamesNode name $ map (\a -> (a, restTree)) names
    where
    restTree = foldNameDefs rest


{- This should be the scoped definitions that are given into 
   this function
-}
translateNamesTree :: ParsedComponent
                   -> [ SrmcDef ] 
                   -> NamesTree 
                   -> [ TranslatedModel ]
translateNamesTree mainComp srmcDefs namesTree =
  map translateDefs $ getProcessDefs namesTree
  where
  -- From 'getProcessDefs' we return the srmc definitions to be included
  -- in the model produced. We also include the list of name space
  -- choices we have made to get those definitions. The reason being
  -- that we will return those as a part of the translated model
  -- returned (from 'translateNamesTree'. This allows ultimately for
  -- the filename to be related to the name space choices that have
  -- been made.
  getProcessDefs :: NamesTree -> [ ([ NameSpaceChoice ], [ SrmcDef ] ) ]
  getProcessDefs NamesLeaf                   = [ ([], []) ]
  getProcessDefs (NamesNode current choices) =
    concat choiceResults
    where
    choiceResults = map processChoice choices
    processChoice :: (NameSpace, NamesTree) 
                  -> [ ([NameSpaceChoice], [ SrmcDef ]) ]
    processChoice (name, subtree) =
      map adjoin subResults 
      where
      levelDefs      = theseDefs ++ theseAliases
      theseAliases   = mapMaybe makeAlias theseDefs
      theseDefs      = findNameSpace name
      subResults     = getProcessDefs subtree

      -- Afix the results of this level to the results from
      -- a sub-level.
      adjoin :: ([NameSpaceChoice], [ SrmcDef ]) 
             -> ([NameSpaceChoice], [ SrmcDef ])
      adjoin (names, definitions) = 
        ((current, name) : names, levelDefs ++ definitions)

      makeAlias :: SrmcDef -> Maybe SrmcDef
      makeAlias (ProcessSet pident _comps) =
        Just $ ProcessSet alias [ IdProcess pident ]
        where
        alias     = NameSpaceId (textual current) localPart
        localPart = Qualified.localName pident
      makeAlias (VirtualDef pIdent _rExpr) =
        Just $ VirtualDef alias (Rates.Cident pIdent)
        where
        alias     = NameSpaceId (textual current) localPart
        localPart = Qualified.localName pIdent
      makeAlias _ = Nothing


  -- This is a flat list of all the srmc definitions within
  -- the original list. So this is essentially moving all
  -- nested definitions into one large list of definitions
  allDefinitions :: [ SrmcDef ]
  allDefinitions = getAllDefinitions srmcDefs
  getAllDefinitions :: [ SrmcDef ] -> [ SrmcDef ]
  getAllDefinitions definitions =
    definitions ++ nestedDefs
    where
    nestedDefs = concat [ getAllDefinitions innerDefs |
                          ServiceDef _name innerDefs <- definitions ]

  -- These are all the space definitions within the flattened
  -- list of all definitions within the (scoped) srmc model.
  allSpaceDefs = [ (name, innerDefs) | 
                   ServiceDef name innerDefs <- allDefinitions ]
  allRateDefs  = [ r | r@(RateSet _ _) <- allDefinitions ]

  -- So I think then for each path through the tree, we just
  -- generate a model by bringing in all the definitions 
  -- from that namespace. Then we must also bring in all
  -- the definitions from the top namespace.
  -- We should also watch out for any used names space which are
  -- not used within a name space set but are referred to.
  -- Finally we also need to change the definitions which refer
  -- to a name space which is a set.
  findNameSpace :: NameSpace -> [ SrmcDef ]
  findNameSpace name 
    | Just idefs           <- lookup name allSpaceDefs =
      idefs
      -- Note that this means you cannot lookup a namespace id
      -- that was equal to a name space set.
    | otherwise                                        =
      error $ "Unknown name: " ++ (textual name)


  -- The top level process definitions are always in use
  -- since there is no way to 'unchoose' them.
  topLevelProcessDefs = [ (pident, head comps) |
                          ProcessSet pident comps <- srmcDefs ]
  -- And similarly so for the virtual definitions
  topLevelVirtualDefs = [ (pident, rexpr) |
                          VirtualDef pident rexpr <- srmcDefs ]

  translateDefs :: ( [ NameSpaceChoice ], [ SrmcDef ]) -> TranslatedModel
  translateDefs (nameChoices, definitions) =
    TModel { slatedModel       = pepaModel
           , slatedRates       = rateArrays
           , slatedProcesses   = []
           , slatedNamesChosen = nameChoices
           }
    where
    pepaModel = ParsedModel { modelRateSpecs    = usedRateSpecs
                            , modelProcessDefs  = processDefs
                            , modelVirtualComps = virtualDefs
                            , modelSystemEqn    = mainComp
                            }

    -- We should provide only the rate definitions of those rates
    -- which are used within this particular model.
    -- Todo this we simply ask for all the rates used in the chosen
    -- process definitions and then filter out those rates which are
    -- not used. Note we also need to check the rates used in the
    -- system component although it is unusual to actually use a rate
    -- there it is common to use a rate name in a process concentration
    rateArrays        = filter ( (1 <) . length . snd) usedRateDefs
    usedRateSpecs     = map (second head) usedRateDefs
    -- All the rate definitions which are actually used, these are still as
    -- srmc rate definitions hence they may be in arrays.
    usedRateDefs      = [ (rident, exprs) | 
                          RateSet rident exprs <- allRateDefs
                          , Set.member rident usedRateNames
                        ]
    usedRateNames     = Set.union (Analysis.ratesUsedInProcessDefs processDefs)
                                  (Analysis.ratesUsedInComponent mainComp)
    -- All the rates defined
    -- rateSpecs         = [ (rident, head exprs) | 
    --                       RateSet rident exprs <- allRateDefs ]
    -- Note that the top level process defs are always added
    -- to the model since these are always in use.
    processDefs       = chosenProcessDefs ++ topLevelProcessDefs
    chosenProcessDefs = [ (pident, head comps) |
                          ProcessSet pident comps <- definitions ]
    -- In the same manner as with process definitions the top level
    -- virtual components are always added
    virtualDefs       = chosenVirtualDefs ++ topLevelVirtualDefs
    chosenVirtualDefs = [ (pident, rexpr) |
                          VirtualDef pident rexpr <- definitions ]

  

{- The type of a namespace is quite straight-forward -}
type NameSpace = QualifiedName
{- The type of a scope used to scope the model 
   such that all names are unique 
-}
type Scope = Map String QualifiedName
emptyScope :: Scope
emptyScope = Map.empty




{-
   Takes a set of definitions and creates a scope in which they should
   be themselves scoped.
   It may seem strange that we separate out the functionality of
   'createDefinitionsScope' and 'convertDefinitions' however since the
   main system component must be scoped we require a way to calculate
   the scope without converting any definitions.
-}
createDefinitionsScope :: Maybe NameSpace -> Scope -> [ SrmcDef ] -> Scope
createDefinitionsScope  prefix scope sDefs =
  foldl addDefinitions scope sDefs
  where
  addDefinitions :: Scope -> SrmcDef -> Scope
  addDefinitions thisScope (ServiceDef ident _)   =
    Map.insert (textual ident) (attachModuleName prefix ident) thisScope
  addDefinitions thisScope (NameSpaceSet ident _) =
    Map.insert (textual ident) (attachModuleName prefix ident) thisScope
  addDefinitions thisScope  (ProcessSet ident _)  =
    Map.insert (textual ident) (attachModuleName prefix ident) thisScope
  addDefinitions thisScope (RateSet ident _)      =
    Map.insert (textual ident) (attachModuleName prefix ident) thisScope
  addDefinitions thisScope (VirtualDef ident _)   =
    Map.insert (textual ident) (attachModuleName prefix ident) thisScope

attachModuleName :: Maybe QualifiedName -> QualifiedName -> QualifiedName
attachModuleName Nothing ident                           = 
  ident
attachModuleName (Just (NameSpaceId longid qname)) ident =
  NameSpaceId longid $ attachModuleName (Just qname) ident
attachModuleName (Just qname) ident                      =
  NameSpaceId (textual qname) ident

scopeDefinitions :: Maybe NameSpace -> Scope -> [ SrmcDef ] -> [ SrmcDef ]
scopeDefinitions prefix existingScope definitions =
  map scopeDefinition definitions
  where
  thisScope = createDefinitionsScope prefix existingScope definitions
  scopeDefinition :: SrmcDef -> SrmcDef
  scopeDefinition (ServiceDef ident sDefs)   =
    ServiceDef newName newDefinitions
    where
    newName        = scopeIdentifier thisScope ident
    newPrefix      = Just newName
    newDefinitions = scopeDefinitions newPrefix thisScope sDefs
  scopeDefinition (NameSpaceSet ident names) =
    NameSpaceSet newName newNames
    where
    newName        = scopeIdentifier thisScope ident
    newNames       = map (scopeIdentifier thisScope) names
  scopeDefinition (ProcessSet ident comps)   =
    ProcessSet newName newComps
    where
    newName        = scopeIdentifier thisScope ident
    newComps       = map (scopeProcess thisScope) comps
  scopeDefinition (RateSet ident rates)      =
    RateSet newName newRates
    where
    newName        = scopeIdentifier thisScope ident
    newRates       = map (scopeRateExpression thisScope) rates
  scopeDefinition (VirtualDef ident rate)    =
    VirtualDef newName newRate
    where
    newName        = scopeIdentifier thisScope ident
    newRate        = scopeRateExpression thisScope rate

scopeIdentifier :: Scope -> QualifiedName -> QualifiedName
scopeIdentifier scope (NameSpaceId namespace qname) =
   case Map.lookup namespace scope of
      Nothing     -> error $ unwords [ "Namespace:"
                                     , namespace
                                     , "used out of scope"
                                     ]
      Just longid -> attachModuleName (Just longid) qname
scopeIdentifier scope ident                         =
  case Map.lookup (textual ident) scope of
    Nothing     -> error $ unwords [ "Used name:"
                                   , textual ident
                                   , "not in scope"
                                   ]
    Just longid -> longid

scopeRate :: Scope -> ParsedRate -> ParsedRate
scopeRate _scope r@(RateTop _)       = r
scopeRate _scope r@(RateImmediate _) = r
scopeRate scope  (RateTimed expr)    = 
   RateTimed $ scopeRateExpression scope expr

{-
   Scopes a rate expression. During the conversion rate names
   and process names will be renamed with a prefix indicating
   in which name space they are in. Hence rate expressions which
   use those scoped rate and process names must change the names
   to which they refer.
-}
scopeRateExpression :: Scope -> RateExpr -> RateExpr
scopeRateExpression scope =
  transformCExp $ namedRateExp . (scopeIdentifier scope)

{-
   Scopes a process including the rate expressions contained within
-}
scopeProcess :: Scope -> ParsedComponent -> ParsedComponent
scopeProcess scope  (IdProcess ident)                    =
   IdProcess $ scopeIdentifier scope ident
scopeProcess _scope p@(StopProcess)                      = 
   p
scopeProcess scope (PrefixComponent trans next) =
   PrefixComponent newTrans (scopeProcess scope next)
   where
   newTrans = trans { pepaTransRate = newRate }
   newRate  = scopeRate scope $ pepaTransRate trans
scopeProcess scope (CondBehaviour cond next)             =
   CondBehaviour (scopeRateExpression scope cond)
                 (scopeProcess scope next)
scopeProcess scope (ComponentSum left right)             =
   ComponentSum (scopeProcess scope left)
                (scopeProcess scope right)
scopeProcess scope (Cooperation left actions right)      =
   Cooperation (scopeProcess scope left)
               actions
               (scopeProcess scope right)
scopeProcess scope (ProcessArray comp size actions)      =
   ProcessArray (scopeProcess scope comp)
                newSize
                actions
   where
   newSize = scopeRateExpression scope size
scopeProcess scope (Hiding comp actions)                 =
   Hiding (scopeProcess scope comp) actions
