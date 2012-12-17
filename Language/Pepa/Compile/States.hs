{-# LANGUAGE PatternGuards #-}
{-
-}
module Language.Pepa.Compile.States
  ( getModelStateSpace
  , removeImmediateStates
  , stateSpaceSize
  , stateSpaceToDot

  , GenerateOptions     ( .. )
  , defaultGenerateOptions
  , StateSpace          ( .. )
  , allStatesInSpace
  , findStateRepr
  , State               ( .. )
  , StateId
  , stateName
  , stateNumberInt
  , stateIdInt
  , stateIndex
  , stateIdIndex
  , indexStateId
  , StateMovements
  , StateMove
  , StateConcentrations
  , StateCondition
  , pprintStateCondition


  , gatherStateConcentrations
  , getInitialConcentrations

  , getInitialState
  , getSourceStates
  , getTargetStates
  , outsideConditionSatisfied
  , conditionSatisfied
  , getSatisfyingStates
  , findTargetedStates

  , displayState
  , stateReprName
  , isAbsorbing
  , moveAction
  , moveRate
  , moveCoalsced
  , moveTarget
  )
where

{- Standard Library Modules Imported -}
import Control.Arrow
  ( second )
import qualified Data.List as List
import Data.Maybe
  ( mapMaybe )
import Data.Map 
  ( Map )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set
  ( Set )
-- import qualified Text.PrettyPrint.HughesPJ as Pretty
import Text.PrettyPrint.HughesPJ
  ( Doc )
{- External Library Modules Imported -}
{- Local Modules Imported -}
import qualified Language.Pepa.QualifiedName as Qualified
import Language.Pepa.QualifiedName
  ( ShowOrig         ( .. )  )
import qualified Language.Pepa.Print as Print
import Language.Pepa.Print
  ( hprintParsedAction )
import Language.Pepa.Syntax
  ( ParsedModel       ( .. )
  , ParsedAction      ( .. )
  , ActionIdentifier
  , ParsedTrans
  , Transition        ( .. )

  , defaultPepaPriority
  , nameOfAction
  , actionNameOfTrans
  )
import Language.Pepa.Rates
  ( Rate              ( .. )
  , RateIdentifier
  , RateValue
  , RateNumber
  , RateExpr
  
  , scaleRate
  , sumRates
  , apparentRate

  , andRateConditions
  , reduceRate
  , reduceRateExprWith
  , reduceRateExpr

  , isImmediateRate
  )
import Language.Pepa.Compile.Model
  ( Model
  , Parallel       ( .. )
  , Component
  , Transitions
  , Derivative

  , pepaToModel
  , derivativesOfModel
  )
import qualified Language.Pepa.MainControl as MainControl
import Language.Pepa.MainControl
  ( MainControl )
import qualified Language.Pepa.Utils as Utils
{- End of Imports -}

{-|
  GenerateOptions is a data structure containing the options for
  state space generation such as the limit on the size of the
  state space and whether or not to allow self looping activities.
-}
data GenerateOptions =
  GenerateOptions { genOptsLimit          :: Maybe Int
                    -- ^ The limit on the size of the state space
                    --   generated if there is one.

                  , genOptsAllowSelfLoops :: Bool
                    -- ^ Whether we should fail if we detect there
                    --   to be a self-loop.
                  , genOptsAllowDeadLocks :: Bool
                    -- ^ Whether we should allow deadlocked states
                    --   generally only useful for transient anlaysis
                  }

defaultGenerateOptions :: GenerateOptions
defaultGenerateOptions =
  GenerateOptions { genOptsLimit          = Just 6000
                  , genOptsAllowSelfLoops = True
                  , genOptsAllowDeadLocks = True
                  }

{-|
  The main exported function from this module. Generate a state space
  from the given parsed model. Do not generate a state space larger than
  the given limit.
-}
getModelStateSpace :: GenerateOptions -- ^ Options controlling space generation
                   -> ParsedModel     -- ^ The model to explore
                   -> MainControl StateSpace
getModelStateSpace genOptions parsedModel =
  do model <- pepaToModel parsedModel
     space <- generateStateSpace genOptions  model
     let logKey  = "state-space"
         logInfo = showStateSpace space
     MainControl.valueResult space logKey logInfo 

{-| Translate the state space into a string containing the contents
    of a graphviz .dot file. Note that there is a library on hackage
    for manipulating .dot files which I should have a look at.
-}
stateSpaceToDot :: StateSpace -> String
stateSpaceToDot space =
  unlines allLines
  where
  allLines    = concat [ headerLines, stateLines, moveLines, [ "}" ] ]
  headerLines = [ "digraph ProbeName {"
                , "size = \"4,4\" ;"
                , "rankdir = LR ; "
                , ""
                ]
  stateLines     = map makeStateLine tangibleStates
  moveLines      = concatMap makeMoveLines tangibleStates
  tangibleMap    = spaceTangible space
  tangibleStates = Map.elems tangibleMap

  
  makeStateLine :: State -> String
  makeStateLine state =
    unwords [ "\"" ++ (show $ stateNumberInt state) ++ "\""
            , "[ shape = circle , color = red ] ;"
            ]


  makeMoveLines :: State -> [ String ]
  makeMoveLines state =
    map makeTargetLine $ stateMovements state
    where
    sourceNumber = stateNumberInt state
    makeTargetLine :: Child -> String
    makeTargetLine (transition, targetrep) =
      case Map.lookup targetrep tangibleMap of
        Just targetState -> 
          makeMoveLine sourceNumber (stateNumberInt targetState)
                                    (showOrig $ actionNameOfTrans transition)
        Nothing           -> 
          error "SpaceToDot: Serious internal flaw, state not found"
        
    makeMoveLine :: Int -> Int -> String -> String
    makeMoveLine i j name =
      unwords [ "\"" ++ show i ++ "\""
              , "->"
              , "\"" ++ show j ++ "\""
              , "[ label = \"" ++ name ++ "\" ] ; "
              ]


data StateSpace = StateSpace { spaceTangible   :: StateReprMapping
                             , spaceTangIds    :: Map StateId State
                             , spaceVanishing  :: StateReprMapping
                             , spaceInitial    :: StateRepre
                             , spaceAllDerivs  :: Set Derivative
                             }
                             deriving Eq

type StateReprMapping = Map StateRepre State


{-| Finds a given state representation in a state space,
    looking in first the tangible map and then the vanishing map
-}
findStateRepr :: StateSpace -> StateRepre -> Maybe State
findStateRepr statespace stateRepr =
  maybe (Map.lookup stateRepr $ spaceVanishing statespace)
        Just
        (Map.lookup stateRepr (spaceTangible statespace))
  
  


{-| 
   The full representation of a state, including the state representation
   and its state identifying number.
  
   It's maybe a little unnecessary to have the state representation here
   since these 'State's will be stored in a mapping from their state
   representation to a 'State'.
   NOTE: I would actually like the movements of a tangible state to contain
   only double rather than rates.
-}
data State      = Tangible { stateRep            :: StateRepre
                           , stateMovements      :: Children
                           , stateNumber         :: StateId
                           , stateConcentrations :: StateConcentrations
                           }
                | Vanishing { stateRep            :: StateRepre
                            , stateMovements      :: Children
                            , stateNumber         :: StateId
                            , stateConcentrations :: StateConcentrations
                            }
                           deriving (Show, Read, Eq)

allStatesInSpace :: StateSpace -> [ State ]
allStatesInSpace statespace =
  concat [ Map.elems $ spaceTangIds statespace
         , Map.elems $ spaceVanishing statespace
         ]

{-| The type of a state identifying number -}
newtype StateId = StateId Int
                  deriving (Eq, Ord, Show, Read)

stateNumberInt :: State -> Int
stateNumberInt = stateIdInt . stateNumber

stateIdInt :: StateId -> Int
stateIdInt (StateId i) = i

stateName :: State -> Qualified.QualifiedName
stateName = Qualified.unqualified . ("state_" ++) . show . stateNumberInt

{-|
  Return the stateId as an index with the given starting point.
-}
stateIdIndex :: Int -> StateId -> Int
stateIdIndex offset (StateId i) = i + (offset - stateIndex)
  -- Note that: if x == stateIndex then i else i + (x - stateIndex)
  -- is equivalent to the above.

{-|
  The number at which we begin state indicies 
-} 
stateIndex :: Int
stateIndex = 1

{-| The converse of 'stateIdIndex' the first argument is the
    offset of the index given and the second is the index
    to be converted
-}
indexStateId :: Int -> Int -> StateId 
indexStateId offset i = StateId (i + (stateIndex - offset))


displayState :: State -> String
displayState state =
  List.intercalate "\n" $ 
  (show $ stateNumberInt state) : indentedLines
  where
  indentedLines = Utils.indentLines concLines
  concLines =  map showConc $ Map.toList (stateConcentrations state)
  showConc :: (Derivative, Int) -> String
  showConc (deriv, i) = unwords [ showOrig deriv, "=", show i ]

{-| Display a state representation as a name that could be used in a 
    string literal name. This is mostly for partial evaluation.
-}
stateReprName :: StateRepre -> String
stateReprName (RepCoop left actions right) =
  unwords [ stateReprName left
          , if null actions then "||" else "<L>"
          , stateReprName right
          ]
stateReprName (RepArray mapping actions)
  | null subStates = error "No active components in array"
  | otherwise      = foldr1 cooperate subStates
  where
  coopOp        = if null actions then "||" else "<L>"
  cooperate l r = unwords [ l, coopOp, r ]
  subStates     = map arrayName nonZeros
  nonZeros      = filter (\(_, i) -> i /= 0) $ Map.toList mapping
  arrayName (s, i) = concat [ stateReprName s, "[", show i, "]" ]

stateReprName (RepSeq ((deriv, _), _))     =
  Qualified.textual deriv

data StateRepre = RepCoop StateRepre [ ActionIdentifier ] StateRepre
                | RepArray (Map StateRepre Int) [ ActionIdentifier ]
                | RepSeq ((Derivative, Transitions), Component)
                deriving (Show, Read, Eq, Ord) 
                -- But maybe I can write a faster version of Ord?

type Child      = (Transition RateValue, StateRepre)
type Children   = [ Child ]

type StateMove      = Child
type StateMovements = Children

-- A state condition is just the same as a rate expression except that we know
-- that it should evaluate to true or false. If/when we 'type' rate expressions
-- into 'rate' and 'bool' expression then this will of course be a
-- boolean expression.
type StateCondition = RateExpr

pprintStateCondition :: StateCondition -> Doc
pprintStateCondition = Print.pprintRateExpr

type Action         = ParsedAction

-- | Returns the action identified with a movement
moveAction :: StateMove -> Action
moveAction = pepaTransAction . fst

-- | Returns the rate involved in a state movement
moveRate :: StateMove -> RateValue
moveRate = pepaTransRate . fst

-- | Returns the target of the given state movement
moveTarget :: StateMove -> StateRepre
moveTarget = snd

-- | Return the coalsced immediate actions from a movement
moveCoalsced :: StateMove -> [ Action ]
moveCoalsced = pepaTransCoalsced . fst

-- | returns whether or not a 'State' is an absorbing state
-- that is a deadlocked state.
isAbsorbing :: State -> Bool
isAbsorbing = null . stateMovements

{-| Pretty print the state space, useful for a log or such -}
showStateSpace :: StateSpace -> String
showStateSpace space =
  sizeInfo ++ tstateLines ++ vstateLines
  where
  sizeInfo       = concat [ "There are: "
                          , show tangLength
                          , " tangible states\n"
                          , "There are: "
                          , show vanishLength
                          , " vanishing states\n"
                          ]

  tangLength     = Map.size tangible
  tangible       = spaceTangible space
  vanishLength   = Map.size vanishing
  vanishing      = spaceVanishing space

  tstateLines     = List.intercalate "\n" $ 
                    map (printState 0) (Map.toList tangible)
  vstateLines     = List.intercalate "\n" $ 
                    map (printState tangLength) (Map.toList vanishing)

  -- The integer is the number to add to the identifier of an immediate
  -- to get its state number. Essentially this should be 0 for tangibles
  printState :: Int -> (StateRepre, State) -> String
  printState offset (_staterep, state) =
    List.intercalate "\n" $ 
    (show $ offset + stateNumberInt state) : indentedLines
    where
    indentedLines = map ("    " ++) (concLines ++ moveLines)

    concLines = map showConc $ Map.toList (stateConcentrations state)
    moveLines = map printMovement $ stateMovements state
      
    showConc :: (Derivative, Int) -> String
    showConc (deriv, i) = unwords [ showOrig deriv, "=", show i ]

  printMovement :: Child -> String
  printMovement (transition, srep) =
    case Map.lookup srep tangible of
      Just state  -> unwords [ hprintParsedAction $ 
                               pepaTransAction transition
                             , "@"
                             , show $ pepaTransRate transition
                             , "-->"
                             , show $  stateNumberInt state]
      Nothing     -> 
        case Map.lookup srep vanishing of
          Just state -> unwords [ hprintParsedAction $  
                                 pepaTransAction transition
                                , "@"
                                , show $ pepaTransRate transition
                                , "-->"
                                , show $ tangLength + (stateNumberInt state) ]
          Nothing    -> error "Lost state representation"


{-
  The concentrations of a state is mapping from the name of a derivative
  to the number of components in that state. Any component not present
  in the mapping is assumed to be zero.
-}
type StateConcentrations = Map Derivative Int

gatherStateConcentrations :: StateRepre -> StateConcentrations
gatherStateConcentrations (RepCoop l _actions r)      =
  Map.unionWith (+) (gatherStateConcentrations l)
                    (gatherStateConcentrations r)
gatherStateConcentrations (RepArray mapping _actions) =
  -- for each state => concentration mapping we create a 
  -- state concentration mapping. We therefore have a bunch of
  -- mappings which we union together adding similar entries.
  -- In general of course arrays will hopefully be of sequential
  -- mapping so this will all be quite simple.
  -- Note that we might try to do the multiplication by the current
  -- concentration at the same time as the addition, but this won't
  -- work because the function passed into 'Map.unionsWith' is only
  -- called if both maps have an entry for the key.
  Map.unionsWith (+) $ map makeLocalConcs (Map.toList mapping)
  where
  -- To make a concentration mapping from a single
  -- (staterep, concentration) pair we just recursively call
  -- 'gatherStateConcentrations' and then multiply each entry
  -- by the number of the given staterepre's in the array.
  makeLocalConcs :: (StateRepre, Int) -> StateConcentrations
  makeLocalConcs (srep, i) = Map.map (i *) $ gatherStateConcentrations srep
gatherStateConcentrations (RepSeq ((deriv, _trans), _)) =
  -- Note we do not put the other derivatives into the mapping at zero.
  -- For bullet proof code we could do that and then later when looking
  -- up an entry if 'Nothing' is returned we throw a wobbly (or error).
  -- But I think our static analysis should take care of that and doing so
  -- would be a significant performance penalty.
  Map.singleton deriv 1


{- Actually 'Seen' should probably just be the state space -}
type Seen  = StateSpace
type Stack = Set StateRepre

{-| Returns the combined size of the state space -}
stateSpaceSize :: StateSpace -> Int
stateSpaceSize space =
  (Map.size $ spaceTangible space) +
  (Map.size $ spaceVanishing space)

{- Returns true if the given state representation occurs in the state space -}
haveSeen :: StateRepre -> Seen -> Bool
haveSeen s seen = 
  (Map.member s $ spaceTangible seen) ||
  (Map.member s $ spaceVanishing seen)


getInitialStateRepr :: Model -> StateRepre
getInitialStateRepr (Cooperate left actions right)    =
  RepCoop (getInitialStateRepr left) actions (getInitialStateRepr right)
getInitialStateRepr (CompArray comp i actions)        =
  RepArray (Map.singleton (getInitialStateRepr comp) i) actions
getInitialStateRepr (Sequential initialDeriv mapping) =
  RepSeq (initialDeriv, mapping)

getInitialConcentrations :: Model -> StateConcentrations
getInitialConcentrations = gatherStateConcentrations . getInitialStateRepr

{-
  A note on memoisation. Initially I had tried to do some memoisation
  of a part of the tree. To do this I defined a child map as:
  type ChildMap = Map StateRepre Children
  Then 'getChildren' simultaneously built up the child map it looked
  like this:
  getChildren :: ChildMap -> StateRepre -> (ChildMap, Children)
  getChildren childMap stateRepr
    | Just children <- Map.lookup stateRepr childMap = (childMap, children)
    | otherwise                                      =
      (Map.insert stateRepr children childMap, children)
      children = calculateChildren ....

  I now think that this is a very questionable idea. It's possible that
  this saving is not as large as it appears and may even do harm
  since we may have a very large storage space.

  However memoisation is currently disabled for another reason.
  That reason is functional rates and conditional behaviour.
  This means that a sub-tree of a state representation may have different
  children than it had before the first time we encountered it because
  now the whole state concentrations are different.
  For this reason memoisation is currently disabled.

  So for example if we are computing the state space of:
  P || (R <..> S)
  and we compute
  P1 || (R <..> S)
  we do not really wish to recompute the children of
  (R <..> S) a second time.
  Unfortunately we must because it might be that we have
  R = (a, P*2) . R1
  or even
  R = if P then (a, r).R1
  So if we were to do memoisation we would have to keep account
  of under which conditions the memoisation was safe.
-}
generateStateSpace :: GenerateOptions   -- ^ Options (eg size limit)
                   -> Model             -- ^ Model to generate from
                   -> MainControl StateSpace
generateStateSpace genOptions model =
  explore emptyStateSpace initialStack
  where
  initialStack    = Set.singleton initialState
  initialState    = getInitialStateRepr model
  emptyStateSpace = StateSpace { spaceTangible  = Map.empty
                               , spaceTangIds   = Map.empty
                               , spaceVanishing = Map.empty
                               , spaceInitial   = initialState
                               , spaceAllDerivs = derivativesOfModel model
                               }

  -- From here we should return a MainControl, because from here
  -- is the best place to check for an absorbing state, rather than
  -- in GenMatrix.
  explore :: Seen -> Stack -> MainControl StateSpace
  explore seen currentStack
    | Set.null currentStack  = return seen
    | overLimit              =
      MainControl.resultError "Sorry your state-space size is too large for me"
      -- If there are no immediate children then we have a tangible state
      -- (even if there are no tangible children either in which case we
      -- have a deadlocked state, but we deal with that below)
    | null immediateChildren =
      exploreTangible
      -- Okay if there are immediate children then forget about any timed
      -- children and create our new vanishing state.
    | otherwise              =
      exploreVanishing
    where
    -- The condition for whether or not we should stop here or continue
    -- This is true if we are over the limit and should stop.
    -- This relies on only pushing elements onto the stack if they
    -- are not on the state. Of course it is a set anyway so this
    -- should happen by default.
    overLimit = 
      case genOptsLimit genOptions of
        Just limit -> ((stateSpaceSize seen) + (Set.size stack)) > limit
        Nothing    -> False

    -- I define down here two values, exploreTangible and exploreVanishing.
    -- I would like to define them above within the guards but it seems you
    -- can't have a 'where' clause within a guard
    -- (it has to apply to all the guard expressions, shame).
    -- For either we need to calculate the new seen and the new stack.
    exploreTangible =
      do result <- explore newSeen newStack
         -- If there are in fact no children (tangible or otherwise)
         -- then we have a deadlocked state. 
         -- If we have explicitly allowed deadlocked states
         -- (or implictly by say doing transient analysis)
         -- then we continue as if we didn't detect a deadlocked state
         -- otherwise we have to warn about it.
         -- TODO: not quite sure about the efficiency of this, because
         -- we essentially have to do all the recursion before adding
         -- the warning.
         if null children && (not $ genOptsAllowDeadLocks genOptions)
            then MainControl.resultWarning result 
                                           [ deadlockWarning ]
                                           "state-space-generator"
                                           "" -- no extra log informatio
            else return result
      where
      newSeen      = seen { spaceTangible = newTangible
                          , spaceTangIds  = newTangIds
                          }
      -- We must update the mapping from state representations so that
      -- we can quickly convert a state representation to a state
      -- (and more importantly we can quickly check if we have seen a
      --  given state representation)
      newTangible  = Map.insert current newState tangibleSeen
      -- We must also update the mapping from state number to state so
      -- that we can quickly lookup a given state number.
      newTangIds   = Map.insert newNumber newState $ spaceTangIds seen
      tangibleSeen = spaceTangible seen
      newState     = Tangible { stateRep            = current
                                -- We are only here if there are no immediate
                                -- children and hence we can just use all
                                -- the children
                              , stateMovements      = children
                              , stateNumber         = newNumber
                              , stateConcentrations = concentrations
                              }

      newNumber    = makeNewNumber tangibleSeen
      -- For a tangible state we push all the children onto the stack
      -- since all the children must be timed transitions.
      newStack = foldl pushOnStack stack children
      -- This is of course only used if 'children' is null and
      -- we have not explicitly allowed deadlocks
      deadlockWarning = unlines [ "The following state is a deadlocked state:"
                                , displayState newState
                                ]
    -- OKAY, so if the NEW number is the current size of the stack
    -- then that means we begin indexing states from zero.
    -- To begin indexing from one, then we want to add one to this
    -- We add 1 here, so that the initial state is state number 1.
    -- So here we add whatever the 'stateIndex' is.
    makeNewNumber :: StateReprMapping -> StateId
    makeNewNumber = StateId . (stateIndex +) . Map.size

    exploreVanishing =
      explore newSeen newStack
      where
      newSeen          = seen { spaceVanishing = newVanishingSeen }
      newVanishingSeen = Map.insert current newState vanishingSeen
      vanishingSeen    = spaceVanishing seen
      -- I think it would be nice to actually have a separate represention
      -- of the 'stateMovements' here where for example instead of a rate
      -- we have a weight, which can only mean one thing and that is a weight.
      -- We may even think of doing the same thing for tangible states above
      -- ie not have a full rate, but only a rate value which can only mean
      -- a timed rate.
      newState         = Vanishing { stateRep            = current
                                   , stateMovements      = immediateChildren
                                   , stateNumber         = newNumber
                                   , stateConcentrations = concentrations
                                   }
      newNumber        = makeNewNumber vanishingSeen
      -- This time of course we only push the immediate children on to the
      -- stack, we do not wish to push the targets of timed transitions on
      -- to the stack since these timed transitions will never be taken.
      newStack = foldl pushOnStack stack immediateChildren
    -- This is basically just popping something from the stack
    -- 'current' is the state we will find the children states of
    -- so in otherwords it is the state which we will "explore" this
    -- time around. We of course don't know whether it is a tangible
    -- or vanishing state until we know all of the children (or movements)
    -- we can make from here.
    (current, stack) = Set.deleteFindMin currentStack

    -- Pushing on the stack is the same reguardless of whether we
    -- are adding a tangible or vanishing state, the only difference
    -- is WHAT we push on to the stack.
    -- Notice as well that we do not blindly push items on the stack
    -- but add items only if they are not currently on the stack otherwise
    -- our test for whether we are over the limit of state space size would
    -- be incorrect since the stack may contain duplicates which takes
    -- it over the edge of limit.
    pushOnStack :: Stack -> Child -> Stack
    pushOnStack givenStack child
      | s == current            = selfLoopAnswer
      | haveSeen s seen         = givenStack
      | Set.member s givenStack = givenStack
      | otherwise               =  Set.insert s givenStack
      where s = moveTarget child
            selfLoopAnswer
              | genOptsAllowSelfLoops genOptions = givenStack
              | otherwise                        = error "Self-loop detected"

    -- Note that we do not calculate an equivalent 'timedChildren' we can
    -- just instead use 'children' since we only use the 'timedChildren' if
    -- there are no immediate children in which case children == timedChildren
    immediateChildren       = filter (isImmediateRate . moveRate) children
    -- This is all of the children of the current state which we are exploring.
    children       = getChildren current
    concentrations = gatherStateConcentrations current

    -- Note also that this may, in some sense, cause us to violate
    -- our 'stop-at-limit' policy, since we do not check on how many
    -- children there are.
    -- Actually may be wrong about that, perhaps the length checks above
    -- together with lazyness is enough, worth checking on that though.
    getChildren :: StateRepre -> Children
    getChildren (RepCoop left actions right) =
      allChildren
      where
      -- So I should really do this with a monad which is why I'm
      -- these two are up here (they should be above the result)
      leftChildren  = getChildren left
      rightChildren = getChildren right
     
      -- So all the children consist of both the moves involved in the
      -- the cooperation and those that are not.
      allChildren = cooperating ++ nonCooperating
      -- For each activity in the cooperation set, we find the group
      -- of transitions done by the left side and done by the right
      -- side and then combine each such into a combined transition
      -- with the appropriate rate.
      cooperating    = concatMap makeCoopActions actions
      makeCoopActions :: ActionIdentifier -> Children
      makeCoopActions action =
         [ ( constructSharedTrans lTrans rTrans
           , RepCoop l actions r)
           | (lTrans, l) <- appropriateLeft
           , (rTrans, r) <- appropriateRight
         ]
         where
         -- So the shared trans from a left and right cooperation we just
         -- take the left transitions and modify the rate to be the
         -- apparent rate of the given transition over this action.
         -- This means we must be careful in that for example we have 
         -- lost the priority and conditions of the right hand transition.
         constructSharedTrans :: Transition RateValue -> Transition RateValue
                              -> Transition RateValue
         constructSharedTrans lTrans rTrans =
            lTrans { pepaTransRate = rate }
            where
            rate  = apparentRate rateL rateR raP raQ
            rateL = pepaTransRate lTrans
            rateR = pepaTransRate rTrans
         -- Note that 'sumRates' will fail on the empty list, but that
         -- neither of raP and raQ can be null because in that case
         -- there wouldn't be an r1 and r2 on which to call this.
         -- That is, r1 is in appropriateLeft and r2 is in
         -- appropriateRight hence neither one is null.
         raP              = sumRates $ map moveRate appropriateLeft
         raQ              = sumRates $ map moveRate appropriateRight
         appropriateLeft  = filter (performsAction action) leftCoop
         appropriateRight = filter (performsAction action) rightCoop

         
      -- For the derivatives which are not in the cooperation set
      -- these can happen independently and must all be enumerated.
      nonCooperating = [ (lTrans, RepCoop l actions right) |
                         (lTrans, l) <- leftNonCoop
                       ] ++
                       [ (rTrans, RepCoop left actions r) |
                         (rTrans, r) <- rightNonCoop 
                       ]
      (leftCoop,
       leftNonCoop)  = List.partition (isCooperating actions) leftChildren
      (rightCoop,
       rightNonCoop) = List.partition (isCooperating actions) rightChildren

    getChildren (RepArray concMap actions) =
      allChildren
      where
      -- All of the children from this particular state of the array.
      -- If actions is null then of course 'cooperating' will also be null
      allChildren   = nonCooperating ++ cooperating

      -- First map the concentration map (staterep -> Int) into a mapping
      -- from a state representation to (Int, Children). 
      -- In other words, all we are doing here is taking the 'concMap' which
      -- maps local state to their concentrations, and adding to the
      -- concentrations the local children of each local state.
      localChildrenMap    = Map.mapWithKey getLocalChildren concMap
      getLocalChildren :: StateRepre -> Int -> (Int, Children)
      getLocalChildren srep i = (i, getChildren srep)
         
      -- cooperating is all the children where the asssociated action is
      -- in the cooperation set and hence we cannot take any such child
      -- unless all components in the array are in a state which can perform
      -- the given action.
      cooperating    = concatMap makeCoop actions

      -- For each activity in the cooperation set there may be more than
      -- one child, if there is more than one way for any of the components
      -- in the array to currently perform the given activity.
      makeCoop :: ActionIdentifier -> Children
      makeCoop action
        -- Must make sure to return null if 'childLists' is itself
        -- null. From below if any of the lists within childLists are null
        -- then this will halt the further cooperation and we'll return
        -- the empty list here. However if childLists is itself null then
        -- we must be careful to return null since the usual result would
        -- be a self-loop with rate infinity. Because:
        -- (foldl anyFunction [ one ] []) = [ one ]
        | null childLists          = []
        -- This should be allowed but currently we will not calculate the
        -- the rates correctly. Basically if we have N components in state S
        -- and from state S we can perform 'action' in two ways, 
        -- (call them a1 and a2) then we need calculate the possible transitions
        -- where N components perform a1 and zero a2, N-1 perform a1
        -- and 1 performs a2, N-2 perform a1 and 2 perform a2 etc etc.
        -- And we're not doing that, we're only working out the rates for
        -- N perform a1 OR N perform a2.
        | length arrayChildren > 1 = 
          error "Unimplemented: an array with choice over same cooperation"
        | otherwise                =  arrayChildren
        where
        arrayChildren = map createArrayChild localUpdates
        createArrayChild :: (RateValue, Map StateRepre Int) -> Child
        createArrayChild (rate, newconcs) =
          -- Hmm, I'm not 100% happy about just creating a new transition
          -- here, what about the priority and conditions etc??
          ( Transition { pepaTransAction     = Action action
                       , pepaTransCoalsced   = []
                       , pepaTransPriority   = defaultPepaPriority
                       , pepaTransConditions = []
                       , pepaTransRate       = rate
                       }
          , RepArray newconcs actions
          )
        -- Note that if any of the childLists are null then the whole
        -- answer should be null.
        localUpdates = foldl foldf [ (RateTop 1, concMap) ] childLists
        foldf :: [ (RateValue, Map StateRepre Int) ] 
              -> (StateRepre, (Int, Children))
              -> [ (RateValue, Map StateRepre Int) ]
        foldf soFar (currStateRep, (currentConc, localChildren)) =
          -- TODO document this far better
          [ ( apparentRate (moveRate localChild) rightRate raP raQ
            , updateConcentrations currStateRep (moveTarget localChild) curr
            ) |
            localChild         <- relevantLocalChildren
          , (rightRate, curr)  <- soFar
          ]
          where
          -- Note that 'sumRates' will fail on the empty list, but that
          -- neither of raP and raQ can be null because in that case
          -- there wouldn't be an r1 and r2 on which to call this.
          -- That is, r1 is in appropriateLeft and r2 is in
          -- appropriateRight hence neither one is null.
          raP              = sumRates $ map moveRate relevantLocalChildren
          raQ              = sumRates $ map fst soFar
          relevantLocalChildren = filter (performsAction action) localChildren

          updateConcentrations :: StateRepre -> StateRepre 
                           -> Map StateRepre Int
                           -> Map StateRepre Int
          updateConcentrations fromState toState =
             (Map.insertWith (+) toState currentConc) .
             -- It seems like we could just delete the fromState
             -- representation from teh state representation mapping.
             -- However we cannot do this because it may be that
             -- (an earlier processed) local child had this representation
             -- as a target.
             -- consider: P = (a, r) . P2; P2 = (a, r) . P ; P[10][a]
             -- If we have say P = 8, P2 = 2, then we wish to have a
             -- target concentration map of P = 2; P2 = 8.
             -- But if we process the first child first this will give us
             -- an intermediate map of P = 0, P2 = 10, if we then simply
             -- deleted the 'fromState' of the second transition we would
             -- have a final mapping of P = 2 (P2 absent, hence = 0).
             -- So instead we update it by subtracting the current concentation
             -- given P = 2, P2 = 8 as we desired.
             (Map.update subtractConc fromState)
             where
             subtractConc :: Int -> Maybe Int
             subtractConc x =
               case x - currentConc of
                 0 -> Nothing
                 n -> Just n
             -- (Map.delete fromState)

        childLists :: [ (StateRepre, (Int, Children)) ]
        childLists = map getRelevantChildren $ Map.toList localChildrenMap

        -- From the list mapping state representation to a pair of
        -- concentration and children we filter the list of children
        -- to be only those which perform 'action'
        getRelevantChildren :: (StateRepre, (Int, Children)) 
                            -> (StateRepre, (Int, Children))
        getRelevantChildren =
          second (second $ filter (performsAction action))

      nonCooperating = Map.foldWithKey makeNonCoop [] localChildrenMap

      -- makeNonCoop need not return a ChildMap because we have already calculated
      -- the mapping for each individual state repre.
      makeNonCoop :: StateRepre -> (Int, Children) -> Children -> Children
      makeNonCoop srep (conc, localChildren) childrenSoFar
        | conc <= 0  = error "We have a serious problem on our hands"
        --  conc == 0 = childrenSoFar
        | otherwise  = theseChildren ++ childrenSoFar
        where
        theseChildren = map createArrayChild $ 
                        filter (not . (isCooperating actions)) localChildren
        -- For each local child make up a new state representation.
        -- This removes one of the current state representations
        createArrayChild :: Child -> Child
        createArrayChild (transition, newLocalState) =
           ( -- So we basically have the same transition as before
             -- except that we have to scale the rate.
             transition { pepaTransRate = newRate }
             -- The target state representation is of course still
             -- an array but with the concertations updated.
           , RepArray newconcs actions
           )
           where
           -- The rate is the rate of the individual local transition
           -- multiplied by how many array components are in that state
           newRate = scaleRate (fromIntegral conc) $ pepaTransRate transition
           -- The new concentrations are computed by minusing one from
           -- the current local state. And then inserting the new local state
           -- with a concentration of one, however we are careful that if
           -- the new local state is already in the concentrations mapping then
           -- rather than adding it with a concentration of one we simply add
           -- one to the current concentration.
           -- NOTE: that we must delete the old state representation from the
           -- map if its concentration goes to zero, otherwise we will have two
           -- states which are the same but are not equal in the sense of "==".
           -- (One state will have for example P = 0 and the other will have just
           --  no mapping for P). We could write "==" for ourselves but it's
           -- questionable as to what is faster anyway.
           newconcs
             | conc == 1 = Map.insertWith (+) newLocalState 1 $
                           Map.delete srep concMap
             | otherwise = Map.insertWith (+) newLocalState 1 $
                           Map.insert srep (conc - 1) concMap
    getChildren (RepSeq ((_deriv, trans), transMapping)) =
      -- Note here we only return the childrens whose rate is above
      -- zero. This means that the use can set a rate to 0.0 in order
      -- to disable a given transition. For example they may do this
      -- with a functional rate.
      mapMaybe mkStateRep trans
      where
      mkStateRep :: (ParsedTrans, Derivative) -> Maybe Child
      mkStateRep (transition, target) =
        case Map.lookup target transMapping of
          Just tarD
            | (rateEnabled rRate) &&
              (isTransEnabled transition)  -> 
              -- We only return the child if the rate is enabled
              -- and the conditions on the transition are true.
              Just ( newTrans , RepSeq ((target, tarD), transMapping) )
            | otherwise                  ->
              -- So if the rate is not enabled or some condition does not
              -- hold true then this child is not enabled and we return nothing
              Nothing
          Nothing   -> error "This shouldn't ever happen"
        where
        newTrans = 
          Transition { pepaTransAction     = pepaTransAction     transition
                       -- The coalsced at this point of course should be empty
                     , pepaTransCoalsced   = pepaTransCoalsced   transition
                     , pepaTransConditions = pepaTransConditions transition
                     , pepaTransRate       = rRate
                     , pepaTransPriority   = pepaTransPriority   transition
                     }
        rRate    = reduceRate (Map.map fromIntegral concentrations) $ 
                   pepaTransRate transition

    -- Checks if the given rate is enabled, basically that it is not
    -- a timed rate with value zero.
    rateEnabled :: RateValue -> Bool
    rateEnabled (RateTimed d)     = d > 0
    rateEnabled (RateTop _)       = True
    rateEnabled (RateImmediate _) = True

    -- I'd like to combine the rateEnabled and isTransEnabled.
    -- I will do this once we have updated the transition data type
    -- to be paramaterised by rate 'kind'
    -- (ModelRate, RateValue, maybe even RateNumber).
    isTransEnabled :: Transition a -> Bool
    isTransEnabled transition
      | null conditions = True
      | otherwise       = conditionSatisfied concentrations singleCond
      where
      singleCond = foldr1 andRateConditions conditions
      conditions = pepaTransConditions transition



  -- Simple function to tell if a child is in a cooperation set
  -- this is used in both cooperation and arrays (since they can internally
  -- cooperate over some actions).
  isCooperating :: [ ActionIdentifier ] -> Child -> Bool
  isCooperating actions child =
     case pepaTransAction $ fst child of
       (Action a)    -> elem a actions
       (ComAction a) -> elem a actions
       (Tau _)       -> False

  -- Similarly 'performsAction' returns true if the given child
  -- performs the given action.
  -- We have (performsAction action child) == (isCooperating [ action ] child)
  performsAction :: ActionIdentifier -> Child -> Bool
  performsAction action child =
    case pepaTransAction $ fst child of
      (Action a)    -> a == action
      (ComAction a) -> a == action
      (Tau _)       -> False

{-
  Removing the immediate states from a list of states.
  To remove an immediate state we must redirect all states
  which enter that immediate state to the target(s) of 
  the edges out of the immediate state.

  What if the initial state is a vanishing state? I think then
  we have a problem. Indeed it could even be that the initial
  state is actually a set of states. I think it's relatively okay
  to just say that "oh well, in that case the modeller has made a bit
  of an error and can do something to avoid that."
-}
removeImmediateStates :: StateSpace -> MainControl StateSpace
removeImmediateStates space
  | Map.null vanishingStates =
    -- If the vanishing states are already null then we can just quickly
    -- return the current state space, this saves checking the entire
    -- state space.
    -- Note: we also give less log information because this state space
    -- should already be logged from the generation of the full states space
    MainControl.valueResult space logKey logInfoNoVanishing
  | otherwise                =
    MainControl.valueResult newSpace logKey logInformation
  where
  logKey                = "vanishing-states-removed"
  -- The logging information in the case that we need not alter the
  -- given state space because there are no vanishing states
  logInfoNoVanishing    = "No vanishing states, state space unchanged"
  -- The logging information in the case that we do create a new state space
  logInformation        = showStateSpace newSpace


  -- The new state space consists of the new set of timed States
  -- plus no vanishing states. 
  newSpace              = 
    StateSpace { spaceTangible  = snd timedStates
               , spaceTangIds   = fst timedStates
               , spaceVanishing = Map.empty
               -- We just copy the initial and all deriviatives
               -- field from the old state space (it is possible
               -- that some derivatives are never possible, eg if
               -- we had: P = a.Q)
               , spaceInitial   = spaceInitial   space
               , spaceAllDerivs = spaceAllDerivs space
               }
  -- The new set of tangible (non-vanishing states) are gotten by
  -- changing each current tangible state so that any transition
  -- from that state now "sees through" subsequent immediate transitions.
  -- Note that the set of states remain the same, that is we do not change
  -- the number of tangible states nor the state concentrations of any
  -- of those tangiable states. We are only updating the transitions.
  --
  -- Further note that we must update the 'spaceTangIds' map, since otherwise
  -- this will be a mapping from integers to the old states which
  -- contains the old movements.
  timedStates           = Map.mapAccum explore Map.empty $ spaceTangible space

  -- We will use the vanishing states to look up the targets of
  -- timed transitions from tangible states. If a transition has
  -- as a target a vanishing state then we must update the transition
  -- to target a timed state by "looking through" the immediate
  -- transition
  vanishingStates       = spaceVanishing space

  -- explore is perhaps the wrong word here; For each movement
  -- of a state if the move's target is an immediate state then
  -- we reduce that move such that it goes to the target(s) of
  -- the (immediate) moves out of the immediate state.
  -- Of course the target(s) may themselves be immediate states
  -- so we have to recurse until that isn't the case.
  explore :: (Map StateId State) -> State -> (Map StateId State, State)
  explore idsSoFar state =
    ( Map.insert (stateNumber newState) newState idsSoFar
    , newState
    )
    where
    newState = state { stateMovements = concat newMoves }
    newMoves = map reduceImmediateMoves $ stateMovements state

  -- This actually operates on non-immediate transitions it turns
  -- any transition which targets an immediate state into a set
  -- of transitions which target the targets of the immediate state.
  -- If the given move has a non-immediate state as its target then
  -- we simply return the move as it need not be reduced.
  reduceImmediateMoves :: StateMove -> [ StateMove ]
  reduceImmediateMoves (transition, targetRep)
    | Just state <- Map.lookup targetRep vanishingStates =
      -- If the moves goes to an immediate state then we must
      -- translate the move into a list of equivalent moves which
      -- bypass the immediate state. However those moves themselves
      -- may go to immediate states hence we must re-reduce.
      -- (can't seem to get where syntax to work here?)
      let reducedMoves = makeNewMoves $ stateMovements state
      in
      concatMap reduceImmediateMoves reducedMoves
    | otherwise                              =
      -- On the other hand if the move does not go to an immediate
      -- state then we're done we can just return the move as is.
      [ (transition, targetRep) ]
    where
    -- From a list of the the immediate moves we make a list of
    -- (possibly) non-immediate moves. How do we do this?
    -- Okay so what we are doing is; for each of the immediate moves
    -- there will be one copy of the move we are currently reducing
    -- (that is the argument to reduceImmediateMoves).
    -- The one copy has its rate scaled by the weight of the immediate
    -- move. So that if we have
    -- s1 --- r1 ---> s2     % timed move from s1 to s2 with rate r1
    -- s2 ---- imm(2) --> s3 % immediate move from s2 to s3 with weight 2
    -- s2 ---- imm(3) --> s4 % immediate move from s2 to s4 with weight 3
    -- then we want to reduce the two immediate moves to get:
    -- s1 --- (r1 * 2)/5 ---> s3
    -- s1 --- (r1 * 3)/5 ---> s4
    makeNewMoves :: [ StateMove ] -> [ StateMove ]
    makeNewMoves moves =
      map makeNewMove moves
      where
      -- All of the weights of the immediate actions
      allWeights      = map getWeight moves
      -- The sum of all the weights of the immediate action.
      sumWeights      = foldr1 (+) allWeights

      -- This translates the original non-immediate move using the
      -- given immediate move to calculate the weighting that the
      -- new non-immediate move should have. Also of course the target
      -- will now be the target of the immediate move (since we are
      -- wanting to skip over the immediate state).
      -- (ie the argument to reduceImmediateMoves)
      makeNewMove :: StateMove -> StateMove
      makeNewMove (immTransition, immTarget) =
        -- So we take the trans from the original move and scale
        -- it according to the weight of the immediate action
        -- the target of the move is obviously the target of the immediate
        -- action. Finally we must add in the coalasced immediate action.
        ( transition { pepaTransCoalsced = coalsced
                     , pepaTransRate     = newRate
                     }
        , immTarget
        )
        where
        newRate = scaleRate weighting $ pepaTransRate transition
        immRate = pepaTransRate immTransition
        -- The weighting of this particular copy of the original move
        -- is the weight of the immediate action divided by the sum
        -- of all the immediate weights.
        weighting  = (getRateWeight immRate) / sumWeights
        -- The coalsced actions include any previously coalsced actions
        -- plus of course this immediate action.
        -- Note that the immediate action should itself have no
        -- coalsced actions since we only ever modify (in this function)
        -- timed activities.
        coalsced   = (pepaTransAction immTransition) :
                     (pepaTransCoalsced transition)

    -- Returns the immediate weight of a rate, we just error if
    -- this is not an immediate rate, it of course must be since
    -- we're looking at the moves of a vanishing state.
    getRateWeight :: RateValue -> Double
    getRateWeight (RateImmediate d) = d
    getRateWeight _otherwise               = 
      error "timed rate where immediate weight expected"
    -- returns the weight of an immediate move
    getWeight :: StateMove -> Double
    getWeight = getRateWeight . pepaTransRate . fst


-- Potential gotcha here is if the initial state is actually
-- a vanishing state.
getInitialState :: StateSpace -> MainControl StateId
getInitialState space = 
  case Map.lookup (spaceInitial space) (spaceTangible space) of
    Just s  -> return $ stateNumber s
    Nothing -> fail "Initial state not found, could it be vanishing?"

getSourceStates :: [ String ] -> StateSpace -> Set StateId
getSourceStates = findTargetedStates

getTargetStates :: [ String ] -> StateSpace -> Set StateId
getTargetStates = findTargetedStates

findTargetedStates :: [ String ] -> StateSpace -> Set StateId
findTargetedStates actions space = 
   Map.fold addTargets Set.empty tangibleStates
   where
   tangibleStates = spaceTangible space
   -- From the given state and set of already found targets
   -- We take the list of moves from the given state and for
   -- any that are in the given action set we add their targets
   -- to the set of targets obtained so far.
   addTargets :: State -> Set StateId -> Set StateId
   addTargets state soFar =
      foldl addTarget soFar $ stateMovements state
      where
      -- so for each move if the action is in the given action
      -- set then add the target otherwise do nothing
      addTarget :: Set StateId -> StateMove -> Set StateId
      addTarget accum move
        | elem actionName actions = Set.insert targetId accum
        | otherwise               = accum
        where
        actionName = showOrig . nameOfAction . moveAction $ move
        targetId   = getStateId $ moveTarget move

   -- Unfortunately then we have to look up the target state's representation
   -- in the state map to find out the state's id.
   getStateId :: StateRepre -> StateId
   getStateId srep
     | Just s    <- Map.lookup srep tangibleStates = stateNumber s
     | otherwise                                   =
       error "findTargetedStates: Serious internal failure, state not found"


{-| Return the states that satisfy a given condition on the concentrations
    in a given state.

    I'm not a hundred percent happy with this method of translating an
    expression from the command-line/file into a value that makes sense here.
    The problem is we must evaluate the given expression in the current state.
    However the expression will refer to unqualified names but the state
    components in the concentrations wil refer to qualified names.
    We wish for "P1 == 0" to mean something like 
    "(P1_0 + P1_1 + P1_2) == 0".
    One way is to modify the expressions and the other way is to do it on the
    fly here. This seems a little more robust, however the previous way seems
    more efficient.

    UPDATE: A far better way would be for 'Language.Pepa.Transform.simplify'
    update the conditions that are passed in here. The conditions passed in
    here are generally from measurement specifications.
    Then we can just use 'conditionSatisfied'.
    It will basically be:
    findSatisfyingStates $ conditionSatisfied . stateConcentrations
-}
getSatisfyingStates :: StateCondition -> StateSpace -> Set StateId
getSatisfyingStates expression =
  findSatisfyingStates condition 
  where
  condition :: State -> Bool 
  condition state = 
    conditionSatisfied (stateConcentrations state) expression

{-|
  The same as 'conditionSatisfied' except that we do not assume that
  the given state condition has had the names qualified
  see 'getSatisfyingStates'
-}
outsideConditionSatisfied :: StateConcentrations -> StateCondition -> Bool
outsideConditionSatisfied concentrations conditionExp
  | value == 0.0 = False
  | otherwise    = True
  where
  value = reduceRateExprWith componentLookup conditionExp
  
  -- So the state condition will be either from a source and target state
  -- condition given on the command-line or calculated from the probes
  -- or will be part of a condition on a transition. These result either
  -- from conditional behaviour or from state-aware probe definitions
  -- such as: {P == 0}a:start, b:stop.
  -- In any case the condition expression contains references to unqualified
  -- component names hence we must look them up. Additionally we must change
  -- the expression "P" into "P_1 + P_2 + P_3" or whatever are the mangled
  -- versions of P (these result from P occurring more than once in the
  -- system equation).
  componentLookup :: RateIdentifier -> RateNumber
  componentLookup rident =
    fromIntegral sumValue
    where
    sumValue = foldl addRIdent 0 $ Map.toList concentrations
    addRIdent :: Int -> (Derivative, Int) -> Int
    addRIdent soFar (thisIdent, i)
      | Qualified.sameOrigName rident thisIdent = soFar + i
      | otherwise                               = soFar


{-|
   Returns true if, in the given state concentrations, the state condition
   (an expression) holds true.
-}
conditionSatisfied :: StateConcentrations -> StateCondition -> Bool
conditionSatisfied concs conditionExp
  | value == 0 = False
  | otherwise = True
  where
  value :: Int
  value   = reduceRateExpr compMap conditionExp
  -- The state concentrations map components to integers but 'reduceRateExpr'
  -- expects to see a mapping from names to rate values.
  -- This should sort itself out if we separate rate expressions into
  -- boolean and rate expressions. 
  compMap = Map.map fromIntegral concs

{-| Return the state ids of any state which satisfies the given condition -}
findSatisfyingStates :: (State -> Bool) -> StateSpace -> Set StateId
findSatisfyingStates condition space =
  Map.fold addStates Set.empty tangibleStates
  where
  tangibleStates = spaceTangible space
  addStates :: State -> Set StateId -> Set StateId
  addStates s soFar
    | condition s = Set.insert (stateNumber s) soFar
    | otherwise   = soFar


