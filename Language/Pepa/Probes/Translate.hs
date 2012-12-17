{-| 
    This module defines the translation from probe specifications to
    pepa component definitions
-}
module Language.Pepa.Probes.Translate
   ( translateProbeR
   , probeToDotFile
   , ProbeTranslateFlags     ( .. )
   , comActionPriority
   )
where

{- Imported Standard Libraries -}
import Prelude hiding
   ( any )
import Data.Foldable
   ( any )
import Data.List
   ( find )
import qualified Data.Maybe as Maybe
import Data.Maybe
  ( mapMaybe )
import Data.Set
   ( fromList
   , toList
   , union
   , singleton
   )
{- Imported External Libraries -}
{- Imported Local Libraries -}
import Language.Pepa.QualifiedName
  ( QualifiedName      ( .. ) )
import Language.Pepa.Rates
  ( Rate               ( .. )
  , RateExpr           ( .. )
  , defaultWeight
  )
import Language.Pepa.Syntax
   ( ActionIdentifier
   
   , ProcessDef
   , Transition         ( .. )
   , ParsedTrans
   , ParsedPriority
   , defaultPepaPriority
   , ParsedComponent    ( .. )
   , ParsedComponentId
   , ParsedAction       ( .. )
   
   , involvesAction
   )
import Language.Pepa.Utils
   ( mkCSlist
   , fullGroupBy
   )
import Language.Hydra.Print
  ( printCexp )
import Language.Pepa.Probes.Syntax
   ( ProbeR      ( .. )
   , probeAlphabet
   )
import Language.Pepa.Probes.StateMachines.NfaTypes
   ( Move        ( .. )
   , Nfa         ( .. )
   , NfaLabel    ( .. )

   , isMoveFrom
   )
import Language.Pepa.Probes.StateMachines.NfaCombinators
   ( nfaOr
   , nfaIf
   , nfaThen
   , nfaStar
   , nfaQuestion
   , nfaReset
   )
import Language.Pepa.Probes.StateMachines.NfaLib
   ( movesFromState )
import Language.Pepa.Probes.StateMachines.NfaToDfa
   ( makeDeterministic )
import Language.Pepa.Probes.StateMachines.MinimiseDfa
   ( minimiseDfa )
{- End of Imports -}

{-|
   The type of flags which modify how the translation is done.
-}
data ProbeTranslateFlags = 
   -- | Do not minimise the dfa
   NoMinimise
   -- | Add the self-loops before minimisation
 | MinSelfLoops
   -- | Minimise before adding the self-loops
 | NoMinSelfLoops
   -- | Do not add any self-loops
 | NoSelfLoops
   -- | Leave the state machine as non-deterministic 
 | NonDeterm  
   deriving Eq


{-| The main exported function translates a probe into a set
    of pepa definitions.
-}
translateProbeR ::
      [ ProbeTranslateFlags ] -- ^ The flags which control the translation
   -> String                  -- ^ The name of the probe
   -> [ ParsedTrans ]         -- ^ The transitions used to get the right
                              --   action kinds (immediate or top)
   -> ProbeR                  -- ^ The probe to be translated
   -> [ ProcessDef ]          -- ^ The returned pepa process definitions
translateProbeR trFlags probeName transitions =
   dfaToPepa transitions probeName .
   probeToMinimisedDfa trFlags 

{-|
   This translates a given probe into a dot file format.
-}
probeToDotFile ::  [ ProbeTranslateFlags ] -> String -> ProbeR -> String
probeToDotFile trFlags probeName probe =
   unlines $ [ "digraph " ++ probeName ++" {" ]
             ++ stateLines ++ moveLines ++ [ "}" ]
   where
   NFA states moves _start _accepts = probeToMinimisedDfa trFlags probe
   stateLines = map toStateLine $ toList states
   moveLines  = map (toArrowLine . showTransGroup) moveGroups

   -- Groups the transitions so that those with the same source
   -- source and target states are depicted on the same line.
   moveGroups :: [ [ ProbeMove ] ]
   moveGroups = fullGroupBy sameSourceAndTarget $ toList moves

   toStateLine :: ProbeState -> String
   toStateLine i = concat [ "\""
                          , show i
                          , "\""
                          , " [ shape = circle , color = red ] ;"
                          ]

   toArrowLine :: (String, String, String) -> String
   toArrowLine (s1, l, s2) = concat [ "\""
                                    , s1
                                    , "\" -> \""
                                    , s2
                                    , "\" [ label = \""
                                    , l
                                    , "\" ] ; "
                                    ]

   sameSourceAndTarget :: ProbeMove -> ProbeMove -> Bool
   sameSourceAndTarget s1 s2 = (sourceOf s1 == sourceOf s2)
                               && (targetOf s1 == targetOf s2)
   sourceOf :: ProbeMove -> ProbeState
   sourceOf (Move s _ _) = s
   sourceOf (Emove s _ ) = s

   targetOf :: ProbeMove -> ProbeState
   targetOf (Move _ _ s) = s
   targetOf (Emove _  s) = s

   labelOf :: ProbeMove -> String
   labelOf (Move _ l _) = hprintProbeMoveLabel l
   labelOf (Emove _ _ ) = "Epsilon"

   -- Turns a group of transitions into a single line for the dot
   -- file so that we have a single arrow with a comma separated list
   -- of labels rather than multiple arrows with the same source
   -- and target states.
   showTransGroup :: [ ProbeMove ] -> (String, String, String)
   showTransGroup []          = ("", "error: empty group", "")
   showTransGroup (s1 : rest) =
      ( show $ sourceOf s1
      , mkCSlist $ map labelOf (s1 : rest)
      , show $ targetOf s1
      ) 
       

{-
   This function does the common work of 'translateProbeR'
   and 'probeToDotFile'. This does mean that if a program wishes
   to do *both* to a dot file and adding the probes to a model then
   it will re-do all of this work. To avoid this would require a little
   re-factoring, but not *that* much. I just don't see it as all that
   likely that someone is going to want a dot-file of a massively complicated
   probe hence even in the case that we do the work twice it won't matter
   that much.
-}
probeToMinimisedDfa :: [ ProbeTranslateFlags ] -> ProbeR -> ProbeNfa
probeToMinimisedDfa trFlags probe =
   localAddSelfMinimise $ localMakeDeterministic cyclicNfa
   where
   -- If we specify no self loops then adding them is the id
   localAddSelfLoops
      | elem NoSelfLoops trFlags = id
      | otherwise                = addSelfLoops actionList
   -- if we specify no minimise then the minimise function is the id
   localMinimise
      | elem NoMinimise trFlags  = id
      | otherwise                = minimiseDfa
   -- The order of minimise and add-self-loops depends on whether we
   -- should minimise the self-loops or not.
   localAddSelfMinimise
      | elem MinSelfLoops trFlags   = localMinimise . localAddSelfLoops
      | elem NoMinSelfLoops trFlags = localAddSelfLoops . localMinimise
      | otherwise                   = localAddSelfLoops . localMinimise

   localMakeDeterministic
      | elem NonDeterm trFlags    = id
      | otherwise                 = makeDeterministic
   cyclicNfa   =  makeCyclic $ buildNfa probe
   actionList  = toList $ probeAlphabet probe

{- Define NFAs -}

{- Our NFAs are NFAs where the states are integers and the 
   labels on the transitions are 'ActionIdentifier's
   
   Okay states *should* be this, but for now we just let them
   be integers and solve the problem of adding self loops 
-}
-- data ProbeState = Communication Int
--                 | Ordinary Int
type ProbeState     = Int
type LabelGuard     = RateExpr
data ProbeMoveLabel = ProbeMoveLabel (Maybe LabelGuard) ActionIdentifier
                      deriving (Eq, Ord)
type ProbeMove      = Move ProbeState ProbeMoveLabel
type ProbeNfa       = Nfa  ProbeState ProbeMoveLabel

unguardedLabel :: ActionIdentifier -> ProbeMoveLabel
unguardedLabel = ProbeMoveLabel Nothing

guardWith :: LabelGuard -> ProbeMoveLabel -> ProbeMoveLabel
guardWith guard (ProbeMoveLabel Nothing a)  = ProbeMoveLabel (Just guard) a
guardWith guard (ProbeMoveLabel (Just g) a) =
  ProbeMoveLabel (Just $ Cand guard g) a

guardOfMove :: ProbeMove -> Maybe LabelGuard
guardOfMove (Move _state1 (ProbeMoveLabel mGuard _action) _state2) = mGuard
guardOfMove (Emove _ _)                                            = Nothing

isUnguarded :: ProbeMove -> Bool
isUnguarded = (Nothing ==) . guardOfMove

instance NfaLabel ProbeMoveLabel where
  stringOfLabel = hprintProbeMoveLabel


hprintProbeMoveLabel :: ProbeMoveLabel -> String
hprintProbeMoveLabel (ProbeMoveLabel Nothing a)  = stringOfLabel a
hprintProbeMoveLabel (ProbeMoveLabel (Just p) a) = 
  concat [ stringOfLabel a
         , "{"
         , printCexp p
         , "}"
         ]

{- Define DFAs -}

{- Translate Probes to NFAs -}

{- Because we want our pepa process to be cyclic, we must add an episilon
   transition from all the terminating (or more rather accepting) states
   to the start state.

   Here I also make the accepting state the start state, since this means
   that it will  'accept' the empty string, this avoids two similar states
   in the end dfa produced.
-}
makeCyclic :: ProbeNfa -> ProbeNfa
makeCyclic (NFA states moves startState accepting) =
   (NFA states newMoves startState $ singleton startState)
   where
   newMoves     = union moves (fromList epsilonMoves)
   
   epsilonMoves = map makeEpsilonMove $ 
                  filter requiresEpsilon (toList accepting)

   makeEpsilonMove :: Int -> ProbeMove
   makeEpsilonMove i = Emove i startState

   requiresEpsilon :: Int -> Bool
   requiresEpsilon i = not $ any isCurrentEpsilon moves
      where
      isCurrentEpsilon :: ProbeMove -> Bool
      isCurrentEpsilon (Move _ _ _) = False
      isCurrentEpsilon (Emove l m)  =
         l == i && m == startState

buildNfa :: ProbeR -> ProbeNfa
buildNfa (SPact a)        = 
   NFA ( fromList [ state0, state1 ] )
       ( singleton $ Move state0 (unguardedLabel a) state1 )
       state0
       ( singleton state1 )
   where
   state0 = 0 -- Ordinary 0
   state1 = 1 -- Ordinary 1
buildNfa (SPlabel a)      = 
   NFA ( fromList [ state0, state1 ] )
       ( singleton $ Move state0 (unguardedLabel a) state1 )
       state0
       ( singleton state1 )
   where
   state0 = 0 -- Communication 0
   state1 = 1 -- Ordinary 1
buildNfa (SPseq r1 r2)    = nfaThen (buildNfa r1) (buildNfa r2)
buildNfa (SPchoice r1 r2) = nfaOr (buildNfa r1) (buildNfa r2)
buildNfa (SPzom r)        = nfaStar $ buildNfa r
buildNfa (SPoom r)        = buildNfa (SPseq r $ SPzom r)
buildNfa (SPiter r n)     = iterNdfa n $ buildNfa r
buildNfa (SPzoo r)        = nfaQuestion $ buildNfa r
buildNfa (SPwo r a )      = nfaReset (unguardedLabel a) $ buildNfa r

buildNfa (SPbit r (m, n))
   | m > n     = buildNfa $ SPbit r (n, m)
   | m == n    = buildNfa $ SPiter r n
   | otherwise = nfaThen (iterNdfa m rNdfa) $ buildNfa rQnmTimes
   where
   rNdfa     = buildNfa r
   rQnmTimes = SPiter rQ (n - m) -- iterNdfa (m - n) $ nfaQuestion rNdfa
   rQ        = SPzoo r
buildNfa (SPcond c r)             =
  nfaIf (guardWith c) $ buildNfa r

iterNdfa :: Int -> ProbeNfa -> ProbeNfa
iterNdfa n r
   | n < 1     = error "an iter probe with n < 1"
   | n == 1    = r
   | otherwise = nfaThen r $ iterNdfa (n - 1) r

{- Add self-loops to DFAs -}


addSelfLoops :: [ ActionIdentifier ] -> ProbeNfa -> ProbeNfa
addSelfLoops alphabet machine@(NFA states moves initial accepts) =
   (NFA states newMoveSet initial accepts)
   where
   newMoveSet = union moves $ fromList newMoves
   newMoves   = concatMap selfLoopsOneState $ toList states

   -- Recall that we should only add the self-loops if the state
   -- does not represent one in which immediate communication can
   -- take place from. Since the immediate communication must take
   -- precedence over observing the model.
   selfLoopsOneState :: Int -> [ ProbeMove ]
   selfLoopsOneState i
      | shouldNotAddSelfLoops = []
      | otherwise             = mapMaybe makeSelfLoop alphabet
      where
      -- makeSelfLoop returns Nothing if we do not need to add
      -- the self loop for the given action identifier.
      -- This occurs if there is already an unguarded action from
      -- the given state ('i'). If there are only guarded actions
      -- then we produce a self-loop with a guard which is
      -- not (c) where c is the 'and' sum of all the other guards.
      -- Finally if there is no action whatsoever from the given
      -- state with the given action then we just add an unguarded
      -- self-loop.
      makeSelfLoop :: ActionIdentifier -> Maybe ProbeMove
      makeSelfLoop a 
         | null relevantMoves            = Just unguardedSelfLoop
         | any isUnguarded relevantMoves = Nothing
         | otherwise                     = Just guardedSelfLoop
         where
         -- All the 'a' actions from state 'i'
         relevantMoves     = filter fromIwithA allStateMoves
         -- All the moves from the state 'i'
         allStateMoves     = toList $ movesFromState machine i
         unguardedSelfLoop = Move i (unguardedLabel a) i
         guardedSelfLoop   = Move i (guardWith notOrGuard $ unguardedLabel a) i

         relevantGuards    = mapMaybe guardOfMove relevantMoves
         orGuard           = foldr1 Cor relevantGuards
         notOrGuard        = Cnot orGuard

         -- Determines if a move goes from the current state via
         -- the given action
         fromIwithA :: ProbeMove -> Bool
         fromIwithA (Move _ (ProbeMoveLabel _guard b) _) = b == a
         fromIwithA (Emove _ _)                          = False 

      -- Determines if (all/any which should it be??) 
      -- the moves from one state are an
      -- immediate communication, in which case we shouldn't add
      -- any self-loops
      shouldNotAddSelfLoops :: Bool
      shouldNotAddSelfLoops = isImmediateState alphabet machine i

         
{-
   This determines whether or not the state is one in which immediate
   communication can/should take place. This is useful for two reasons,
   first of all we wish not to add self-loops in such a state and second
   of all we wish to make such a state an accepting state for the purposes
   of minimisation.
-}
isImmediateState :: [ ActionIdentifier ] -> ProbeNfa -> ProbeState -> Bool
isImmediateState alphabet machine state =
   any isImmediateCommunication movesFromCurrentState
   where
   movesFromCurrentState = movesFromState machine state

   -- Determines if a move is an immediate communication, this
   -- is not well done I must admit, we just use the fact that
   -- the immediate communication labels should *not* be in
   -- the alphabet of the probe.
   isImmediateCommunication :: ProbeMove -> Bool
   isImmediateCommunication (Emove _ _    )                          = False
   isImmediateCommunication (Move _ (ProbeMoveLabel _mGuard name) _) =
      not $ elem name alphabet

   
{- Translate DFAs to Pepa defs -}
{- Note here the list of parsed transitions, this should include
   all the transitions in the model plus all the labels of the
   probe mapped to immediate communication actions. This way we know
   how to add the action names of the Dfa to the probe definitions
   as transitions.
-}
dfaToPepa :: [ ParsedTrans ] -- The transitions in the model
          -> String          -- The name of the probe
          -> ProbeNfa        -- The Dfa representation of the probe
          -> [ ProcessDef ]
dfaToPepa transitions probeName (NFA states moves startState _) =
   map defOfState $ toList states
   where
   moveList = toList moves
   defOfState :: Int -> ProcessDef
   defOfState i = ( makeProcessName i
                  , foldr1 ComponentSum prefixes
                  )
                  where
                  prefixes = map prefixOfMove iMoves
                  iMoves   = filter (isMoveFrom i) moveList
   
   prefixOfMove :: ProbeMove -> ParsedComponent
   prefixOfMove (Move _ (ProbeMoveLabel Nothing a) j)  =
      PrefixComponent (transOfAction a)
                      (IdProcess $ makeProcessName j)
   prefixOfMove (Move _ (ProbeMoveLabel (Just g) a) j) =
      PrefixComponent condTrans (IdProcess $ makeProcessName j)
      where
      simpleTrans = transOfAction a
      condTrans   = simpleTrans { pepaTransConditions = [ g ] }
      {-
      beCond = BeDnamExp g
      prefix = PrefixComponent (transOfAction a)
                               (IdProcess $ makeProcessName j) -}
   prefixOfMove (Emove _ _)                            =
      error "Dfa contains an epsilon move"

   -- If it is the start state then it should have the same name
   -- as the probe.
   makeProcessName :: Int -> ParsedComponentId
   makeProcessName i
      | i == startState = Unqualified probeName
      | otherwise       = Unqualified $ probeName ++ (show i)
   {- This looks up how to add the transition as an immediate
      action or as a timed (with infty) activity.
   -}
   transOfAction :: ActionIdentifier -> ParsedTrans
   transOfAction actionId =
     Maybe.fromMaybe defaultTrans $
       find (involvesAction actionId) transitions
     where
     -- If the action isn't in the list of transitions for
     -- the model then it must be a communication activity
     defaultTrans = Transition { pepaTransAction     = ComAction actionId
                               , pepaTransCoalsced   = []
                               , pepaTransPriority   = comActionPriority
                               , pepaTransRate       = weight
                               , pepaTransConditions = []
                               }
     weight       = RateImmediate defaultWeight

comActionPriority :: ParsedPriority
comActionPriority = defaultPepaPriority
