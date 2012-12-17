{-|
   A module of  combinators for building
   non-deterministic finite state machines.
   Note these combinators are for nfas in relation to stochastic
   probes. These are therefore ultimately cyclic machines though
   at the stage these combinators are used that is not the case.

   If you are looking for a generic finite automata library, this
   is probably not it, though may be easier to amend than others. 
-}
module Language.Pepa.Probes.StateMachines.NfaCombinators
   ( IntNfa

   , nfaOr
   , nfaIf
   , nfaThen
   , nfaStar
   , nfaQuestion
   , nfaReset
   )
where

{- Standard library module imports -}
import Prelude hiding 
   ( any )
import Data.Foldable
   ( any )
import Data.Maybe
   ( mapMaybe )
import qualified Data.Set as Set
import Data.Set
   ( Set )
{- Local module imports -}
import Language.Pepa.Probes.StateMachines.NfaTypes
   ( Nfa       ( .. )
   , NfaState  ( .. )
   , NfaLabel  ( .. )

   , Move      ( .. )

   , isMoveFrom
   , isEpsilonMove
   , moveTarget
   )
{- End of module imports -}

{-|
   The type of a non-deterministic finite state automata with integers
   as their states.
-}
type IntNfa = Nfa Int
-------------------------------------------------------------------------- 
--									--
--	Combinators for machines use to build them			--
--									--
-------------------------------------------------------------------------- 

{- Of course this *should* have type:
@
nfaOr :: (NfaState state, NfaLabel label) => 
         Nfa state label        -- The left nfa
      -> Nfa state label        -- The right nfa
      -> Nfa state label
@
-}
{-|
   Creates an nfa which can perform as the first input nfa
   or the second nfa.
-}
nfaOr :: (NfaLabel a) => IntNfa a -> IntNfa a -> IntNfa a
nfaOr (NFA states1 moves1 _start1 _finish1)
      (NFA states2 moves2 _start2 _finish2) =
   NFA allStates allMoves newStart $ Set.singleton ( m1 + m2 + 1 )
   where
   allStates = Set.union states1' $ Set.union states2' newStates
   allMoves  = Set.union moves1' $ Set.union moves2' newMoves

   newStart  = minimal

   m1        = Set.size states1
   m2        = Set.size states2
   states1'  = Set.map (renumber 1) states1
   states2'  = Set.map (renumber (m1+1)) states2
   newStates = Set.fromList [0,(m1+m2+1)]
   moves1'   = Set.map (renumberMove 1) moves1
   moves2'   = Set.map (renumberMove (m1+1)) moves2
   newMoves  = Set.fromList [ Emove 0 1 
                            , Emove 0 (m1+1)
                            , Emove m1 (m1+m2+1)
                            , Emove (m1+m2) (m1+m2+1)
                            ]

{-| Creates an nfa which performs as the first input but
    all of the first moves of the nfa are altered by the
    given label changing function which will generally
    be adding a guard.
-}
nfaIf :: (NfaLabel a) => (a -> a) -> IntNfa a -> IntNfa a
nfaIf guardFunction (NFA states moves start finish) =
  NFA states newMoves start finish
  where
  -- We modify all the moves which have as their origin
  -- the starting state OR are reachable from the starting
  -- state via only epsilon moves.
  newMoves = Set.map guardAppropriate moves
  guardAppropriate move@(Move s1 a s2)
    | Set.member move apppropriate  = Move s1 (guardFunction a) s2
    | otherwise                     = move
  guardAppropriate move@(Emove _ _) = move

  apppropriate = getAppropriate Set.empty start

  -- Returns the list of moves which are possible from the current
  -- state, including those which are transitively possible via
  -- any number of epsilong moves.
  --  getAppropriate :: Set State -> State -> Set (Move Int a)
  getAppropriate seen state
    | Set.member state seen = error "detected epsilon loop in nfaIf"
    | otherwise             = Set.union approp transitive
    where
    -- The moves which are from the current state that we are looking
    -- at. This means the move must be possible from the starting state
    -- of the nfa.
    approp         = Set.filter (isMoveFrom state) moves
    -- All the moves from the current state which are epsilon moves
    appropEpsilon  = Set.filter isEpsilonMove approp
    -- The targets of the epsilon moves from the current state, this means
    -- that moves whose source is one of these targets are also possible
    -- from the current state.
    epsilonTargets = Set.toList $ Set.map moveTarget appropEpsilon
    -- The moves which are transitively available from the current
    -- state because their sources are reachable through some number
    -- of epsilon moves.
    transitive     = Set.unions $ map (getAppropriate newSeen) epsilonTargets
    newSeen        = Set.insert state seen


     

{- I think there is a bug in this, shouldn't it be able to go from
   *any* of the accepting states of finish1 to start2 ?

   Okay when I've got time I can try the following:
   @ k = size states1 @
   @newMoves = Set.map (\x -> Emove x (start2 $ renumber k)) finish1@
   Oh actually we definitely do *not* want to do it like this, at least
   not for our finite state machines, because if we are using 'accepting'
   to mean immediate communication states then we definitely don't want
   to arbitrarily jump out of those states.
-}
{-|
   Performs as the first input nfa and *then* as the second.
-}
nfaThen :: (NfaLabel a) => IntNfa a -> IntNfa a -> IntNfa a
nfaThen (NFA states1 moves1 start1 _finish1) 
        (NFA states2 moves2 _start2 finish2) =
   NFA
   (Set.union states1 states2')
   (Set.union moves1 moves2')
   start1
   finish2'
   where
   states2' = Set.map (renumber k) states2
   moves2'  = Set.map (renumberMove k) moves2
   finish2' = Set.map (renumber k) finish2
   k = (Set.size states1) - 1


{-|
   Performs as the input nfa any number of times including zero times.
   Most useful if the input nfa is not a single activity since in that
   case it is the same as no nfa at all since the self-loops will add
   the ability to do any number of that activity in any state.
-}
nfaStar :: (NfaLabel a) => IntNfa a -> IntNfa a
nfaStar (NFA states moves _start _finish) =
   NFA
   (Set.union states' newstates)
   (Set.union moves' newmoves)
   0
   (Set.singleton (m+1))
   where
   m         = Set.size states
   states'   = Set.map (renumber 1) states
   newstates = Set.fromList [ 0 , m+1 ]
   moves'    = Set.map (renumberMove 1) moves
   newmoves  = Set.fromList [ Emove 0 1
                            , Emove m 1
                            , Emove 0 (m+1)
                            , Emove m (m+1)
                            ]

{-| 
  A bit similar to 'nfaStar' except that it will do the machine
  once or zero times.
-}
nfaQuestion :: (NfaLabel a) => IntNfa a -> IntNfa a
nfaQuestion (NFA states1 moves1 start1 _finish1) =
   NFA (Set.insert newEnd states1)
       (Set.union moves1 newMoves)
       start1
       (Set.singleton newEnd)
   where
   k        = Set.size states1 - 1
   newEnd   = k + 1
   newMoves = Set.fromList [ Emove start1 newEnd, Emove k newEnd ]

{-|
   Takes an nfa and a label and resets the nfa to the start state
   at every state in which the given label cannot be performed.
-}
nfaReset :: (NfaLabel l) => l -> IntNfa l -> IntNfa l
nfaReset action (NFA states moves start finish) =
   (NFA states newMoveSet start finish )
   -- For every move if we cannot currently perform
   -- 'action' then move from 'action' to the start state.
   where
   newMoveSet = Set.union moves newMoves
   newMoves   = mapMaybeSet resetAction states

   -- resetAction :: Int -> Maybe (Move Int l)
   resetAction i
      -- if we have finished we cannot reset
      | Set.member i finish           = Nothing
      | any iPerformsAction moves     = Nothing
      | otherwise                     = Just $ Move i action 0 -- start
      where
      -- iPerformsAction :: Move Int l -> Bool
      iPerformsAction (Move j lab _) = (j == i) && (lab == action)
      iPerformsAction _              = False

-------------------------------------------------------------------------- 
--      Auxilliary functions used in the building cominbators above     --
--      regular expressions.                                            --
-------------------------------------------------------------------------- 
renumberMove :: (NfaState a, NfaLabel b) => Int -> Move a b -> Move a b
renumberMove k (Move s1 c s2) =
   Move (renumber k s1) c (renumber k s2)
renumberMove k (Emove s1 s2)  =
   Emove (renumber k s1) (renumber k s2)

-- A temporary mapMaybe for sets
mapMaybeSet :: (Ord a, Ord b) => (a -> Maybe b) -> Set a -> Set b
mapMaybeSet f = Set.fromList . (mapMaybe f) . Set.toList