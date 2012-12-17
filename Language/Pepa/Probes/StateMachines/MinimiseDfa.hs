{-|
   A module for minimising a deterministic finite state machine.
   Note that the main function it provides 'minimiseDfa' operates
   on a slightly different algorithm to the normal minimisation
   function for dfas. This is because our dfa is always cyclic
   and we never wish for it to \"accept\" any input but merely that
   it continues in the correct \"states\".
-}
module Language.Pepa.Probes.StateMachines.MinimiseDfa 
   ( minimiseDfa )
where

{- Standard Library Modules Imported -}
import Prelude hiding
   ( map )
import qualified Data.Function as Function
import Data.List
   ( find )
import Data.Set
   ( toList
   , map
   , delete
   )
{- Local Modules Imported -}
import Language.Pepa.Probes.StateMachines.NfaTypes
   ( Nfa       ( .. )
   , Move      ( .. )
   , NfaState  ( .. )
   , NfaLabel  ( .. )
   )
import qualified Language.Pepa.Utils as Utils
{- End of module imports -}

-------------------------------------------------------------------------
{-|
   Minimising the dfa is not quite the same as a traditional dfa
   minimiser. Basically we group the moves into sets corresponding
   to their source. So each move-group is essentially a state within
   the dfa. Then two states are considered equal they have equal
   move sets. That is, two states s1 and s2 are considered equal if
   every action which can be taken by s1 can be taken by s2 and results
   in the same state and vice versa. For the purposes of the comparison any
   move that results in state s1 or s2 is considered the same (provided
   it has the same label obviously). So for example if s1 loops on action
   /a/ that is equivalent to s2 looping on /a/ or performing /a/ and
   going to state s1. In fact if s1 performs /a/ and goes to s2, and s2
   performs /a/ and goes to s1 then the two moves are considered equal.

   If two states are considered equal (because they have equal move sets)
   we delete one of the states, remove all moves which originate from that
   state, and change all moves which target that state to target the
   equivalent state. We then recursively minimise the dfa.
-}                                  
-------------------------------------------------------------------------

minimiseDfa :: (NfaState s, NfaLabel l) => Nfa s l -> Nfa s l
minimiseDfa machine@(NFA _states moves _start _accepting) =
   case findTwoEqual moveSetsEqual moveSets of
      Nothing                        -> machine
      Just ([], _)                   -> machine  -- I don't think either 
      Just (_, [])                   -> machine  -- should happen
      Just ((Move s _ _ : _), right) -> minimiseDfa $ reviseDfa machine s right
      Just ((Emove s  _ : _), right) -> minimiseDfa $ reviseDfa machine s right
   where
   -- Group the moves according to their source. All the moves in
   -- one group originate at the same state.
   moveSets = Utils.fullGroupBy sameSource $ toList moves

   -- Returns true if two moves have the same source.
   sameSource :: (NfaState s, NfaLabel l) => Move s l -> Move s l -> Bool
   sameSource = Function.on (==) sourceOfMove

   -- Check if two move sets are equal, this means that the two
   -- associated states are equal and one may be removed.
   moveSetsEqual :: (NfaState s, NfaLabel l) => 
                    [ Move s l ] -> [ Move s l ] -> Bool
   moveSetsEqual [] []      = True
   moveSetsEqual [] _       = False
   moveSetsEqual _ []       = False
   moveSetsEqual left right =
      allLeft && allRight
      where
      -- everything in the left is in the right?
      allLeft  = all (\x -> any (equivMove x) right) left
      -- everything in the right is in the left? 
      allRight = all (\x -> any (equivMove x) left) right

      leftSource  = sourceOfMove $ head left
      rightSource = sourceOfMove $ head right

      -- Moves are equivalent if labels are the same and
      -- either both targets are the same or both targets
      -- are either of the two states under consideration because
      -- in that case they are both (possibly) equivalent to a self-loop.
      -- Grrr, I cannot seem to write the type signature for this.
      -- equivMove :: (NfaState s, NfaLabel s) => Move s l -> Move s l -> Bool
      -- equivMove :: Move s l -> Move s l -> Bool
      equivMove (Move _ a1 tar1) (Move _ a2 tar2) =   
         (a1 == a2) && -- The labels are the same
         ( (tar1 == tar2) || -- the targets are the same
           ( (tar1 == leftSource || tar1 == rightSource) &&
             (tar2 == leftSource || tar2 == rightSource)
           )
         )
      equivMove (Emove _ tar1) (Emove _ tar2)     =
         ( (tar1 == tar2) || -- the targets are the same
           ( (tar1 == leftSource || tar1 == rightSource) &&
             (tar2 == leftSource || tar2 == rightSource)
           )
         )
      equivMove _ _                               = False


sourceOfMove :: (NfaState s, NfaLabel l) => Move s l -> s
sourceOfMove (Move s _ _) = s
sourceOfMove (Emove s _ ) = s

{- 
   This function takes two sets of moves which are deemed to be equal
   and removes one of the states from the machine.
-}
reviseDfa :: (NfaState s, NfaLabel l) => 
             Nfa s l -> s -> [ Move s l ] -> Nfa s l
reviseDfa (NFA states moves start accepting) equivalentState right =
   NFA newStates newMoves newStart $ delete deletedState accepting
   where
   newStates = delete deletedState states
   newStart  = if start == deletedState then equivalentState else start
   -- get the state to delete
   deletedState  = sourceOfMove $ head right

   -- delete all the moves
   retainedMoves = foldr delete moves right
   -- For every move remaining if the target is the deleted State
   -- then change the target to be the equivalent state
   newMoves      = map changeTarget retainedMoves

   -- Once again, how do I write this down?
   -- changeTarget :: Move state label -> Move state label
   changeTarget move@(Move s a t)
      | t == deletedState = Move s a equivalentState
      | otherwise         = move
   changeTarget move@(Emove s t)
      | t == deletedState = Emove s equivalentState
      | otherwise         = move
   

findTwoEqual :: (a -> a -> Bool) -> [a] -> Maybe (a, a)
findTwoEqual _ []      = Nothing
findTwoEqual _ [ _ ]   = Nothing
findTwoEqual f (h : t) =
   case find (f h) t of
      Nothing -> findTwoEqual f t
      Just k  -> Just (h, k)

---- Temporary groupBy for sets -----------------------------------
-- fullGroupBySet :: (Ord a) => (a -> a -> Bool) -> Set a -> Set (Set a)
-- fullGroupBySet cond = (map fromList) . fromList . (fullGroupBy cond) . toList 
